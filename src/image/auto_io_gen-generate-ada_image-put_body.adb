--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2001 - 2004, 2006 - 2007 Stephen Leake.  All Rights Reserved.
--  Copyright (C) 2020  Oliver Kellogg  <okellogg@users.sf.net>
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Asis.Aux;
with Asis.Elements;
with Auto_Io_Gen.Options;
with SAL.Gen.Alg.Process_All_Constant;

package body Auto_Io_Gen.Generate.Ada_Image.Put_Body is
   use Ada.Text_IO;

   Body_First : Boolean := True; --  Shared between printing discriminants and components.

   procedure End_Function_Image (File : in Ada.Text_IO.File_Type)
   is begin
      Indent_Line (File, "return To_String (Buffer);");
      Indent_Decr (File, "end Image;");
      New_Line (File);
   end End_Function_Image;

   procedure Declare_Function_Image
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type;
      Trivial         : in Boolean := True)
   is begin
      Indent_Incr (File, "function Image (Item : " &
                         Lists.Type_Name (Type_Descriptor) & ") return String");
      Indent_Less (File, "is");
      Indent_Line (File, "Buffer : Unbounded_String;");
      Indent_Less (File, "begin");
      if Trivial then
         Indent_Line (File, "Put (Buffer, Item);");
         End_Function_Image (File);
      end if;
   end Declare_Function_Image;

   procedure Generate_Component_Line
     (File      : in Ada.Text_IO.File_Type;
      Component : in Auto_Io_Gen.Lists.Component_Type;
      First     : in Boolean);
   --  Generate body code to put one component.

   procedure Generate_Derived_Array
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate body code for all Put subprograms for a derived array type.

   procedure Generate_Private_Array_Wrapper
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate body code for all Put subprograms in the public child
   --  for a private array type.

   procedure Generate_Record
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Record_Type_Descriptor_Type);
   --  Generate body code for all Put subprograms for a record type.

   procedure Generate
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is begin
      if Options.Debug then
         Put_Line ("Generate.Put_Body. " & Lists.Type_Name (Type_Descriptor));
      end if;

      case Type_Descriptor.Label is
      when Lists.Array_Labels_Type =>
         if Type_Descriptor.Private_Implementation then
            Generate_Private_Array_Wrapper (File, Type_Descriptor);
         else
            declare
               Type_Name    : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);
               Package_Name : constant String := Instantiated_Package_Name (Type_Name);
            begin
               Declare_Function_Image (File, Type_Descriptor, Trivial => False);
               Indent_Line (File, Package_Name & ".Put (Buffer, Item);");
               End_Function_Image (File);
            end;
         end if;

      when Lists.Scalar_Labels_Type =>
         --  No body needed; Ada.Text_IO instantiated in spec.
         null;

      when Lists.Record_Label =>
         Generate_Record (File, Type_Descriptor);

      when Lists.Derived_Label =>
         case Type_Descriptor.Derived_Root_Label is
         when Lists.Array_Labels_Type =>
            Generate_Derived_Array (File, Type_Descriptor);

         when Lists.Record_Label =>
            raise SAL.Not_Implemented;
         end case;

      when Lists.Private_Label =>
         --  These should have been replaced by a specific label when
         --  the full type declaration was processed.
         raise Program_Error;

      when Lists.Access_Label =>
         Indent (File, "-- Generate_Image.Put_Body: Access_Label TODO");
      end case;
   end Generate;

   procedure Generate_Component_Line
     (File      : in Ada.Text_IO.File_Type;
      Component : in Auto_Io_Gen.Lists.Component_Type;
      First     : in Boolean)
   is
      pragma Unreferenced (First);
      Component_Name : constant String := Asis.Aux.Name (Component.Component_Name);
      Type_Name      : constant String := Asis.Aux.Name (Component.Type_Name);
      Method_Suffix  : String (1 .. 2) := "  ";
      Delimiter      : String (1 .. 3) := "   ";
      Is_String      : Boolean := False;
   begin

      if not Body_First then
         --  Finish last component put
         Indent_Line
           (File,
            "Put (To, Character' (','));",
            "if not Single_Line_Record then New_Line (To); end if;");

         --  Start current component put
         Indent_Line (File, "Put (To, Character' (' '));");

      else
         Body_First := False;
      end if;

      --  Indent_Line (File, "-- Generate_Component_Line : " & Type_Name);
      Indent_Incr (File, "if Named_Association_Record then");
      Indent_Line (File, "Put (To, """ & Component_Name & " => "");",
                         "if not Single_Line_Component then New_Line (To); end if;");
      Indent_Decr (File, "end if;");

      if Asis.Elements.Is_Nil (Component.Type_Package) then
         if Component.Invisible then
            Indent (File, "Private_Images.");
         else
            Set_Indent (File);
         end if;
      else
         Indent (File, Text_IO_Child_Name (Component.Type_Package) & '.');
      end if;

      if Type_Name = "String" then
         Delimiter := "'""'";
      elsif Type_Name = "Wide_String" then
         Method_Suffix := "w ";
         Delimiter := "'""'";
      elsif Type_Name = "Wide_Wide_String" then
         Method_Suffix := "ww";
         Delimiter := "'""'";
      elsif Type_Name = "Character" then
         Delimiter := "'''";
      elsif Type_Name = "Wide_Character" then
         Method_Suffix := "w ";
         Delimiter := "'''";
      elsif Type_Name = "Wide_Wide_Character" then
         Method_Suffix := "ww";
         Delimiter := "'''";
      end if;
      if Delimiter /= "   " then
         Put (File, "Put" & Method_Suffix & "(To, " & Delimiter & " & ");
      else
         Put (File, "Put (To, ");
         if Component.Scalar then
            Put (File, "Image (");
         end if;
      end if;
      Put (File, "Item." & Component_Name);
      if Delimiter /= "   " then
         Put (File, " & " & Delimiter);
      elsif Component.Scalar then
         Put (File, ")");
      end if;
      Put_Line (File, ");");

   end Generate_Component_Line;

   procedure Generate_Derived_Array
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is begin
      Indent_Incr (File, "procedure Put  -- Generate_Derived_Array");
      Indent_Line (File, "(To   : in out Unbounded_String;",
                         " Item : in " & Lists.Type_Name (Type_Descriptor) & ")");
      Indent_Less (File, "is begin");

      if Asis.Elements.Is_Nil (Type_Descriptor.Derived_Root_Package_Declaration) then
         Indent_Line
           (File, "Put (To, " &
                  Asis.Aux.Name (Type_Descriptor.Derived_Root_Type_Declaration) &
                  " (Item));");
      else
         Indent_Line
           (File, Asis.Aux.Name (Type_Descriptor.Derived_Root_Package_Declaration) &
                  ".Images.Put (To, " &
                  Asis.Aux.Name (Type_Descriptor.Derived_Root_Package_Declaration) &
                  "." &
                  Asis.Aux.Name (Type_Descriptor.Derived_Root_Type_Declaration) &
                  " (Item));");
      end if;

      Indent_Decr (File, "end Put;");
      New_Line (File);
      Declare_Function_Image (File, Type_Descriptor);
   end Generate_Derived_Array;

   procedure Generate_Private_Array_Wrapper
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is
      Package_Name : constant String := Instantiated_Package_Name (Asis.Aux.Name (Type_Descriptor.Type_Name));
   begin
      Instantiate_Generic_Array_Text_IO (File, Type_Descriptor);

      Indent_Incr (File, "procedure Put  -- Generate_Private_Array_Wrapper");
      Indent_Line (File, "(To   : in out Unbounded_String;",
                         " Item : in " & Lists.Type_Name (Type_Descriptor) & ")");

      Indent_Less (File, "is begin");
      Indent_Line (File, Package_Name & ".Put (To, Item);");
      Indent_Decr (File, "end Put;");
      New_Line (File);
      Declare_Function_Image (File, Type_Descriptor);
   end Generate_Private_Array_Wrapper;

   procedure Generate_Record
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Record_Type_Descriptor_Type)
   is
      Separate_Body_Name : constant String := "Put_" & Root_Type_Name (Asis.Aux.Name (Type_Descriptor.Type_Name));

      procedure Print_Parameter_List
        (Check_Unreferenced : in Boolean;
         Discriminants      : in Boolean;
         Separate_Body      : in Boolean)
      is
         pragma Unreferenced (Check_Unreferenced, Discriminants, Separate_Body);
      begin
         Indent_Line (File, "(To    : in out Unbounded_String;",
                            " Item : in " & Lists.Type_Name (Type_Descriptor) & ")");
         Indent_Less (File, "is begin");
      end Print_Parameter_List;

      procedure Print_Component
        (Component : in Lists.Component_Type;
         First     : in Boolean)
      is
         pragma Unreferenced (First);
      begin
         Generate_Component_Line (File, Component, Body_First);
      end Print_Component;

      procedure Print_Components is new Lists.Component_Algs.Process_All_Constant (Process_Item => Print_Component);

      procedure Print_Variant
        (Variant   : in Lists.Variant_Access_Type;
         First     : in Boolean)
      is
         pragma Unreferenced (First);
      begin
         Indent_Line (File, "when " & Asis.Aux.Name (Variant.Choice) & " =>");

         Indent_Level := Indent_Level + 1;

         if Lists.Length (Variant.Components) = 0 then
            Indent_Line (File, "null;");
         else
            Print_Components (Variant.Components);
         end if;

         Indent_Level := Indent_Level - 1;
      end Print_Variant;

      procedure Print_Variants is new Lists.Variant_Algs.Process_All_Constant (Process_Item => Print_Variant);

      procedure Print_Variant_Part
      is begin
         if Lists.Length (Type_Descriptor.Record_Variant_Part.Variants) = 0 then
            return;
         end if;

         --  Discriminant was output with other discriminants; only
         --  need here to generate case statement.
         Indent_Line (File, "case Item." & Asis.Aux.Name (Type_Descriptor.Record_Variant_Part.Discriminant) & " is");

         Print_Variants (Type_Descriptor.Record_Variant_Part.Variants);

         Indent_Line (File, "end case;");
      end Print_Variant_Part;

   begin
      if Type_Descriptor.Record_Tagged then
         if Type_Descriptor.Separate_Body then
            raise SAL.Not_Implemented;
         end if;

         Indent_Incr (File, "procedure Put_Components");

         Print_Parameter_List
           (Check_Unreferenced => True,
            Discriminants      => False,
            Separate_Body      => False);

         Body_First := True;

         if Type_Descriptor.Record_Derived then
            Indent_Line
              (File,
               Asis.Aux.Name (Type_Descriptor.Record_Parent_Package_Name) &
                 Options.Package_Separator & "Images.Put_Components");
            Indent_More
              (File,
               "(To, " &
                 Asis.Aux.Name (Type_Descriptor.Record_Parent_Package_Name) &
                 "." &
                 Asis.Aux.Name (Type_Descriptor.Record_Parent_Type_Name) &
                 " (Item));");
            Body_First := False;
         end if;

         Print_Components (Type_Descriptor.Record_Components);
         Print_Variant_Part;

         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "end Put_Components;");
         New_Line (File);
      end if;

      if Type_Descriptor.Separate_Body then
         Indent_Incr (File, "procedure " & Separate_Body_Name);

         Print_Parameter_List
           (Check_Unreferenced => not Type_Descriptor.Record_Tagged,
            Discriminants      => True,
            Separate_Body      => True);

         Indent_More (File, "is separate;");
         New_Line (File);
         Indent_Level := Indent_Level - 1;
      end if;

      Indent_Incr (File, "procedure Put");

      Print_Parameter_List
        (Check_Unreferenced => not Type_Descriptor.Record_Tagged,
         Discriminants      => True,
         Separate_Body      => Type_Descriptor.Separate_Body);

      if Type_Descriptor.Separate_Body then
         Indent_More (File, "renames " & Separate_Body_Name & ";");
      else
         Indent_Line (File, "Put (To, String' (""(""));");

         Body_First := True;
         Print_Components (Type_Descriptor.Record_Discriminants);

         if Type_Descriptor.Record_Tagged then
            if not Body_First then
               --  Finish last discriminant put
               Indent_Line
                 (File,
                  "Put (To, Character' (','));",
                  "if not Single_Line_Record then New_Line (To); end if;");
               --  Start components put
               Indent_Line (File, "Put (To, Character' (' '));");

            else
               Body_First := False;
            end if;
            Indent_Line (File, "Put_Components (To, Item);");
         else
            Print_Components (Type_Descriptor.Record_Components);
            Print_Variant_Part;
         end if;

         Indent_Line (File, "Put (To, String' ("")""));");
         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "end Put;");
      end if;

      New_Line (File);

      Declare_Function_Image (File, Type_Descriptor);
   end Generate_Record;

end Auto_Io_Gen.Generate.Ada_Image.Put_Body;
