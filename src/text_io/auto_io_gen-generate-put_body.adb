--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2001 - 2004, 2006 - 2007 Stephen Leake.  All Rights Reserved.
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
package body Auto_Io_Gen.Generate.Put_Body is
   use Ada.Text_IO;

   Body_First : Boolean := True; --  Shared between printing discriminants and components.

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
            --  No body needed; SAL.Gen_Array_Text_IO instantiated in spec.
            null;
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
      end case;
   end Generate;

   procedure Generate_Component_Line
      (File      : in Ada.Text_IO.File_Type;
       Component : in Auto_Io_Gen.Lists.Component_Type;
       First     : in Boolean)
   is
      pragma Unreferenced (First);
      Component_Name : constant String := Asis.Aux.Name (Component.Component_Name);
   begin

      if not Body_First then
         --  Finish last component put
         Indent_Line
            (File,
             "Put (File, Character' (',')); if not Single_Line_Record then New_Line (File); end if;");

         --  Start current component put
         Indent (File, "Put (File, Character' (' '));");

      else
         Body_First := False;
      end if;

      Indent_Line (File, "if Named_Association_Record then",
                         "   Put (File, """ & Component_Name & " => "");",
                         "   if not Single_Line_Component then New_Line (File); end if;",
                         "end if;");

      if Asis.Elements.Is_Nil (Component.Type_Package) then
         if Component.Invisible then
            Indent (File, "Private_Text_IO.");
         else
            Set_Indent (File);
         end if;
      else
         Indent (File, Text_IO_Child_Name (Component.Type_Package) & '.');
      end if;

      if Component.Scalar then
         Put_Line (File, "Put (File, Item." & Component_Name & ");");
      else
         Put_Line (File, "Put_Item (File, Item." & Component_Name & ",");
         Indent_More
            (File,
             "Single_Line => Single_Line_Component, Named_Association => Named_Association_Component);");
      end if;

   end Generate_Component_Line;

   procedure Generate_Derived_Array
      (File            : in Ada.Text_IO.File_Type;
       Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is begin
      Indent_Incr (File, "procedure Put");

      Indent_Line (File, "(File                      : in " & Ada_Text_IO & ".File_Type;",
                         " Item                      : in " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Single_Line_Array         : in Boolean := False;",
                         " Named_Association_Array   : in Boolean := False;",
                         " Single_Line_Element       : in Boolean := True;",
                         " Named_Association_Element : in Boolean := False)");

      Indent_Less (File, "is begin");

      if Asis.Elements.Is_Nil (Type_Descriptor.Derived_Root_Package_Declaration) then
         Indent (File, "Put");
         Indent_Line
           (File,
            " (File, " &
              Asis.Aux.Name (Type_Descriptor.Derived_Root_Type_Declaration) &
              " (Item),");
      else
         Indent_Line
           (File,
            Asis.Aux.Name (Type_Descriptor.Derived_Root_Package_Declaration) &
              ".Text_IO.Put");

         Indent_Line
           (File,
            " (File, " &
              Asis.Aux.Name (Type_Descriptor.Derived_Root_Package_Declaration) &
              "." &
              Asis.Aux.Name (Type_Descriptor.Derived_Root_Type_Declaration) &
              " (Item),");
      end if;

      Indent_Line
        (File, " Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);");
      Indent_Decr (File, "end Put;");
      New_Line (File);

      Indent_Incr (File, "procedure Put");
      Indent_Line (File, "(Item                      : in " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Single_Line_Array         : in Boolean := False;",
                         " Named_Association_Array   : in Boolean := False;",
                         " Single_Line_Element       : in Boolean := True;",
                         " Named_Association_Element : in Boolean := False)");
      Indent_Less (File, "is begin");
      Indent_Line (File, "Put (Current_Output, Item,");
      Indent_Line
        (File, "     Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);");
      Indent_Decr (File, "end Put;");
      New_Line (File);

      Indent_Incr (File, "procedure Put_Item");
      Indent_Line (File, "(File              : in " & Ada_Text_IO & ".File_Type;",
                         " Item              : in " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Single_Line       : in Boolean := False;",
                         " Named_Association : in Boolean := False)");
      Indent_Less (File, "is begin");
      Indent_Line (File, "Put (File, Item, Single_Line, Named_Association, Single_Line, Named_Association);");
      Indent_Decr (File, "end Put_Item;");
      New_Line (File);
   end Generate_Derived_Array;

   procedure Generate_Private_Array_Wrapper
      (File            : in Ada.Text_IO.File_Type;
       Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is
      Package_Name : constant String := Instantiated_Package_Name (Asis.Aux.Name (Type_Descriptor.Type_Name));
   begin
      Instantiate_Generic_Array_Text_IO (File, Type_Descriptor);

      case Type_Descriptor.Array_Component_Label is
      when Lists.Private_Label =>
         null;

      when Lists.Enumeration_Label =>
         Indent_Incr (File, "procedure Set_" & Package_Name & "_Default_Width (Width : in Ada.Text_IO.Field)");
         Indent_Less (File, "is begin");
         Indent_Line (File, Package_Name & ".Default_Width := Width;");
         Indent_Decr (File, "end Set_" & Package_Name & "_Default_Width;");

         Indent_Incr (File, "procedure Set_" & Package_Name & "_Default_Setting (Setting : in Ada.Text_IO.Type_Set)");
         Indent_Less (File, "is begin");
         Indent_Line (File, Package_Name & ".Default_Setting := Setting;");
         Indent_Decr (File, "end Set_" & Package_Name & "_Default_Setting;");

      when Lists.Float_Label =>
         Indent_Incr (File, "procedure Set_" & Package_Name & "_Default_Fore (Fore : in Ada.Text_IO.Field)");
         Indent_Less (File, "is begin");
         Indent_Line (File, Package_Name & ".Default_Fore := Fore;");
         Indent_Decr (File, "end Set_" & Package_Name & "_Default_Fore;");

         Indent_Incr (File, "procedure Set_" & Package_Name & "_Default_Aft (Aft : in Ada.Text_IO.Field)");
         Indent_Less (File, "is begin");
         Indent_Line (File, Package_Name & ".Default_Aft := Aft;");
         Indent_Decr (File, "end Set_" & Package_Name & "_Default_Aft;");

         Indent_Incr (File, "procedure Set_" & Package_Name & "_Default_Exp (Exp : in Ada.Text_IO.Field)");
         Indent_Less (File, "is begin");
         Indent_Line (File, Package_Name & ".Default_Exp := Exp;");
         Indent_Decr (File, "end Set_" & Package_Name & "_Default_Exp;");

      when Lists.Signed_Integer_Label | Lists.Modular_Integer_Label =>
         Indent_Incr (File, "procedure Set_" & Package_Name & "_Default_Width (Width : Ada.Text_IO.Field)");
         Indent_Less (File, "is begin");
         Indent_Line (File, Package_Name & ".Default_Width := Width;");
         Indent_Decr (File, "end Set_" & Package_Name & "_Default_Width;");

         Indent_Incr (File, "procedure Set_" & Package_Name & "_Default_Base (Base : Ada.Text_IO.Number_Base)");
         Indent_Less (File, "is begin");
         Indent_Line (File, Package_Name & ".Default_Base := Base;");
         Indent_Decr (File, "end Set_" & Package_Name & "_Default_Base;");

      end case;

      Indent_Incr (File, "procedure Put");

      Indent_Line (File, "(File                        : in " & Ada_Text_IO & ".File_Type;",
                         " Item                        : in " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Single_Line_Record          : in Boolean := True;",
                         " Named_Association_Record    : in Boolean := False;",
                         " Single_Line_Component       : in Boolean := True;",
                         " Named_Association_Component : in Boolean := False)");

      Indent_Level := Indent_Level - 1;

      if Type_Descriptor.Array_Component_Label in Lists.Scalar_Array_Component_Labels_Type then
         Indent_Incr (File, "is");
         Indent_Line (File, "pragma Unreferenced (Single_Line_Component);",
                            "pragma Unreferenced (Named_Association_Component);");
         Indent_Decr (File, "begin");
      else
         Indent_Line (File, "is begin");
      end if;

      Indent_Level := Indent_Level + 1;

      Indent (File, Package_Name & ".Put");
      Indent_Line (File, " (File, Item,");
      if Type_Descriptor.Array_Component_Label in Lists.Scalar_Array_Component_Labels_Type then
         Indent_Line (File, " Single_Line => Single_Line_Record, Named_Association => Named_Association_Record);");
      else
         Indent_Line
           (File,
            " Single_Line_Record, Named_Association_Record, Single_Line_Component, Named_Association_Component);");
      end if;

      Indent_Decr (File, "end Put;");
      New_Line (File);

      Indent_Incr (File, "procedure Put");
      Indent_Line (File, "(Item                        : in " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Single_Line_Record          : in Boolean := True;",
                         " Named_Association_Record    : in Boolean := False;",
                         " Single_Line_Component       : in Boolean := True;",
                         " Named_Association_Component : in Boolean := False)");

      if Type_Descriptor.Array_Component_Label in Lists.Scalar_Array_Component_Labels_Type then
         Indent_Less (File, "is");
         Indent_Line (File, "pragma Unreferenced (Single_Line_Component);",
                            "pragma Unreferenced (Named_Association_Component);");
         Indent_Less (File, "begin");
      else
         Indent_Less (File, "is begin");
      end if;

      Indent_Line (File, Package_Name & ".Put (Current_Output, Item,");
      case Type_Descriptor.Array_Component_Label is
      when Lists.Scalar_Array_Component_Labels_Type =>
         Indent_Line (File, " Single_Line => Single_Line_Record,",
                            " Named_Association => Named_Association_Record);");

      when Lists.Private_Label =>
         Indent_Line
           (File,
            " Single_Line_Record, Named_Association_Record, Single_Line_Component, Named_Association_Component);");
      end case;

      Indent_Decr (File, "end Put;");
      New_Line (File);

      Indent_Incr (File, "procedure Put_Item");
      Indent_Line (File, "(File              : in " & Ada_Text_IO & ".File_Type;",
                         " Item              : in " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Single_Line       : in Boolean := False;",
                         " Named_Association : in Boolean := False)");
      Indent_Less (File, "is begin");
      Indent_Line (File, Package_Name & ".Put_Item (File, Item, Single_Line, Named_Association);");
      Indent_Decr (File, "end Put_Item;");
      New_Line (File);
   end Generate_Private_Array_Wrapper;

   procedure Generate_Record
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Record_Type_Descriptor_Type)
   is
      Separate_Body_Name : constant String := "Put_" & Root_Type_Name (Asis.Aux.Name (Type_Descriptor.Type_Name));

      procedure Print_Parameter_List
        (With_File          : in Boolean;
         Is_Item            : in Boolean;
         Check_Unreferenced : in Boolean;
         Discriminants      : in Boolean;
         Separate_Body      : in Boolean)
      is
         Need_Single_Line_Record : Boolean;
      begin
         Indent_Level := Indent_Level + 1;
         if With_File then
            Indent_Line (File, "(File                        : in " & Ada_Text_IO & ".File_Type;");
            Indent (File, " ");
         else
            Indent (File, "(");
         end if;

         Put_Line (File, "Item                        : in " & Lists.Type_Name (Type_Descriptor) & ";");

         if Is_Item then
            Indent_Line (File, " Single_Line                 : in Boolean := False;",
                               " Named_Association           : in Boolean := False)");
         else
            Indent_Line (File, " Single_Line_Record          : in Boolean := True;",
                               " Named_Association_Record    : in Boolean := False;",
                               " Single_Line_Component       : in Boolean := True;",
                               " Named_Association_Component : in Boolean := False)");
         end if;

         Indent_Level := Indent_Level - 1;

         if not Separate_Body then
            Indent_Line (File, "is");

            if Check_Unreferenced then
               if Discriminants then
                  Need_Single_Line_Record :=
                    Lists.Length (Type_Descriptor.Record_Components) +
                    Lists.Length (Type_Descriptor.Record_Discriminants) +
                    Lists.Length (Type_Descriptor.Record_Variant_Part.Variants) > 1;
               else
                  Need_Single_Line_Record :=
                    Lists.Length (Type_Descriptor.Record_Components) > 1;
               end if;

               if not Need_Single_Line_Record then
                  Indent_More (File, "pragma Unreferenced (Single_Line_Record);");
               end if;

               if not Type_Descriptor.Record_Structured_Components then
                  Indent_More (File, "pragma Unreferenced (Named_Association_Component);");
               end if;
            end if;

            Indent_Incr (File, "begin");
         end if;
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
        (Variant : in Lists.Variant_Access_Type;
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

         Indent_Line (File, "procedure Put_Components");

         Print_Parameter_List
           (Is_Item            => False,
            With_File          => True,
            Check_Unreferenced => True,
            Discriminants      => False,
            Separate_Body      => False);

         Body_First := True;

         if Type_Descriptor.Record_Derived then
            Indent_Line
              (File,
               Asis.Aux.Name (Type_Descriptor.Record_Parent_Package_Name) &
                 Options.Package_Separator & "Text_IO.Put_Components");
            Indent_Line
              (File,
               "(File, " &
                 Asis.Aux.Name (Type_Descriptor.Record_Parent_Package_Name) &
                 "." &
                 Asis.Aux.Name (Type_Descriptor.Record_Parent_Type_Name) &
                 " (Item), Single_Line_Record, Named_Association_Record,");
            Indent_Line (File, "     Single_Line_Component, Named_Association_Component);");
            Body_First := False;
         end if;

         Print_Components (Type_Descriptor.Record_Components);
         Print_Variant_Part;

         Indent_Decr (File, "end Put_Components;");
         New_Line (File);
      end if;

      if Type_Descriptor.Separate_Body then
         Indent_Line (File, "procedure " & Separate_Body_Name);

         Print_Parameter_List
           (Is_Item            => False,
            With_File          => True,
            Check_Unreferenced => not Type_Descriptor.Record_Tagged,
            Discriminants      => True,
            Separate_Body      => True);

         Indent_Line (File, "is separate;");
         New_Line (File);
      end if;

      Indent_Line (File, "procedure Put");

      Print_Parameter_List
        (Is_Item            => False,
         With_File          => True,
         Check_Unreferenced => not Type_Descriptor.Record_Tagged,
         Discriminants      => True,
         Separate_Body      => Type_Descriptor.Separate_Body);

      if Type_Descriptor.Separate_Body then
         Indent_Line (File, "renames " & Separate_Body_Name & ";");
      else
         Indent_Line (File, "Put (File, String' (""(""));");

         Body_First := True;
         Print_Components (Type_Descriptor.Record_Discriminants);

         if Type_Descriptor.Record_Tagged then
            if not Body_First then
               --  Finish last discriminant put
               Indent_Line
                 (File,
                  "Put (File, Character' (',')); if not Single_Line_Record then New_Line (File); end if;");
               --  Start components put
               Indent_Line (File, "Put (File, Character' (' '));");

            else
               Body_First := False;
            end if;
            Indent_Line (File, "Put_Components (File, Item, Single_Line_Record, Named_Association_Record,",
                               "                Single_Line_Component, Named_Association_Component);");
         else
            Print_Components (Type_Descriptor.Record_Components);
            Print_Variant_Part;
         end if;

         Indent_Line (File, "Put (File, String' ("")""));");
         Indent_Decr (File, "end Put;");
      end if;

      New_Line (File);

      Indent_Line (File, "procedure Put");

      Print_Parameter_List
        (Is_Item            => False,
         With_File          => False,
         Check_Unreferenced => False,
         Discriminants      => True,
         Separate_Body      => False);

      Indent_Line (File, "Put (Current_Output, Item, Single_Line_Record, Named_Association_Record,",
                         "     Single_Line_Component, Named_Association_Component);");
      Indent_Decr (File, "end Put;");
      New_Line (File);

      Indent_Line (File, "procedure Put_Item");

      Print_Parameter_List
        (Is_Item            => True,
         With_File          => True,
         Check_Unreferenced => False,
         Discriminants      => True,
         Separate_Body      => False);

      Indent_Line (File, "Put (File, Item, Single_Line, Named_Association,",
                         "     Single_Line, Named_Association);");
      Indent_Decr (File, "end Put_Item;");
      New_Line (File);
   end Generate_Record;

end Auto_Io_Gen.Generate.Put_Body;
