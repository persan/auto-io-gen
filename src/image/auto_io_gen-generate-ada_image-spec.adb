--  Abstract :
--
--  See spec
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
--
with Ada.Text_IO;
with Asis.Aux;
with Auto_Io_Gen.Options;
with SAL.Gen.Alg.Process_All_Constant;
with GNAT.Source_Info;
package body Auto_Io_Gen.Generate.Ada_Image.Spec is
   use  GNAT.Source_Info;
   procedure Generate_Put_Array_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate spec code for the Get and Put procedures for an array type.

   procedure Generate_Put_Derived_Array_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate spec code for the Get and Put procedures for a derived array type.

   procedure Generate_Put_Private_Array_Defaults
     (File      : in Ada.Text_IO.File_Type;
      Label     : in Auto_Io_Gen.Lists.Array_Component_Labels_Type;
      Type_Name : in Asis.Element);

   procedure Generate_Put_Private_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate spec code for the Get and Put procedures for a private
   --  type; full implementation is in body or private child.

   procedure Generate_Put_Record_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate spec code for the Get and Put procedures for a record type.

   procedure Generate_Put_Scalar_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate spec code for the Get and Put procedures for a Scalar type.

   procedure Generate_Child_Spec
     (File                : in Ada.Text_IO.File_Type;
      Type_List           : in Lists.Type_Descriptor_Lists.List_Type;
      With_List           : in Lists.Context_Trees.Tree_Type;
      Formal_Package_List : in Lists.Formal_Package_Lists.List_Type;
      Parent_Package_Name : in String;
      Child_Package_Name  : in String;
      Invisible           : in Boolean;
      Is_Generic          : in Boolean)
   is
      use Ada.Text_IO;

      procedure Print_Header (Type_List : in Auto_Io_Gen.Lists.Type_Descriptor_Lists.List_Type)
      is
         pragma Unreferenced (Type_List);
      begin
         Put_Line (File, "--  Abstract :");
         Put_Line (File, "--");
         Put_Line (File, "--  Images for types in " & Parent_Package_Name);
         Put_Line (File, "--");
         Put_Line (File, "--  This file is auto-generated by " & Auto_Io_Gen.Program_Name);
         Put_Line (File, "--  from " & Parent_Package_Name);
         Put_Line (File, "--");
         Put_Line (File, "with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;");
         New_Line (File);

         declare
            use Lists.Context_Trees_Iterators;
            Iterator : Iterator_Type := First (With_List);
         begin
            loop
               exit when Is_Null (Iterator);

               if Current (Iterator).Need_Use then
                  Put_Line (File, "with " & Current (Iterator).Name.all & "; use " &
                              Current (Iterator).Name.all & ";");
               else
                  Put_Line (File, "with " & Current (Iterator).Name.all & ";");
               end if;

               Next (Iterator);
            end loop;
         end;


         if Is_Generic then
            Put_Line (File, "generic");
            Indent_Level := 2;

            declare
               procedure Put_Argument_List (List : in Lists.Element_Lists.List_Type)
               is
                  use Lists.Element_Lists;
                  I : Iterator_Type := First (List);
               begin
                  if Is_Null (I) then
                     return;
                  end if;

                  New_Line (File);
                  Put (File, " (");
                  loop
                     Put (File, Text_IO_Child_Name (Current (I), Generic_Formal));
                     Next (I);
                     exit when Is_Null (I);
                     Put (File, ", ");
                  end loop;
                  Put (File, ")");
               end Put_Argument_List;

               use Lists.Formal_Package_Lists;
               Iterator : Iterator_Type := First (Formal_Package_List);
            begin
               loop
                  exit when Is_Null (Iterator);
                  Indent
                    (File,
                     "with package " &
                       Text_IO_Child_Name (Current (Iterator).Package_Declaration) &
                       " is new " &
                       Text_IO_Child_Name (Current (Iterator).Package_Declaration, Generic_Unit));

                  Put_Argument_List (Current (Iterator).Arguments);
                  Put_Line (File, ";");

                  Next (Iterator);
               end loop;
            end;
            Indent_Level := 1;
         end if;

         if Invisible then
            Put_Line (File, "private package " & Child_Package_Name & " is");
         else
            Put_Line (File, "package " & Child_Package_Name & " is");
         end if;

         Indent_Level := 2;

         New_Line (File);

      end Print_Header;

      procedure Print_Type
        (Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Access_Type;
         First           : in Boolean                                       := True)
      is
         pragma Unreferenced (First);
      begin
         if Invisible then
            --  Creating the private text_io child; does this type belong here?
            if not Type_Descriptor.Invisible then
               return;
            end if;
         else
            --  Creating the public text_io child.
            if Type_Descriptor.Invisible then
               return;
            end if;
         end if;

         case Type_Descriptor.Label is
         when Auto_Io_Gen.Lists.Array_Labels_Type =>
            if Type_Descriptor.Private_Implementation then
               Generate_Put_Private_Array_Defaults
                 (File, Type_Descriptor.Array_Component_Label, Type_Descriptor.Type_Name);
               Generate_Put_Private_Spec (File, Type_Descriptor.all);
            else
               Generate_Put_Array_Spec (File, Type_Descriptor.all);
            end if;

         when Auto_Io_Gen.Lists.Record_Label =>
            Generate_Put_Record_Spec (File, Type_Descriptor.all);

         when Auto_Io_Gen.Lists.Derived_Label =>
            case Type_Descriptor.Derived_Root_Label is
            when Auto_Io_Gen.Lists.Array_Labels_Type =>
               Generate_Put_Derived_Array_Spec (File, Type_Descriptor.all);

            when Auto_Io_Gen.Lists.Record_Label =>
               raise Not_Implemented;
            end case;

         when Lists.Private_Label =>
            --  These should have been replaced by a specific label when
            --  the full type declaration was processed.
            raise
              Program_Error with
                "Private_Label " & Lists.Type_Name (Type_Descriptor.all) &
              " should have been replaced; perhaps add 'ignore' to public part";

         when Auto_Io_Gen.Lists.Scalar_Labels_Type =>
            Generate_Put_Scalar_Spec (File, Type_Descriptor.all);

         end case;
      end Print_Type;

      procedure Print_Footer (Type_List : in Auto_Io_Gen.Lists.Type_Descriptor_Lists.List_Type)
      is
         pragma Unreferenced (Type_List);
      begin
         Indent_Level := 1;
         Put_Line (File, "end " & Child_Package_Name & ";");
      end Print_Footer;

      procedure Print_Type_List is new Auto_Io_Gen.Lists.Type_Descriptor_Algs.Process_All_Constant
        (Pre_Process_Container  => Print_Header,
         Process_Item           => Print_Type,
         Post_Process_Container => Print_Footer);

   begin
      Print_Type_List (Type_List);
   end Generate_Child_Spec;

   procedure Do_Put_Rename
     (File         : in Ada.Text_IO.File_Type;
      Type_Name    : in String;
      Package_Name : in String;
      Label        : in Lists.Scalar_Labels_Type;
      File_Param   : in Boolean;
      Is_Array     : in Boolean                 := False;
      Is_Item      : in Boolean                 := False)
   is
      pragma Unreferenced (Label, File_Param, Is_Array, Is_Item);
      use Ada.Text_IO;
   begin
      Put_Line (Enclosing_Entity & "(" & Type_Name & ", " & Package_Name & ")");
      Indent_Line (File, "function Image (Item : in " & Type_Name & ") return String",
                         "               renames " & Package_Name & ".Image;");
   end Do_Put_Rename;


   procedure Do_Rename
     (File         : in Ada.Text_IO.File_Type;
      Type_Name    : in String;
      Package_Name : in String;
      Label        : in Lists.Scalar_Labels_Type;
      File_Param   : in Boolean;
      Is_Array     : in Boolean                 := False;
      Is_Item      : in Boolean                 := False)
   is begin
      Do_Put_Rename (File, Type_Name, Package_Name, Label, File_Param, Is_Array, Is_Item);
   end Do_Rename;

   procedure Generate_Put_Array_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;
      Type_Name    : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);
      Package_Name : constant String := Instantiated_Package_Name (Type_Name);

      procedure Do_Renames (Element_Label : in Lists.Type_Labels_Type)
      is begin
         Do_Rename (File, Type_Name, Package_Name, Element_Label, File_Param => True, Is_Array => True);
      end Do_Renames;
   begin
      if Options.Debug then
         Put_Line (Enclosing_Entity & "(" & Type_Name & ")");
      end if;

      Instantiate_Generic_Array_Text_IO (File, Type_Descriptor);

      case Type_Descriptor.Array_Component_Label is
      when Lists.Private_Label =>

         Indent_Line (File, "function Image (Item : in " & Type_Name & ") return String;");
         New_Line (File);

      when Lists.Enumeration_Label =>
         Do_Renames (Lists.Enumeration_Label);

      when Lists.Float_Label =>
         --  Mapping from Array_Component_Labels_Type to Type_Labels_Type
         Do_Renames (Lists.Float_Label);

      when Lists.Signed_Integer_Label =>
         Do_Renames (Lists.Signed_Integer_Label);

      when Lists.Modular_Integer_Label =>
         Do_Renames (Lists.Modular_Integer_Label);

      end case;

      New_Line (File);
   end Generate_Put_Array_Spec;

   procedure Generate_Put_Derived_Array_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;

      Type_Name : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);

   begin
      if Options.Debug then
         Put_Line (Enclosing_Entity & "(" & Type_Name & ")");
      end if;
      Indent_Line (File, "function I_Image (  Item : in " & Type_Name & ") return String;");

      New_Line (File);

   end Generate_Put_Derived_Array_Spec;

   procedure Generate_Put_Private_Array_Defaults
     (File      : in Ada.Text_IO.File_Type;
      Label     : in Auto_Io_Gen.Lists.Array_Component_Labels_Type;
      Type_Name : in Asis.Element)
   is
      pragma Unreferenced (Label);
      -- Package_Name : constant String := Instantiated_Package_Name (Asis.Aux.Name (Type_Name));
   begin
      Indent_Line (File, "function I_Image (Item : in " &  Asis.Aux.Name (Type_Name) & ") return String;");

   end Generate_Put_Private_Array_Defaults;

   procedure Generate_Put_Private_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;

      Type_Name : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);

   begin
      if Options.Debug then
         Put_Line (Enclosing_Entity & "(" & Type_Name & ")");
      end if;

      Indent_Line (File, "function I_Image (Item : in " & Type_Name & ") return String;");
      if Type_Descriptor.Record_Tagged then
         Indent_Line (File, "function I_Image_Components (Item : in " & Type_Name & ") return String;");
      end if;

      New_Line (File);
   end Generate_Put_Private_Spec;

   procedure Generate_Put_Record_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;

      Type_Name : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);

   begin
      if Options.Debug then
         Put_Line (Enclosing_Entity & "(" & Type_Name & ")");
      end if;

      Indent_Line (File, "procedure Put (To   : in out Unbounded_String;",
                         "               Item : in     " & Type_Name & ");");
      New_Line (File);
      Indent_Line (File, "function Image (Item : in " & Type_Name & ") return String;");
      -- if Type_Descriptor.Record_Tagged then
      --    Indent_Line (File, "function Image_Components (Item : in " & Type_Name & ") return String;");
      -- end if;

      New_Line (File);
   end Generate_Put_Record_Spec;

   procedure Generate_Put_Scalar_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;

      Type_Name    : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);

   begin
      if Options.Debug then
         Put_Line (Enclosing_Entity & "(" & Type_Name & ")");
      end if;
      Indent_Line (File, "function Image (Item : " & Type_Name & ") return String is (Item'Img);");
   end Generate_Put_Scalar_Spec;

end Auto_Io_Gen.Generate.Ada_Image.Spec;
