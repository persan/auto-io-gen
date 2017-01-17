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
with Auto_Text_IO.Options;
with SAL.Gen.Alg.Process_All_Constant;
package body Auto_Text_IO.Generate.Spec is

   procedure Generate_Put_Get_Array_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type);
   --  Generate spec code for the Get and Put procedures for an array type.

   procedure Generate_Put_Get_Derived_Array_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type);
   --  Generate spec code for the Get and Put procedures for a derived array type.

   procedure Generate_Put_Get_Private_Array_Defaults
     (File      : in Ada.Text_IO.File_Type;
      Label     : in Auto_Text_IO.Lists.Array_Component_Labels_Type;
      Type_Name : in Asis.Element);

   procedure Generate_Put_Get_Private_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type);
   --  Generate spec code for the Get and Put procedures for a private
   --  type; full implementation is in body or private child.

   procedure Generate_Put_Get_Record_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type);
   --  Generate spec code for the Get and Put procedures for a record type.

   procedure Generate_Put_Get_Scalar_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type);
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

      procedure Print_Header (Type_List : in Auto_Text_IO.Lists.Type_Descriptor_Lists.List_Type)
      is
         pragma Unreferenced (Type_List);
      begin
         Put_Line (File, "--  Abstract :");
         Put_Line (File, "--");
         Put_Line (File, "--  Text_IO for types in " & Parent_Package_Name);
         Put_Line (File, "--");
         Put_Line (File, "--  This file is auto-generated by " & Auto_Text_IO.Program_Name);
         Put_Line (File, "--  from " & Parent_Package_Name);
         Put_Line (File, "--");

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

         Put_Line (File, "with " & Ada_Text_IO & ";");

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
        (Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Access_Type;
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
         when Auto_Text_IO.Lists.Array_Labels_Type =>
            if Type_Descriptor.Private_Implementation then
               Generate_Put_Get_Private_Array_Defaults
                 (File, Type_Descriptor.Array_Component_Label, Type_Descriptor.Type_Name);
               Generate_Put_Get_Private_Spec (File, Type_Descriptor.all);
            else
               Generate_Put_Get_Array_Spec (File, Type_Descriptor.all);
            end if;

         when Auto_Text_IO.Lists.Record_Label =>
            Generate_Put_Get_Record_Spec (File, Type_Descriptor.all);

         when Auto_Text_IO.Lists.Derived_Label =>
            case Type_Descriptor.Derived_Root_Label is
            when Auto_Text_IO.Lists.Array_Labels_Type =>
               Generate_Put_Get_Derived_Array_Spec (File, Type_Descriptor.all);

            when Auto_Text_IO.Lists.Record_Label =>
               raise Not_Implemented;
            end case;

         when Lists.Private_Label =>
            --  These should have been replaced by a specific label when
            --  the full type declaration was processed.
            raise
              Program_Error with
                "Private_Label " & Lists.Type_Name (Type_Descriptor.all) &
              " should have been replaced; perhaps add 'ignore' to public part";

         when Auto_Text_IO.Lists.Scalar_Labels_Type =>
            Generate_Put_Get_Scalar_Spec (File, Type_Descriptor.all);

         end case;
      end Print_Type;

      procedure Print_Footer (Type_List : in Auto_Text_IO.Lists.Type_Descriptor_Lists.List_Type)
      is
         pragma Unreferenced (Type_List);
      begin
         Put_Line (File, "end " & Child_Package_Name & ";");
      end Print_Footer;

      procedure Print_Type_List is new Auto_Text_IO.Lists.Type_Descriptor_Algs.Process_All_Constant
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
      use Ada.Text_IO;
   begin
      if Is_Item then
         Indent_Line (File, "procedure Put_Item");
      else
         Indent_Line (File, "procedure Put");
      end if;

      Indent_Level := Indent_Level + 1;

      if File_Param then
         Indent_Line (File, "(File  : in " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item  : in " & Type_Name & ";");
      else
         Indent_Line (File, "(Item  : in " & Type_Name & ";");
      end if;

      if not Is_Item then
         case Label is
         when Lists.Enumeration_Label =>
            Indent_Line (File, " Width : in " & Ada_Text_IO & ".Field := " & Package_Name & ".Default_Width;");
            Indent      (File, " Set   : in " & Ada_Text_IO & ".Type_Set := " & Package_Name & ".Default_Setting");

         when Lists.Float_Label | Lists.Fixed_Label =>
            Indent_Line (File, " Fore  : in " & Ada_Text_IO & ".Field := " & Package_Name & ".Default_Fore;");
            Indent_Line (File, " Aft   : in " & Ada_Text_IO & ".Field := " & Package_Name & ".Default_Aft;");
            Indent      (File, " Exp   : in " & Ada_Text_IO & ".Field := " & Package_Name & ".Default_Exp");

         when Lists.Signed_Integer_Label |
              Lists.Modular_Integer_Label =>

            Indent_Line (File, " Width : in " & Ada_Text_IO & ".Field := " & Package_Name & ".Default_Width;");
            Indent      (File, " Base  : in " & Ada_Text_IO & ".Field := " & Package_Name & ".Default_Base");

         end case;
      end if;

      if Is_Array then
         if not Is_Item then
            Put_Line (File, ";");
         end if;
         Indent_Line (File, " Single_Line       : in Boolean := " & Package_Name & ".Default_Single_Line;");

         --  This line gets too long for the GNAT style check. Need
         --  a general wrap mechanism, but this works for now.
         Indent_Line (File, " Named_Association : in Boolean :=");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, Package_Name & ".Default_Named_Association)");
         Indent_Level := Indent_Level - 1;
      else
         Put_Line (File, ")");
      end if;

      if Is_Item and Is_Array then
         Indent_Line (File, "renames " & Package_Name & ".Put_Item;");
      else
         Indent_Line (File, "renames " & Package_Name & ".Put;");
      end if;

      Indent_Level := Indent_Level - 1;

   end Do_Put_Rename;

   procedure Do_Get_Rename
     (File         : in Ada.Text_IO.File_Type;
      Type_Name    : in String;
      Package_Name : in String;
      Label        : in Lists.Scalar_Labels_Type;
      File_Param   : in Boolean;
      Is_Array     : in Boolean                 := False;
      Is_Item      : in Boolean                 := False)
   is
      use Ada.Text_IO;
   begin
      if Is_Item then
         Indent_Line (File, "procedure Get_Item");
      else
         Indent_Line (File, "procedure Get");
      end if;

      Indent_Level := Indent_Level + 1;

      if File_Param then
         Indent_Line (File, "(File  : in     " & Ada_Text_IO & ".File_Type;");
         Indent      (File, " Item  :    out " & Type_Name);
      else
         Indent      (File, "(Item  :    out " & Type_Name);
      end if;

      if not Is_Item then
         case Label is
         when Lists.Enumeration_Label =>
            null;

         when Lists.Float_Label | Lists.Fixed_Label =>
            Put_Line (File, ";");
            Indent (File, " Width : in     " & Ada_Text_IO & ".Field := 0");

         when Lists.Signed_Integer_Label |
              Lists.Modular_Integer_Label =>
            Put_Line (File, ";");
            Indent (File, " Width : in     " & Ada_Text_IO & ".Field := 0");

         end case;
      end if;

      if Is_Array then
         Put_Line (File, ";");

         --  This line gets too long for the GNAT style check. Need
         --  a general wrap mechanism, but this works for now.
         Indent_Line (File, " Named_Association : in Boolean :=");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, Package_Name & ".Default_Named_Association)");
         Indent_Level := Indent_Level - 1;
      else
         Put_Line (File, ")");
      end if;

      if Is_Item and Is_Array then
         Indent_Line (File, "renames " & Package_Name & ".Get_Item;");
      else
         Indent_Line (File, "renames " & Package_Name & ".Get;");
      end if;

      Indent_Level := Indent_Level - 1;

   end Do_Get_Rename;

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
      Do_Get_Rename (File, Type_Name, Package_Name, Label, File_Param, Is_Array, Is_Item);
   end Do_Rename;

   procedure Generate_Put_Get_Array_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;
      Type_Name    : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);
      Package_Name : constant String := Instantiated_Package_Name (Type_Name);

      procedure Do_Renames (Element_Label : in Lists.Type_Labels_Type)
      is begin
         Do_Rename (File, Type_Name, Package_Name, Element_Label, File_Param => True, Is_Array => True);
         Do_Rename (File, Type_Name, Package_Name, Element_Label, File_Param => False, Is_Array => True);
         Do_Rename (File, Type_Name, Package_Name, Element_Label,
                    File_Param => True, Is_Item => True, Is_Array => True);

      end Do_Renames;
   begin
      if Options.Debug then
         Put_Line ("Generate_Get_Put_Array_Spec. " & Type_Name);
      end if;

      Instantiate_Generic_Array_Text_IO (File, Type_Descriptor);

      case Type_Descriptor.Array_Component_Label is
      when Lists.Private_Label =>

         Indent_Line (File, "procedure Put");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, "(File                      : in " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item                      : in " & Type_Name & ";");
         Indent_Line (File, " Single_Line_Array         : in Boolean := " &
                        Package_Name & ".Default_Single_Line_Array;");
         Indent_Line (File, " Named_Association_Array   : in Boolean := " &
                        Package_Name & ".Default_Named_Association_Array;");
         Indent_Line (File, " Single_Line_Element       : in Boolean := " &
                        Package_Name & ".Default_Single_Line_Element;");

         --  This line gets too long for the GNAT style check.
         --  Need a general wrap mechanism, but this works for
         --  now.
         Indent_Line (File, " Named_Association_Element : in Boolean :=");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, Package_Name & ".Default_Named_Association_Element)");
         Indent_Level := Indent_Level - 1;

         Indent_Line (File, "renames " & Package_Name & ".Put;");
         Indent_Level := Indent_Level - 1;

         Indent_Line (File, "procedure Put");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, "(Item                : in " & Type_Name & ";");
         Indent_Line (File, " Single_Line_Array         : in Boolean := " &
                        Package_Name & ".Default_Single_Line_Array;");
         Indent_Line (File, " Named_Association_Array   : in Boolean := " &
                        Package_Name & ".Default_Named_Association_Array;");
         Indent_Line (File, " Single_Line_Element       : in Boolean := " &
                        Package_Name & ".Default_Single_Line_Element;");

         --  This line gets too long for the GNAT style check.
         --  Need a general wrap mechanism, but this works for
         --  now.
         Indent_Line (File, " Named_Association_Element : in Boolean :=");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, Package_Name & ".Default_Named_Association_Element)");
         Indent_Level := Indent_Level - 1;

         Indent_Line (File, "renames " & Package_Name & ".Put;");
         Indent_Level := Indent_Level - 1;

         Indent_Line (File, "procedure Put_Item");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, "(File                 : in " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item                 : in " & Type_Name & ";");
         Indent_Line (File, " Single_Line          : in Boolean := " &
                        Package_Name & ".Default_Single_Line_Array;");
         Indent_Line (File, " Named_Association    : in Boolean := " &
                        Package_Name & ".Default_Named_Association_Array)");
         Indent_Line (File, "renames " & Package_Name & ".Put_Item;");
         Indent_Level := Indent_Level - 1;

         Indent_Line (File, "procedure Get");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, "(File                      : in     " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item                      :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association_Array   : in     Boolean :=");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, Package_Name & ".Default_Named_Association_Array;");
         Indent_Level := Indent_Level - 1;
         Indent_Line (File, " Named_Association_Element : in     Boolean :=");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, Package_Name & ".Default_Named_Association_Element)");
         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "renames " & Package_Name & ".Get;");
         Indent_Level := Indent_Level - 1;

         Indent_Line (File, "procedure Get");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, "(Item                      :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association_Array   : in     Boolean :=");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, Package_Name & ".Default_Named_Association_Array;");
         Indent_Level := Indent_Level - 1;
         Indent_Line (File, " Named_Association_Element : in     Boolean :=");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, Package_Name & ".Default_Named_Association_Element)");
         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "renames " & Package_Name & ".Get;");
         Indent_Level := Indent_Level - 1;

         Indent_Line (File, "procedure Get_Item");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, "(File              : in " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item              :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association : in     Boolean := False)");
         Indent_Line (File, "renames " & Package_Name & ".Get_Item;");
         Indent_Level := Indent_Level - 1;

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
   end Generate_Put_Get_Array_Spec;

   procedure Generate_Put_Get_Derived_Array_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;

      Type_Name : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);

   begin
      if Options.Debug then
         Put_Line ("Generate_Put_Get_Derived_Array_Spec. " & Type_Name);
      end if;

      Indent_Line (File, "procedure Put");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(File                      : in " & Ada_Text_IO & ".File_Type;");
      Indent_Line (File, " Item                      : in " & Type_Name & ";");
      Indent_Line (File, " Single_Line_Array         : in Boolean := False;");
      Indent_Line (File, " Named_Association_Array   : in Boolean := False;");
      Indent_Line (File, " Single_Line_Element       : in Boolean := True;");
      Indent_Line (File, " Named_Association_Element : in Boolean := False);");

      Indent_Level := Indent_Level - 1;
      Indent_Line (File, "procedure Put");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(Item                      : in " & Type_Name & ";");
      Indent_Line (File, " Single_Line_Array         : in Boolean := False;");
      Indent_Line (File, " Named_Association_Array   : in Boolean := False;");
      Indent_Line (File, " Single_Line_Element       : in Boolean := True;");
      Indent_Line (File, " Named_Association_Element : in Boolean := False);");

      Indent_Level := Indent_Level - 1;
      Indent_Line (File, "procedure Put_Item");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(File              : in " & Ada_Text_IO & ".File_Type;");
      Indent_Line (File, " Item              : in " & Type_Name & ";");
      Indent_Line (File, " Single_Line       : in Boolean := False;");
      Indent_Line (File, " Named_Association : in Boolean := False);");

      Indent_Level := Indent_Level - 1;
      New_Line (File);

         Indent_Line (File, "procedure Get");
         Indent_Level := Indent_Level + 1;

         Indent_Line (File, "(File                      : in     " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item                      :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association_Array   : in     Boolean := False;");
         Indent_Line (File, " Named_Association_Element : in     Boolean := False);");
         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "procedure Get");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, "(Item                      :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association_Array   : in     Boolean := False;");
         Indent_Line (File, " Named_Association_Element : in     Boolean := False);");

         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "procedure Get_Item");
         Indent_Level := Indent_Level + 1;

         Indent_Line (File, "(File              : in     " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item              :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association : in     Boolean := False);");
         Indent_Level := Indent_Level - 1;

         New_Line (File);
   end Generate_Put_Get_Derived_Array_Spec;

   procedure Generate_Put_Get_Private_Array_Defaults
     (File      : in Ada.Text_IO.File_Type;
      Label     : in Auto_Text_IO.Lists.Array_Component_Labels_Type;
      Type_Name : in Asis.Element)
   is
      use Auto_Text_IO.Lists;
      Package_Name : constant String := Instantiated_Package_Name (Asis.Aux.Name (Type_Name));
   begin
      case Label is
      when Private_Label =>
         null;

      when Enumeration_Label =>
         Indent_Line (File, "procedure Set_" & Package_Name & "_Default_Width (Width : in Ada.Text_IO.Field);");
         Indent_Line (File, "procedure Set_" & Package_Name & "_Default_Setting (Setting : in Ada.Text_IO.Type_Set);");

      when Float_Label =>
         Indent_Line (File, "procedure Set_" & Package_Name & "_Default_Fore (Fore : in Ada.Text_IO.Field);");
         Indent_Line (File, "procedure Set_" & Package_Name & "_Default_Aft (Aft : in Ada.Text_IO.Field);");
         Indent_Line (File, "procedure Set_" & Package_Name & "_Default_Exp (Exp : in Ada.Text_IO.Field);");

      when Signed_Integer_Label | Modular_Integer_Label =>
         Indent_Line (File, "procedure Set_" & Package_Name & "_Default_Width (Width : Ada.Text_IO.Field);");
         Indent_Line (File, "procedure Set_" & Package_Name & "_Default_Base (Base : Ada.Text_IO.Number_Base);");

      end case;

   end Generate_Put_Get_Private_Array_Defaults;

   procedure Generate_Put_Get_Private_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;

      Type_Name : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);

   begin
      if Options.Debug then
         Put_Line ("Generate_Put_Get_Private_Spec. " & Type_Name);
      end if;

      Indent_Line (File, "procedure Put");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(File                        : in " & Ada_Text_IO & ".File_Type;");

      --  We should use structured comments to get the defaults for
      --  Single_Line etc.
      Indent_Line (File, " Item                        : in " & Type_Name & ";");
      Indent_Line (File, " Single_Line_Record          : in Boolean := True;");
      Indent_Line (File, " Named_Association_Record    : in Boolean := False;");
      Indent_Line (File, " Single_Line_Component       : in Boolean := True;");
      Indent_Line (File, " Named_Association_Component : in Boolean := False);");

      Indent_Level := Indent_Level - 1;
      Indent_Line (File, "procedure Put");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(Item                        : in " & Type_Name & ";");
      Indent_Line (File, " Single_Line_Record          : in Boolean := True;");
      Indent_Line (File, " Named_Association_Record    : in Boolean := False;");
      Indent_Line (File, " Single_Line_Component       : in Boolean := True;");
      Indent_Line (File, " Named_Association_Component : in Boolean := False);");

      Indent_Level := Indent_Level - 1;
      Indent_Line (File, "procedure Put_Item");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(File              : in " & Ada_Text_IO & ".File_Type;");
      Indent_Line (File, " Item              : in " & Type_Name & ";");
      Indent_Line (File, " Single_Line       : in Boolean := False;");
      Indent_Line (File, " Named_Association : in Boolean := False);");

      Indent_Level := Indent_Level - 1;
      New_Line (File);

         Indent_Line (File, "procedure Get");
         Indent_Level := Indent_Level + 1;

         Indent_Line (File, "(File                        : in     " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item                        :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association_Record    : in     Boolean := False;");
         Indent_Line (File, " Named_Association_Component : in     Boolean := False);");
         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "procedure Get");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, "(Item                        :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association_Record    : in     Boolean := False;");
         Indent_Line (File, " Named_Association_Component : in     Boolean := False);");

         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "procedure Get_Item");
         Indent_Level := Indent_Level + 1;

         Indent_Line (File, "(File              : in     " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item              :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association : in     Boolean := False);");
         Indent_Level := Indent_Level - 1;

         New_Line (File);
   end Generate_Put_Get_Private_Spec;

   procedure Generate_Put_Get_Record_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;

      Type_Name : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);

   begin
      if Options.Debug then
         Put_Line ("Generate_Put_Get_Record_Spec. " & Type_Name);
      end if;

      Indent_Line (File, "procedure Put");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(File                        : in " & Ada_Text_IO & ".File_Type;");

      --  We should use structured comments to get the defaults for
      --  Single_Line etc.
      Indent_Line (File, " Item                        : in " & Type_Name & ";");
      Indent_Line (File, " Single_Line_Record          : in Boolean := True;");
      Indent_Line (File, " Named_Association_Record    : in Boolean := False;");
      Indent_Line (File, " Single_Line_Component       : in Boolean := True;");
      Indent_Line (File, " Named_Association_Component : in Boolean := False);");

      Indent_Level := Indent_Level - 1;
      Indent_Line (File, "procedure Put");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(Item                        : in " & Type_Name & ";");
      Indent_Line (File, " Single_Line_Record          : in Boolean := True;");
      Indent_Line (File, " Named_Association_Record    : in Boolean := False;");
      Indent_Line (File, " Single_Line_Component       : in Boolean := True;");
      Indent_Line (File, " Named_Association_Component : in Boolean := False);");

      Indent_Level := Indent_Level - 1;
      Indent_Line (File, "procedure Put_Item");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(File              : in " & Ada_Text_IO & ".File_Type;");
      Indent_Line (File, " Item              : in " & Type_Name & ";");
      Indent_Line (File, " Single_Line       : in Boolean := False;");
      Indent_Line (File, " Named_Association : in Boolean := False);");

      if Type_Descriptor.Record_Tagged then
         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "procedure Put_Components");
         Indent_Level := Indent_Level + 1;

         Indent_Line (File, "(File                        : in " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item                        : in " & Type_Name & ";");
         Indent_Line (File, " Single_Line_Record          : in Boolean := True;");
         Indent_Line (File, " Named_Association_Record    : in Boolean := False;");
         Indent_Line (File, " Single_Line_Component       : in Boolean := True;");
         Indent_Line (File, " Named_Association_Component : in Boolean := False);");

      end if;

      Indent_Level := Indent_Level - 1;
      New_Line (File);

         Indent_Line (File, "procedure Get");
         Indent_Level := Indent_Level + 1;

         Indent_Line (File, "(File                        : in     " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item                        :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association_Record    : in     Boolean := False;");
         Indent_Line (File, " Named_Association_Component : in     Boolean := False);");
         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "procedure Get");
         Indent_Level := Indent_Level + 1;
         Indent_Line (File, "(Item                        :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association_Record    : in     Boolean := False;");
         Indent_Line (File, " Named_Association_Component : in     Boolean := False);");

         Indent_Level := Indent_Level - 1;
         Indent_Line (File, "procedure Get_Item");
         Indent_Level := Indent_Level + 1;

         Indent_Line (File, "(File              : in     " & Ada_Text_IO & ".File_Type;");
         Indent_Line (File, " Item              :    out " & Type_Name & ";");
         Indent_Line (File, " Named_Association : in     Boolean := False);");
         Indent_Level := Indent_Level - 1;

         if Type_Descriptor.Record_Tagged then
            Indent_Line (File, "procedure Get_Components");
            Indent_Level := Indent_Level + 1;

            Indent_Line (File, "(File                        : in     " & Ada_Text_IO & ".File_Type;");
            Indent_Line (File, " Item                        :    out " & Type_Name & ";");
            Indent_Line (File, " Named_Association_Record    : in     Boolean := False;");
            Indent_Line (File, " Named_Association_Component : in     Boolean := False);");
            Indent_Level := Indent_Level - 1;
         end if;

         New_Line (File);
   end Generate_Put_Get_Record_Spec;

   procedure Generate_Put_Get_Scalar_Spec
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type)
   is
      use Ada.Text_IO;

      Type_Name    : constant String := Asis.Aux.Name (Type_Descriptor.Type_Name);
      Package_Name : constant String := Instantiated_Package_Name (Type_Name);

   begin
      if Options.Debug then
         Put_Line ("Generate_Put_Get_Scalar_Spec. " & Type_Name);
      end if;

      Indent (File, "package " & Package_Name & " is new " & Ada_Text_IO);

      case Lists.Scalar_Labels_Type'(Type_Descriptor.Label) is
      when Lists.Enumeration_Label =>
         Put (File, ".Enumeration_IO ");

      when Lists.Float_Label =>
         Put (File, ".Float_IO ");

      when Lists.Fixed_Label =>
         Put (File, ".Fixed_IO ");

      when Lists.Signed_Integer_Label =>
         Put (File, ".Integer_IO ");

      when Lists.Modular_Integer_Label =>
         Put (File, ".Modular_IO ");

      end case;

      Put_Line (File, "(" & Asis.Aux.Name (Type_Descriptor.Type_Name) & ");");

      Do_Rename (File, Type_Name, Package_Name, Type_Descriptor.Label, File_Param => True, Is_Array => False);
      Do_Rename (File, Type_Name, Package_Name, Type_Descriptor.Label, File_Param => False, Is_Array => False);

      New_Line (File);
   end Generate_Put_Get_Scalar_Spec;

end Auto_Text_IO.Generate.Spec;