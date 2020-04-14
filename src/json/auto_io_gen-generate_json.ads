--  Abstract :
--
--  Write Text_IO functions for types in the list built by Build.
--
--  Copyright (C) 2001, 2002, 2003 Stephen Leake.  All Rights Reserved.
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

with Auto_Io_Gen.Lists;
package Auto_Io_Gen.Generate_JSON is
--   pragma Elaborate_Body; -- Body depends on Text_IO, but this is circular due to child packages.

   procedure Create_Text_IO_Child
     (Type_List           : in Auto_Io_Gen.Lists.Type_Descriptor_Lists.List_Type;
      Spec_With_List      : in Auto_Io_Gen.Lists.Context_Trees.Tree_Type;
      Body_With_List      : in Auto_Io_Gen.Lists.Context_Trees.Tree_Type;
      Formal_Package_List : in Lists.Formal_Package_Lists.List_Type;
      Parent_Package_Name : in String;
      Needs_Body          : in Boolean;
      Needs_Text_IO_Utils : in Boolean;
      Invisible           : in Boolean;
      Is_Generic          : in Boolean);
   --  Generates the Text_IO child package.

private

   --  Visible for child packages.


   function Ada_Text_IO return String;
   --  Return "Ada.Text_IO".
   --  TBC: Ada_83 mode was removed - candidate for removal.

   function Component_Type_Name
      (Type_Element         : in Asis.Element;
       Type_Package_Element : in Asis.Element)
      return String;
   --  Type_Element must be from Type_Descriptor_Type.Array_Component
   --  or Component_Type.Type_Name; Type_Package_Element must be from
   --  corresponding package element. Return appropriate type name;
   --  just the type identifier if package name is nil and element
   --  name is An_Identifier, otherwise package name.type name.

   procedure Indent (File : in Ada.Text_IO.File_Type; Text : in String);
   --  Do Set_Indent (File), then Put (File, Text).

   procedure Indent_Line (File : in Ada.Text_IO.File_Type; Text : in String);
   --  Do Set_Indent (File), then Put_Line (File, Text).

   procedure Instantiate_Generic_Array_Text_IO
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Lists.Type_Descriptor_Type);
   --  Write code to File that instantiates the appropriate package
   --  from SAL.Gen_Array_Text_IO for Type_Descriptor.

   function Instantiated_Package_Name (Type_Name : in String) return String;
   --  Return a name for an instantiation of a text_io package for Type_Name.

   function Root_Type_Name (Type_Name : in String) return String;
   --  Return Type_Name without trailing _Type, if any.

end Auto_Io_Gen.Generate_JSON;
