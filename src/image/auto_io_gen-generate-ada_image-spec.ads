--  Abstract :
--
--  Generate child package spec.
--
--  Copyright (C) 2001 - 2004 Stephen Leake.  All Rights Reserved.
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
private package Auto_Io_Gen.Generate.Ada_Image.Spec is
   pragma Elaborate_Body;  --  Ada.Text_IO, Asis

   procedure Generate_Child_Spec
     (File                : in Ada.Text_IO.File_Type;
      Type_List           : in Lists.Type_Descriptor_Lists.List_Type;
      With_List           : in Lists.Context_Trees.Tree_Type;
      Formal_Package_List : in Lists.Formal_Package_Lists.List_Type;
      Parent_Package_Name : in String;
      Child_Package_Name  : in String;
      Invisible           : in Boolean;
      Is_Generic          : in Boolean);
   --  Generate child package spec. File must be open for write.

end Auto_Io_Gen.Generate.Ada_Image.Spec;
