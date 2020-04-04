--  Abstract :
--
--  Root of automatic Text_IO package generator.
--
--  Design :
--
--  There are two phases; first traverse the ASIS tree for the parent
--  package, collecting information in a list of type descriptors.
--  Then traverse that list, generating the Text_IO child packages
--  (public and private).
--
--  Using the internal type list separates the two tasks, making the
--  result easier to understand. It is also required for accumulating
--  the list of other Text_IO packages that must be 'with'ed.
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

with Asis;
with Ada.Text_IO;
package Auto_Io_Gen is
--   pragma Elaborate_Body; --  Asis is, but this is circular with Options.

   Not_Supported   : exception; --  Some Ada type is not supported.
   Parameter_Error : exception; --  Bad user parameter.

   function Version return String;
   function Program_Name return String;

   type Child_Name_Label_Type is (Expression, Generic_Formal, Generic_Unit, With_Clause);
   --  How text_io child package name is used; in an expression with
   --  Put or Get, as a generic formal parameter, as a generic unit in
   --  a generic formal package declaration, or in a with clause. This
   --  affects the naming convention used by Text_IO_Child_Name.

   function Text_IO_Child_Name
     (Package_Declaration : in Asis.Element;
      Label               : in Child_Name_Label_Type := Expression)
      return String;
   --  Return appropriate Text_IO child package name.
   --  Package_Declaration must be A_Package_Declaration,
   --  A_Generic_Package_Declaration, or A_Formal_Package_Declaration.
   --
   --  Handles special cases for predefined Ada packages; for
   --  example, the name returned for Interfaces.C.Text_IO is
   --  Interfaces_C_Text_IO.
   --
   --  Raises Programmer_Error if Element is not an appropriate type.

   function Private_Text_IO_Child_Name (Package_Declaration : in Asis.Element) return String;
   --  Similar to Text_IO_Child_Name, but return the name for the
   --  private Text_IO child, for use in a context clause. We always
   --  do "with, use" for the private children, and they never appear
   --  as a generic formal parameter, so no "Label" parameter is
   --  needed.

   Not_Implemented : exception;
   Not_Found       : exception;

   procedure Set_Indent (File : in Ada.Text_IO.File_Type);
   Indent_Level : Ada.Text_IO.Positive_Count := 1;
   --  1 is no indentation.

   function Ada2file (Folder, Name , Suffix : String) return String;
   procedure Traceback;
end Auto_Io_Gen;
