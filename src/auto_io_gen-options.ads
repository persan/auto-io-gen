--  Abstract :
--
--  Options and parameters set from the command line, and other
--  globals. Operations to process the command line arguments.
--
--  Based on gnatstub, distributed with GNAT 3.12p (http://www.gnat.com)
--
--  Copyright (C) 2001 - 2004, 2006 Stephen Leake.  All Rights Reserved.
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
with Ada.Strings.Wide_Unbounded;
with GNAT.Strings;
with Auto_Io_Gen.Lists;
with Ada.Containers.Vectors;
package Auto_Io_Gen.Options is
   pragma Elaborate_Body; --  Gnat.OS_Lib (in body) is.

   -------------
   --  Misc stuff

   Initialized : Boolean := False;
   --  Set to True by Initialize, if initialization is successful

   Indent : aliased integer := 3;
   --  Indentation per level in the outputted source

   ---------------
   --  Command line options (alphabetical).

   Error_On_Warn : aliased Boolean := True;
   --  Return error status for warnings. We now no longer support
   --  warnings; this will be cleaned up later.


   Debug : aliased Boolean := False;
   --  If True, generate debug information.

   Overwrite_Child : aliased Boolean := False;
   --  Should an existing .Text_IO child be overwritten

   Quiet : aliased Boolean := False;
   --  If True, do not generate a message when the child package has
   --  successfully been created

   Verbose : aliased Boolean := False;
   --  If True, generate version info, including ASIS/GNAT version

   ------------------------------
   --  File and Directory names

   Package_Separator : Character := '.';
   --  '.' for Ada_83 False, '_' for True.

   File_Package_Separator : constant Character := '-';

   Spec_File_Extension : constant String := "ads";
   Body_File_Extension : constant String := "adb";

   --  Determined by compiler conventions.

   Package_File_Name : GNAT.Strings.String_Access;
   --  The name of the source file that contains the processed unit.
   --  The file name may or may not contain the path information.

   Root_File_Name  : GNAT.Strings.String_Access;
   --  Package_File_Name without directory information or file extension

   Report_File_Name : GNAT.Strings.String_Access;
   --  File name to use in reporting errors. This is the string
   --  returned by the ASIS query Text_Name. It can be different from
   --  Package_File_Name if a preprocessor is used (eg gnatprep).

   Destination_Dir : GNAT.Strings.String_Access;
   --  Output files (Text IO child packages) are written here.

   Asis_Init_String : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   --  Parameter for Asis.Ada_Environments.Associate.

   Generate_JSON : aliased Boolean := False;
   Generate_Image :  aliased Boolean := False;
   Generate_Text_Io : aliased Boolean := False;
   Project_File : aliased GNAT.Strings.String_Access;
   ------------
   --  Procedures

   procedure Brief_Help;
   --  Prints brief help information to stdout.

   procedure Read_Command_Line;
   --  Reads and checks the command line parameters and initializes
   --  the options.

   procedure Initialize;
   --  Checks the existence of the files to be processed and
   --  applicability of the options with these files. Tries to create
   --  the tree file, if necessary. If everything is OK, sets the
   --  global Is_Initialized variable True. This procedure does not
   --  use anything from ASIS

   procedure Clean_Up;
   --  Remove the tree file, if needed.


   type Create_Text_IO_Child_proc is access procedure
     (Type_List           : in Auto_Io_Gen.Lists.Type_Descriptor_Lists.List_Type;
      Spec_With_List      : in Auto_Io_Gen.Lists.Context_Trees.Tree_Type;
      Body_With_List      : in Auto_Io_Gen.Lists.Context_Trees.Tree_Type;
      Formal_Package_List : in Lists.Formal_Package_Lists.List_Type;
      Parent_Package_Name : in String;
      Needs_Body          : in Boolean;
      Needs_Text_IO_Utils : in Boolean;
      Invisible           : in Boolean;
      Is_Generic          : in Boolean);
   procedure Register (Option, Language_Name : String; Generator : Create_Text_IO_Child_Proc);

   type Language_Description is record
      Enabled : aliased Boolean := False;
      Generator : Create_Text_IO_Child_Proc;
   end record;
   type Language_Description_Access is access Language_Description;
   package Language_Description_Vectors is new Ada.Containers.Vectors (Natural, Language_Description_Access, "=");
   Languages : Language_Description_Vectors.Vector;
end Auto_Io_Gen.Options;
