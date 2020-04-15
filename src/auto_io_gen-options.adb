--  Abstract :
--
--  Options and parameters set from the command line, and other globals.
--
--  Based on gnatstub, distributed with GNAT 3.12p (http://www.gnat.com)
--
--  Copyright (C) 2001 - 2006 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with GNAT.OS_Lib;
with Gnat.Exception_Traces;
with GNAT.Traceback.Symbolic;
with GNAT.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;
with GNAT.Source_Info;
with Ada.Text_IO;

package body Auto_Io_Gen.Options is

   use Ada.Strings.Unbounded;
   use Ada.Directories;
   use Ada.Text_IO;
   use GNAT.Strings;

   Create_Output_Folders : aliased Boolean := False;
   Trace_Exceptions      : aliased Boolean := False;
   I_Options             : Ada.Strings.Unbounded.Unbounded_String;
   --  Temporary storage for "-Idir" options; read by Scan_Arg, used
   --  by Check_Parameters.

   Project_Arg           : aliased GNAT.Strings.String_Access;
   --  "-Pfile" option; read by Scan_Arg, used by Create_Tree.

   Gnatmake_User_Args    : Ada.Strings.Unbounded.Unbounded_String;
   --  from -g option

   Delete_Tree           : aliased Boolean := True;
   --  Should the tree file be deleted when processing is complete

   Overwrite_Tree        : aliased Boolean := True;
   --  Should an existing tree file be overwritten

   Parser_Was_Set_Up     : Boolean := False;
   -- Auxiliary flag for avoiding double execution of Setup_Parser

   Reuse_Tree            : aliased Boolean := False;
   --  Should an existing tree file be reused

   Search_Dir_List       : GNAT.OS_Lib.Argument_List_Access;
   --     Search_Dir_Count : Natural := 0;
   --  -I options from the command line transformed into the form
   --  appropriate for calling gcc to create the tree file

   -- Tree_File             : File_Type;
   --  The input tree file

   Tree_Exists           : Boolean := False;
   --  True if the tree file has been created or has been found as
   --  existing during the initialization

   Tree_Name             : GNAT.Strings.String_Access;

   -------------
   --  Local operation declarations (alphabetical)

   procedure Check_Parameters;
   --  Checks that options and files existing in the file system fit
   --  each other. If the check fails, generates a diagnostic message
   --  and raises Parameter_Error

   procedure Create_Tree;
   --  Creates a tree file or checks if the tree file already exists,
   --  depending on options


   --------------
   --  Operation Bodies (alphabetical)

   Command_Line_Parser : GNAT.Command_Line.Command_Line_Configuration;

   procedure Register (Option, Language_Name : String;
                       Generator             : Create_Text_IO_Child_Proc;
                       Std_Names             : Standard_IO_Name_Proc) is
      Temp : constant Language_Description_Access := new Language_Description;
      use GNAT.Command_Line;
   begin
      if Verbose then
         Put_Line (GNAT.Source_Info.Enclosing_Entity & "(""" & Option & """, """ & Language_Name & """)");
      end if;
      Temp.Generator := Generator;
      Temp.Standard  := Std_Names;
      Languages.Append (Temp);
      Define_Switch (Command_Line_Parser, Temp.Enabled'Access, "", "--" & Option, "Generate " & Language_Name & ".");
   end;


   procedure Brief_Help
   is
   begin
      Put_Line (Auto_Io_Gen.Version);
      Put_Line ("Create a Text_IO child package for a package,");
      Put_Line ("containing Get and Put routines for all types in the package.");
      Put_Line ("");
      Put_Line ("Usage: Auto_Io_Gen [options] filename");
      Put_Line ("");
      Put_Line ("  filename   source file");
      Put_Line ("  directory  directory to place Text_IO child (default is '.')");
      Put_Line ("     relative to source directory, or absolute");
      Put_Line ("");
      Put_Line ("options:");
      Put_Line ("");
      --        Put_Line ("  -g     additional gnatmake args");
      --        Put_Line ("  -Idir  source search dir, has the same meaning as for gnatmake");
      --        Put_Line ("  -I-    same as gnatmake");

      New_Line;
      Put_Line ("Comment annotations:");
      Put_Line ("  --  Auto_Io_Gen : ignore");
      Put_Line ("  --  Auto_Io_Gen : separate");
      Put_Line ("See manual for more info.");
   end Brief_Help;

   procedure Setup_Parser is
      use GNAT.Command_Line;
   begin
      if Parser_Was_Set_Up then
         return;
      end if;

      Define_Switch (Command_Line_Parser, Debug'Access,                 "-d",  "--debug",   "Debug output.");
      Define_Switch (Command_Line_Parser, Verbose'Access,               "-v",  "--verbose", "Verbose output.");
      Define_Switch (Command_Line_Parser, Overwrite_Child'Access,       "-f",  "",          "Replace existing files.");
      Define_Switch (Command_Line_Parser, Quiet'Access,                 "-q",  "",          "Quiet mode.");
      Define_Switch (Command_Line_Parser, Create_Output_Folders'Access, "-o",  "",          "Set Output Directory.");
      Define_Switch (Command_Line_Parser, Project_Arg'Access,           "-P",  "",          "Use project file.");
      Define_Switch (Command_Line_Parser, Overwrite_Child'Access,       "-t",  "",          "Overwrite the existing tree file.");
      Define_Switch (Command_Line_Parser, Delete_Tree'Access,           "-k",  "",          "Do not remove the GNAT tree file", Value => False);
      Define_Switch (Command_Line_Parser, Reuse_Tree'Access,            "-r",  "",          "Reuse the GNAT tree file instead of re-creating it (-r also implies -k).");
      Define_Switch (Command_Line_Parser, Indent'Access,                "-i=", "",          "(N in 1 .. 9) number of spaces used for indentation in generated code.", Initial => Indent);
      Define_Switch (Command_Line_Parser, Trace_Exceptions'Access,      "-T",  "",          "Trace all exceptions.");
      Define_Switch (Command_Line_Parser, Create_Output_Folders'Access, "-p",  "",          "Create Output Folders.");
      Define_Switch (Command_Line_Parser, Print_Version'Access,         "",    "--version", "Print version.");
      Define_Switch (Command_Line_Parser, Print_Help'Access,            "-h",  "--help",    "Print this text.");

      Parser_Was_Set_Up := True;
   end;

   procedure Check_Parameters
   is


      Root_Directory  : GNAT.Strings.String_Access;

   begin


      --  Check if the source file is set
      if Package_File_Name = null then
         Put_Line ("Auto_Io_Gen: missing source file parameter");
         raise Parameter_Error;
      end if;

      --  Check if the package file really exists:
      if not Exists (Package_File_Name.all) then
         Put_Line ("Auto_Io_Gen: cannot find " & Package_File_Name.all);
         raise Parameter_Error;
      end if;

      --  Compute some names before continuing checking:

      Root_File_Name := new String'(Simple_Name (Package_File_Name.all));
      Root_Directory := new String'(Containing_Directory (Package_File_Name.all));

      if Destination_Dir = null then
         Destination_Dir := Root_Directory;
      else
         if Full_Name (Destination_Dir.all) = Destination_Dir.all then
            null;
         else
            Destination_Dir := new String'(Ada.Directories.Current_Directory & GNAT.OS_Lib.Directory_Separator & Destination_Dir.all);
         end if;
      end if;

      --  Check if the destination directory exists:


      if not Exists (Destination_Dir.all) then
         if Create_Output_Folders then
            if Verbose then
               Put_Line ("mkdir " & Destination_Dir.all);
            end if;
            Ada.Directories.Create_Path (Destination_Dir.all);
         else
            Put_Line ("Auto_Io_Gen: " & Destination_Dir.all & " does not exist");
            raise Parameter_Error;
         end if;
      end if;

      --  Checking to see whether the child exists is done in generate,
      --  after we know what the name is (generic or not).

      --  Check the tree file. It is created in the current directory, where gcc is run.
      Tree_Name := new String'(Base_Name (Root_File_Name.all) & ".adt");

      if Exists (Tree_Name.all) then
         Tree_Exists := True;
         if not (Reuse_Tree or Overwrite_Tree) then
            Put_Line ("Auto_Io_Gen: " & Tree_Name.all & " already exists");
            Put_Line ("              use -r or -t to reuse or to overwrite it");
            raise Parameter_Error;
         end if;
      else
         if Reuse_Tree then
            Put_Line ("Auto_Io_Gen: cannot find " & Tree_Name.all & " (-r is set)");
            raise Parameter_Error;
         end if;
      end if;

      if Reuse_Tree then
         Delete_Tree    := False;
         Overwrite_Tree := False;
      end if;

      Asis_Init_String := Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String
        ("-C1 " & Ada.Characters.Handling.To_Wide_String (Tree_Name.all));

      --  Convert '-I' options from a string into argument list
      if Length (I_Options) = 0 then
         Search_Dir_List := new GNAT.OS_Lib.Argument_List (1 .. 0);
      else
         Trim (I_Options, Ada.Strings.Both);
         Search_Dir_List := GNAT.OS_Lib.Argument_String_To_List (To_String (I_Options));
      end if;

   end Check_Parameters;

   procedure Clean_Up
   is

      --      Ali_Name : constant String  := Base_Name (Root_File_Name.all) & ".ali";
   begin
      if Delete_Tree and then Tree_Exists then
         if Tree_Name /= null then
            Delete_File (Tree_Name.all);
         end if;
         --       Open (Tree_File, In_File, Ali_Name);

      end if;
   end Clean_Up;


   procedure Create_Tree
   is
      use GNAT;
      Success                : Boolean;
      Temp_Package_File_Name : constant String := Package_File_Name.all;

      Command             : Gnat.Strings.String_Access := OS_Lib.Locate_Exec_On_Path ("gprbuild");
      Command_Args_List   : String_List_Access;
   begin
      if Tree_Exists and Reuse_Tree then
         return;
      end if;

      if Verbose then
         Put_Line ("Creating tree " & Tree_Name.all);
      end if;

      if Project_Arg /= null and then Project_Arg.all'Length /= 0 then
         Command := OS_Lib.Locate_Exec_On_Path ("gprbuild");
         declare
            Gnatmake_Args_String : constant String :=
                                     "-XAUTO_IO_GEN=True " &
                                     To_String (Gnatmake_User_Args) & " " &
                                     Temp_Package_File_Name & " " &
                                     "-P" & Project_Arg.all;
         begin
            Command_Args_List := OS_Lib.Argument_String_To_List (Gnatmake_Args_String);
         end;
      else
         Command := OS_Lib.Locate_Exec_On_Path ("asis-gcc");
         if Command = null then
            Command := OS_Lib.Locate_Exec_On_Path ("gcc");
         end if;


         declare
            Gcc_Args_String : constant String := "-c -gnatc -gnatt " & Temp_Package_File_Name;
         begin
            Command_Args_List := OS_Lib.Argument_String_To_List (Gcc_Args_String);
         end;
      end if;

      OS_Lib.Spawn (Command.all, Command_Args_List.all & Search_Dir_List.all, Success);

      if not Success then
         Put_Line ("Auto_Io_Gen: cannot create the tree file for "  & Temp_Package_File_Name);
         if Debug or Verbose then
            Put (Command.all & " ");
            for I in Command_Args_List.all'Range loop
               Put (Command_Args_List.all (I).all);
               Put (" ");
            end loop;
            New_Line;
            for I in Search_Dir_List.all'Range loop
               Put_Line (Search_Dir_List.all (I).all);
            end loop;
         end if;
         Free (Command);
         Free (Command_Args_List);
         raise Parameter_Error;
      else
         Free (Command);
         Free (Command_Args_List);
         Tree_Exists := True;
      end if;

   end Create_Tree;

   procedure Read_Command_Line
   is
   begin
      Setup_Parser;
      GNAT.Command_Line.Getopt (Command_Line_Parser);
      if Print_Help then
         GNAT.Command_Line.Display_Help (Command_Line_Parser);
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      if Print_Version then
         Put_Line (Version);
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      if Debug then
         Verbose := True;
      end if;
      if Trace_Exceptions then
         Gnat.Exception_Traces.Set_Trace_Decorator (Gnat.Traceback.Symbolic.Symbolic_Traceback'Access);
         Gnat.Exception_Traces.Trace_On (Gnat.Exception_Traces.Every_Raise);
      end if;
      loop
         declare
            Argument : constant String := GNAT.Command_Line.Get_Argument (True);
         begin
            exit when Argument'Length = 0;
            if Package_File_Name = null then
               Package_File_Name := new String'(Argument);
            elsif Destination_Dir = null then
               Destination_Dir := new String'(Argument);
            else
               Put      ("Auto_Io_Gen: only one file name and at most one ");
               Put_Line ("destination directory are allowed");
               raise Parameter_Error;
            end if;
         end;
      end loop;

   exception
      when Parameter_Error =>
         Brief_Help;
         Initialized := False;
         --  nothing else to do!
      when others =>
         Initialized := False;
         raise;
   end Read_Command_Line;




   procedure Initialize
   is begin
      Check_Parameters;
      Create_Tree;

      Initialized := True;
   exception
      when Parameter_Error =>
         --  Message already output.
         Initialized := False;

      when E : others =>
         Put_Line
           ("Auto_Io_Gen: unhandled exception in Initialize: " &
              Ada.Exceptions.Exception_Name (E) &
              ": " &
              Ada.Exceptions.Exception_Message (E));

         Initialized := False;
   end Initialize;
begin
   Setup_Parser;
end Auto_Io_Gen.Options;
