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
with Ada.Command_Line;
with Ada.Exceptions;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Gnat.Exception_Traces;
with GNAT.Traceback.Symbolic;
package body Auto_Text_IO.Options is

   use Ada.Text_IO;

   I_Options : Ada.Strings.Unbounded.Unbounded_String;
   --  Temporary storage for "-Idir" options; read by Scan_Arg, used
   --  by Check_Parameters.

   Project_Arg : Ada.Strings.Unbounded.Unbounded_String;
   --  "-Pfile" option; read by Scan_Arg, used by Create_Tree.

   Gnatmake_User_Args : Ada.Strings.Unbounded.Unbounded_String;
   --  from -g option

   Delete_Tree : Boolean := True;
   --  Should the tree file be deleted when processing is complete

   Overwrite_Tree : Boolean := True;
   --  Should an existing tree file be overwritten

   Reuse_Tree : Boolean := False;
   --  Should an existing tree file be reused

   Search_Dir_List  : GNAT.OS_Lib.Argument_List_Access;
   Search_Dir_Count : Natural := 0;
   --  -I options from the command line transformed into the form
   --  appropriate for calling gcc to create the tree file

   Tree_File : File_Type;
   --  The input tree file

   Tree_Exists : Boolean := False;
   --  True if the tree file has been created or has been found as
   --  existing during the initialization

   Tree_Name : Ada.Strings.Unbounded.Unbounded_String;

   -------------
   --  Local operation declarations (alphabetical)

   procedure Check_Parameters;
   --  Checks that options and files existing in the file system fit
   --  each other. If the check fails, generates a diagnostic message
   --  and raises Parameter_Error

   procedure Create_Tree;
   --  Creates a tree file or checks if the tree file already exists,
   --  depending on options

   function Get_Natural (Val : in String) return Natural;
   --  Computes a Natural value from Val.
   --
   --  Raises Parameter_Error if Val can not be considered as a string
   --  image of a natural number.
   --
   --  Raises Constraint_Error if Val is an empty string.

   procedure Scan_Arg (Arg : in String);
   --  Process a command line argument Arg. If the argument is illegal,
   --  generates a diagnostic message and raises Parameter_Error

   procedure Unknown_Option (Arg : in String);
   --  Output an error message of the form "auto_text_io : unknown
   --  option <Arg>" and raise Parameter_Error

   --------------
   --  Operation Bodies (alphabetical)

   procedure Brief_Help
   is
   begin
      Put_Line (Auto_Text_IO.Version);
      Put_Line ("Create a Text_IO child package for a package,");
      Put_Line ("containing Get and Put routines for all types in the package.");
      Put_Line ("");
      Put_Line ("Usage: auto_text_io [options] filename [directory]");
      Put_Line ("");
      Put_Line ("  filename   source file");
      Put_Line ("  directory  directory to place Text_IO child (default is '.')");
      Put_Line ("     relative to source directory, or absolute");
      Put_Line ("");
      Put_Line ("options:");
      Put_Line ("");
      Put_Line ("  -12    allow Ada 12 syntax in source file");
      Put_Line ("  -d     output debug information");
      Put_Line ("  -f     replace an existing Text_IO child (if any)");
      Put_Line ("  -g     additional gnatmake args");
      Put_Line ("  -Idir  source search dir, has the same meaning as for gnatmake");
      Put_Line ("  -I-    same as gnatmake");
      Put_Line ("  -iN    (N in 1 .. 9) number of spaces used for identation in generated code");
      Put_Line ("  -k     do not remove the GNAT tree file");
      Put_Line ("  -q     quiet mode - do not output some messages");
      Put_Line ("  -Pfile specify GNAT project file for running gnatmake.");
      Put_Line ("  -r     reuse the GNAT tree file instead of re-creating it");
      Put_Line ("         (-r also implies -k)");
      Put_Line ("  -t     overwrite the existing tree file");
      Put_Line ("  -v     verbose; output progress information");
      Put_Line ("  -?     output this help information");
      New_Line;
      Put_Line ("Comment annotations:");
      Put_Line ("  --  Auto_Text_IO : ignore");
      Put_Line ("  --  Auto_Text_IO : separate");
      Put_Line ("See manual for more info.");
   end Brief_Help;

   procedure Check_Parameters
   is
      use GNAT.OS_Lib, Ada.Strings.Unbounded;

      Temp_Package_File_Name : constant String := To_String (Package_File_Name);

      Root_Name_First : Integer; --  First char of Root_File_Name in Temp_Package_File_Name
      Root_Name_Last  : Integer; --  Last char ""
      Root_Directory  : Unbounded_String;

   begin

         File_Package_Separator := '-';

      Spec_File_Extension := To_Unbounded_String (".ads");
      Body_File_Extension := To_Unbounded_String (".adb");

      --  Check if the source file is set
      if Temp_Package_File_Name'Length = 0 then
         Put_Line ("auto_text_io: missing source file parameter");
         raise Parameter_Error;
      end if;

      --  Check if the package file really exists:
      if not Is_Regular_File (Temp_Package_File_Name) then
         Put_Line ("auto_text_io: cannot find " & Temp_Package_File_Name);
         raise Parameter_Error;
      end if;

      --  Compute some names before continuing checking:
      Root_Name_First := Temp_Package_File_Name'First;
      Root_Name_Last  := Temp_Package_File_Name'Last;

      for I in reverse Temp_Package_File_Name'First .. Temp_Package_File_Name'Last loop
         if Temp_Package_File_Name (I) = '.' then
            --  Don't set Root_Name_Last for leading "../foo"
            if Root_Name_First = Temp_Package_File_Name'First then
               Root_Name_Last := I - 1;
            end if;
            --  Allow either '/' or '\', since gnatmake does
         elsif Temp_Package_File_Name (I) = '/' or else Temp_Package_File_Name (I) = '\' then
            Root_Name_First := I + 1;
            exit;
         end if;
      end loop;

      Root_File_Name := To_Unbounded_String (Temp_Package_File_Name (Root_Name_First .. Root_Name_Last));
      Root_Directory := To_Unbounded_String
        (Temp_Package_File_Name (Temp_Package_File_Name'First .. Root_Name_First - 1));

      if Length (Destination_Dir) = 0 then
         Destination_Dir := Root_Directory;
      else
         if Slice (Destination_Dir, 1, 1) = "/" or else --  Unix
           Slice (Destination_Dir, 2, 2) = ":" --  Win32
         then
            --  Absolute path; done
            null;
         else
            Destination_Dir := Root_Directory & Destination_Dir;
         end if;
      end if;

      --  Check if the destination directory exists:

      declare
         Last_Char : constant Character := Element (Destination_Dir, Length (Destination_Dir));
      begin
         if Last_Char /= '/' and then
           Last_Char /= '\'
         then
            Destination_Dir := Destination_Dir & '/';
         end if;
      end;

      if not Is_Directory (To_String (Destination_Dir)) then
         Put_Line ("auto_text_io: " & To_String (Destination_Dir) & " does not exist");
         raise Parameter_Error;
      end if;

      --  Checking to see whether the child exists is done in generate,
      --  after we know what the name is (generic or not).

      --  Check the tree file. It is created in the current directory, where gcc is run.
      Tree_Name := Root_File_Name & ".adt";

      if Is_Regular_File (To_String (Tree_Name)) then
         Tree_Exists := True;
         if not (Reuse_Tree or Overwrite_Tree) then
            Put_Line ("auto_text_io: " & To_String (Tree_Name) & " already exists");
            Put_Line ("              use -r or -t to reuse or to overwrite it");
            raise Parameter_Error;
         end if;
      else
         if Reuse_Tree then
            Put_Line ("auto_text_io: cannot find " & To_String (Tree_Name) & " (-r is set)");
            raise Parameter_Error;
         end if;
      end if;

      if Reuse_Tree then
         Delete_Tree    := False;
         Overwrite_Tree := False;
      end if;

      Asis_Init_String := Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String
        ("-C1 " & Ada.Characters.Handling.To_Wide_String (To_String (Tree_Name)));

      --  Convert '-I' options from a string into argument list
      if Length (I_Options) = 0 then
         Search_Dir_List := new Argument_List (1 .. 0);
      else
         Trim (I_Options, Ada.Strings.Both);
         Search_Dir_List := Argument_String_To_List (To_String (I_Options));
      end if;

   end Check_Parameters;

   procedure Clean_Up
   is
      use Ada.Strings.Unbounded;

      Ali_Name : constant String := To_String (Root_File_Name) & ".ali";
   begin
      if Delete_Tree and then Tree_Exists then

         --  Delete the tree file itself
         Open (Tree_File, In_File, To_String (Tree_Name));
         Delete (Tree_File);

         Open (Tree_File, In_File, Ali_Name);
         Delete (Tree_File);

      end if;
   end Clean_Up;

   procedure Create_Tree
   is
      use Ada.Strings.Unbounded;
      use GNAT.OS_Lib;
      Success                : Boolean;
      Temp_Package_File_Name : constant String := To_String (Package_File_Name);
      Gnat_12_Arg          : constant String := "-gnat12 ";

      Command : GNAT.OS_Lib.String_Access := Locate_Exec_On_Path ("gnatmake");
      Command_Args_List   : String_List_Access;
   begin
      if Tree_Exists and Reuse_Tree then
         return;
      end if;

      if Verbose then
         Put_Line ("Creating tree " & To_String (Tree_Name));
      end if;

      if Length (Project_Arg) > 0 then
         Command := Locate_Exec_On_Path ("gnatmake");
         declare
            Gnatmake_Args_String : constant String :=
              "-c -gnatc -gnatt " &
              To_String (Gnatmake_User_Args) & " " &
              Temp_Package_File_Name & " " &
              To_String (Project_Arg);
         begin
            if Ada_12 then
               Command_Args_List := Argument_String_To_List (Gnat_12_Arg & Gnatmake_Args_String);
            else
               Command_Args_List := Argument_String_To_List (Gnatmake_Args_String);
            end if;
         end;
      else
         Command := Locate_Exec_On_Path ("gcc");
         declare
            Gcc_Args_String : constant String := "-c -gnatc -gnatt " & Temp_Package_File_Name;
         begin
            if Ada_12 then
               Command_Args_List := Argument_String_To_List (Gnat_12_Arg & Gcc_Args_String);
            else
               Command_Args_List := Argument_String_To_List (Gcc_Args_String);
            end if;
         end;
      end if;

      Spawn (Command.all, Command_Args_List.all & Search_Dir_List.all, Success);

      if not Success then
         Put_Line ("auto_text_io: cannot create the tree file for "  & Temp_Package_File_Name);
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

   function Get_Natural (Val : in String) return Natural
   is
      Result : Natural := 0;
   begin
      for I in Val'Range loop

         if Val (I) not in '0' .. '9' then
            Put_Line ("auto_text_io: invalid switch integer parameter " & Val);
            raise Parameter_Error;
         else
            Result := Result * 10 + Character'Pos (Val (I)) - Character'Pos ('0');
         end if;

      end loop;

      return Result;

   end Get_Natural;

   procedure Read_Command_Line
   is
      use Ada.Command_Line;
      Next_Arg : Positive := 1;
   begin
      if Argument_Count = 0 then
         Brief_Help;
         --  Is_Initialized remains False here!
      else
         --  Scan the command line parameters
         while Next_Arg <= Argument_Count loop
            Scan_Arg (Argument (Next_Arg));
            Next_Arg := Next_Arg + 1;
         end loop;

      end if;

   exception
   when Parameter_Error =>
      Brief_Help;
      Initialized := False;
      --  nothing else to do!
   when others =>
      Initialized := False;
      raise;
   end Read_Command_Line;

   procedure Scan_Arg (Arg : in String)
   is
      use Ada.Strings.Unbounded;

      First : constant Integer := Arg'First;
      Len   : constant Natural := Arg'Length;
   begin
      if Len = 0 then
         return;
      end if;

      if Arg (First) = '-' then

         if Len >= 2 then

            case Arg (First + 1) is
            when '1' =>
               if Arg = "-12" then
                  Ada_12 := True;
               else
                  Unknown_Option (Arg);
               end if;


            when 'd' =>
               if Arg = "-d" then
                  Debug   := True;
                  Verbose := True;
               else
                  Unknown_Option (Arg);
               end if;

            when 'f' =>

               if Arg = "-f" then
                  Overwrite_Child := True;
               else
                  Unknown_Option (Arg);
               end if;

            when 'g' =>
               Gnatmake_User_Args := Gnatmake_User_Args & " " & To_Unbounded_String (Arg (Arg'First + 2 .. Arg'Last));

            when 'i' =>

               if Len = 2 then
                  Put_Line ("auto_text_io: missing value for -i parameter");
                  raise Parameter_Error;
               end if;

               Indent := Positive_Count (Get_Natural (Arg (First + 2 .. Arg'Last)));

            when 'I' =>
               Append (I_Options, " " & Arg);
               Search_Dir_Count := Search_Dir_Count + 1;

            when 'k' =>

               if Arg = "-k" then
                  Delete_Tree := False;
               else
                  Unknown_Option (Arg);
               end if;

            when 'P' =>
               declare
                  File_Name : constant String := Arg (Arg'First + 2 .. Arg'Last);
               begin
                  if GNAT.OS_Lib.Is_Readable_File (File_Name) then
                     Project_Arg := To_Unbounded_String ("-P" & File_Name);
                  else
                     Put_Line ("auto_text_io: " & File_Name & " is not a readable project file");
                  end if;
               end;

            when 'q' =>

               if Arg = "-q" then
                  Quiet := True;
               else
                  Unknown_Option (Arg);
               end if;

            when 'r' =>

               if Arg = "-r" then
                  Reuse_Tree := True;
               else
                  Unknown_Option (Arg);
               end if;

            when 's' =>
               declare
                  use GNAT.Directory_Operations;
                  File   : File_Type;
                  Buffer : String (1 .. 1024);
                  Last   : Natural;
               begin
                  Open (File, in_file, Arg (Arg'First + 2 .. Arg'Last));
                  while not End_Of_File (File) loop
                     Get_Line (File, Buffer, Last);
                     if GNAT.OS_Lib.Is_Directory (Buffer (Buffer'First .. Last)) then
                        Append (I_Options,
                               " -I" & Format_Pathname (Buffer (Buffer'First .. Last)));
                     end if;
                  end loop;
                  Close (File);
               end;

            when 'T' =>
               if Arg = "-T" then
                  Gnat.Exception_Traces.Set_Trace_Decorator (Gnat.Traceback.Symbolic.Symbolic_Traceback'Access);
                  Gnat.Exception_Traces.Trace_On (Gnat.Exception_Traces.Every_Raise);
               else
                  Unknown_Option (Arg);
               end if;

            when 't' =>

               if Arg = "-t" then
                  Overwrite_Tree := True;
               else
                  Unknown_Option (Arg);
               end if;

            when 'v' =>
               if Arg = "-v" then
                  Verbose := True;
               else
                  Unknown_Option (Arg);
               end if;

            when others =>
               Unknown_Option (Arg);
            end case;
         else
            Unknown_Option (Arg);
         end if;

      else

         --  Either a file name or a destination directory
         if Length (Package_File_Name) = 0 then
            Package_File_Name := To_Unbounded_String (Arg);
         elsif Length (Destination_Dir) = 0 then
            Destination_Dir := To_Unbounded_String (Arg);
         else
            Put      ("auto_text_io: only one file name and at most one ");
            Put_Line ("destination directory are allowed");
            raise Parameter_Error;
         end if;

      end if;

   end Scan_Arg;

   procedure Unknown_Option (Arg : in String) is
   begin
      Put_Line ("auto_text_io: unknown option " & Arg);
      raise Parameter_Error;
   end Unknown_Option;

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
        ("auto_text_io: unhandled exception in Initialize: " &
           Ada.Exceptions.Exception_Name (E) &
           ": " &
           Ada.Exceptions.Exception_Message (E));

      Initialized := False;
   end Initialize;

end Auto_Text_IO.Options;
