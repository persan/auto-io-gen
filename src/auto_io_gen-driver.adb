--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2001 - 2007 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Wide_Unbounded;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Wide_Text_IO;
with Asis.Ada_Environments;
with Asis.Aux;
with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Extensions;
with Asis.Implementation;
with Auto_Io_Gen.Build;

with Auto_Io_Gen.Generate;
with Auto_Io_Gen.Generate_Image;
with Auto_Io_Gen.Generate_JSON;

with Auto_Io_Gen.Options;
with Gnatvsn;
procedure Auto_Io_Gen.Driver
is
begin

   OPtions.Read_Command_Line;


   if OPtions.Verbose then
      Put_Line (Version);
      Put_Line ("GNAT version (from ASIS) " & Gnatvsn.Gnat_Version_String);
      New_Line;
   end if;

   --  Compile the Asis context if necessary.
   OPtions.Initialize;

   if not OPtions.Initialized then
      --  Error message already output.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if OPtions.Verbose then
      Put_Line ("Package_File_Name => " & OPtions.Package_File_Name.all);
   end if;

   declare
      use Asis;
      use Asis.Ada_Environments;
      use Asis.Compilation_Units;
      use type Asis.Errors.Error_Kinds;

      Asis_Context   : Context;
      CU             : Compilation_Unit; -- The input package
      CU_Kind        : Unit_Kinds;
      Control        : Traverse_Control := Continue;
      State          : Auto_Io_Gen.Build.State_Type;

      procedure Asis_Clean_Up
      is begin
         if Is_Open (Asis_Context) then
            Close (Asis_Context);
         end if;

         Dissociate (Asis_Context);

         Implementation.Finalize;

      end Asis_Clean_Up;

   begin

      Implementation.Initialize;

      Associate
        (Asis_Context,
         "Asis_Context",
         Ada.Strings.Wide_Unbounded.To_Wide_String (Options.Asis_Init_String));

      Open (Asis_Context);

      CU      := Asis.Extensions.Main_Unit_In_Current_Tree (Asis_Context);
      CU_Kind := Asis.Compilation_Units.Unit_Kind (CU);

      if Is_Nil (CU) then
         Put_Line ("file " & OPtions.Package_File_Name.all & " does not contain a unit to create a Text_IO child for");
         Asis_Clean_Up;

      elsif not (CU_Kind = A_Package or else
                 CU_Kind = A_Generic_Package)
      then

         if not OPtions.Quiet then
            Ada.Wide_Text_IO.Put ("Auto_Io_Gen: Compilation unit " & Unit_Full_Name (CU));
            Put_Line (" does not require a Text_IO child");
            Put_Line (" Unit Kind: " & Unit_Kinds'Image (CU_Kind));
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         end if;

      else

         --  If the Text_Name contains a path, it is either absolute
         --  or relative to the build directory. But GNAT ASIS assumes
         --  it is relative to the file directory. So delete the path
         --  info.
         --
         declare
            Temp_Name : constant String := Ada.Characters.Handling.To_String
            --               (Asis.Compilation_Units.Text_Name (CU)); -- GNAT 3.13 returns preprocessor name here.
              (Asis.Extensions.Original_Text_Name (CU)); -- GNAT 3.14 and later does it here.

            --  Allow either directory separator; GNAT 3.14 .. 5.03
            --  return '/' in Temp_Name on Windows and Lynx; 5.04
            --  returns '\' on Windows, '/' on Lynx.
            Name_First : constant Natural := 1 + Ada.Strings.Fixed.Index
              (Source => Temp_Name,
               Set    => Ada.Strings.Maps.To_Set ("/\"),
               Going  => Ada.Strings.Backward);
         begin
            Options.Report_File_Name := new String'(Temp_Name (Name_First .. Temp_Name'Last));
         end;

         Auto_Io_Gen.Build.Build_Tree
           (Element => Asis.Elements.Unit_Declaration (CU),
            Control => Control,
            State   => State);

         if State.Error_Count = 0 then
            if Options.Generate_Text_Io then
               Generate.Create_Text_IO_Child
                 (Type_List           => State.Type_List,
                  Needs_Body          => State.Needs_Body,
                  Needs_Text_IO_Utils => State.Needs_Text_IO_Utils,
                  Invisible           => False,
                  Is_Generic          => State.Is_Generic,
                  Spec_With_List      => State.Spec_With_List,
                  Body_With_List      => State.Body_With_List,
                  Formal_Package_List => State.Formal_Package_List,
                  Parent_Package_Name => Asis.Aux.Name (State.Parent_Package));

               if State.Needs_Invisible_Spec then
                  Generate.Create_Text_IO_Child
                    (Type_List           => State.Type_List,
                     Needs_Body          => State.Needs_Invisible_Body,
                     Needs_Text_IO_Utils => State.Needs_Invisible_Text_IO_Utils,
                     Invisible           => True,
                     Is_Generic          => State.Is_Generic,
                     Spec_With_List      => State.Invisible_Spec_With_List,
                     Body_With_List      => State.Invisible_Body_With_List,
                     Formal_Package_List => State.Formal_Package_List,
                     Parent_Package_Name => Asis.Aux.Name (State.Parent_Package));
               end if;
            end if;

            if Options.Generate_Image then
               Generate_Image.Create_Text_IO_Child
                 (Type_List           => State.Type_List,
                  Needs_Body          => State.Needs_Body,
                  Needs_Text_IO_Utils => State.Needs_Text_IO_Utils,
                  Invisible           => False,
                  Is_Generic          => State.Is_Generic,
                  Spec_With_List      => State.Spec_With_List,
                  Body_With_List      => State.Body_With_List,
                  Formal_Package_List => State.Formal_Package_List,
                  Parent_Package_Name => Asis.Aux.Name (State.Parent_Package));

               if State.Needs_Invisible_Spec then
                  Generate_Image.Create_Text_IO_Child
                    (Type_List           => State.Type_List,
                     Needs_Body          => State.Needs_Invisible_Body,
                     Needs_Text_IO_Utils => State.Needs_Invisible_Text_IO_Utils,
                     Invisible           => True,
                     Is_Generic          => State.Is_Generic,
                     Spec_With_List      => State.Invisible_Spec_With_List,
                     Body_With_List      => State.Invisible_Body_With_List,
                     Formal_Package_List => State.Formal_Package_List,
                     Parent_Package_Name => Asis.Aux.Name (State.Parent_Package));
               end if;
            end if;
            if Options.Generate_JSON then
               Generate_JSON.Create_Text_IO_Child
                 (Type_List           => State.Type_List,
                  Needs_Body          => State.Needs_Body,
                  Needs_Text_IO_Utils => State.Needs_Text_IO_Utils,
                  Invisible           => False,
                  Is_Generic          => State.Is_Generic,
                  Spec_With_List      => State.Spec_With_List,
                  Body_With_List      => State.Body_With_List,
                  Formal_Package_List => State.Formal_Package_List,
                  Parent_Package_Name => Asis.Aux.Name (State.Parent_Package));

               if State.Needs_Invisible_Spec then
                  Generate_JSON.Create_Text_IO_Child
                    (Type_List           => State.Type_List,
                     Needs_Body          => State.Needs_Invisible_Body,
                     Needs_Text_IO_Utils => State.Needs_Invisible_Text_IO_Utils,
                     Invisible           => True,
                     Is_Generic          => State.Is_Generic,
                     Spec_With_List      => State.Invisible_Spec_With_List,
                     Body_With_List      => State.Invisible_Body_With_List,
                     Formal_Package_List => State.Formal_Package_List,
                     Parent_Package_Name => Asis.Aux.Name (State.Parent_Package));
               end if;
            end if;

         else
            Put_Line (Integer'Image (State.Error_Count) & " errors; no Text_IO child generated");
         end if;
      end if;

      Asis_Clean_Up;
   exception
      when others =>
         Asis_Clean_Up;
         raise;
   end;

   Auto_Io_Gen.Options.Clean_Up;

exception
   when E : Parameter_Error =>
      New_Line;
      Put_Line (Exception_Message (E));

      Auto_Io_Gen.Options.Clean_Up;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when Asis.Exceptions.ASIS_Failed =>
      New_Line;
      Put_Line ("Auto_Io_Gen: internal error. Run with -d option for more info.");

      Auto_Io_Gen.Options.Clean_Up;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      if Auto_Io_Gen.Options.Debug then
         Put_Line ("ASIS Error");
         Ada.Wide_Text_IO.Put_Line (Asis.Implementation.Diagnosis);
      end if;

   when E : others =>
      New_Line;
      Put_Line ("Unexpected bug in " & Version);
      Put (Exception_Name (E) & " was raised: ");

      if Exception_Message (E)'Length = 0 then
         Put_Line ("(no exception message)");
      else
         Put_Line (Exception_Message (E));
      end if;

      Put_Line ("Please report to stephen_leake@stephe-leake.org");

      Auto_Io_Gen.Options.Clean_Up;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end Auto_Io_Gen.Driver;
