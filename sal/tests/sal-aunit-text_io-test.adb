--  Abstract :
--
--  See spec
--
--  Copyright (C) 2008 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with Ada.Text_IO;
with Ada.Directories;
with AUnit.Test_Cases.Registration;
with SAL.AUnit;
with AUnit.Assertions;
package body SAL.AUnit.Text_IO.Test is

   ----------
   --  Test subprograms
   --
   --  We only test things that are complicated

   procedure Check_Files_Skip_Lines (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Ada.Text_IO;
      Expected_File_Name : constant String := "sal-aunit-text_io-test.expected";
      Computed_File_Name : constant String := "sal-aunit-text_io-test.computed";

      Expected_File : File_Type;
      Computed_File : File_Type;
   begin

      begin
         --  If file is left over from a previous test, delete it
         Ada.Directories.Delete_File (Expected_File_Name);
      exception
      when Name_Error =>
         --  not left over
         null;
      end;

      begin
         Ada.Directories.Delete_File (Computed_File_Name);
      exception
      when Name_Error =>
         null;
      end;

      Create (Expected_File, Out_File, Expected_File_Name);
      Put_Line (Expected_File, "line 1");
      Put_Line (Expected_File, "line 2 contains a time stamp; skip it 1:34 PM");
      Put_Line (Expected_File, "line 3");
      Put_Line (Expected_File, "line 4 contains a date stamp; skip it 2/15/2008");
      Close (Expected_File);

      Create (Computed_File, Out_File, Computed_File_Name);
      Put_Line (Computed_File, "line 1");
      Put_Line (Computed_File, "line 2 contains a time stamp; skip it 1:37 PM");
      Put_Line (Computed_File, "line 3");
      Put_Line (Computed_File, "line 4 contains a date stamp; skip it 4/15/2008");
      Close (Computed_File);

      begin
         Check_Files ("Fail", Computed_File_Name, Expected_File_Name);

         Standard.AUnit.Assertions.Assert (False, "didn't get ASSERT_ERROR");
      exception
      when Standard.AUnit.Assertions.Assertion_Error =>
         null;
      end;

      Check_Files ("Skip lines", Computed_File_Name, Expected_File_Name, Skip => (2, 4));

   end Check_Files_Skip_Lines;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("SAL.AUnit.Text_IO.Test");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_Files_Skip_Lines'Access, "Check_Files_Skip_Lines");
   end Register_Tests;

end SAL.AUnit.Text_IO.Test;
