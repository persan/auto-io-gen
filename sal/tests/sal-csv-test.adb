--  Abstract :
--
--  See spec
--
--  Copyright (C) 2008 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with AUnit.Test_Cases.Registration;
with Ada.Directories;
with SAL.AUnit;
package body SAL.CSV.Test is

   procedure Check is new SAL.AUnit.Gen_Check_Unconstrained_Array
     (Item_Type   => Integer,
      Index_Type  => Positive,
      Array_Type  => Positive_Array_Integer_Type,
      Check_Index => SAL.AUnit.Check,
      Check_Item  => SAL.AUnit.Check);

   procedure Test_Read (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.AUnit;
      CSV_File_Name : constant String := "sal.csv.test-file.csv";
      CSV_File      : File_Type;
   begin

      --  create a test file to read
      declare
         use Ada.Text_IO;
         Test_File : Ada.Text_IO.File_Type;
      begin

         if Ada.Directories.Exists (CSV_File_Name) then
            Ada.Directories.Delete_File (CSV_File_Name);
         end if;

         Create (Test_File, Out_File, CSV_File_Name);

         Put_Line (Test_File, "A, B, C, D, E");
         Put_Line (Test_File, "1, 2, 3, 4, 5");
         Put_Line (Test_File, "6, ,,, 7");

         Close (Test_File);
      end;

      Open (CSV_File, CSV_File_Name, Max_Row_Size => 13);

      Check ("columns", Columns (CSV_File), 5);

      Check ("header commas", CSV_File.Commas.all, (2, 5, 8, 11));

      --  Deliberately read columns out of order to show that works
      Check ("header 1", Read (CSV_File, 1), "A");
      Check ("header 5", Read (CSV_File, 5), " E");
      Check ("header 2", Read (CSV_File, 2), " B");
      Check ("header 4", Read (CSV_File, 4), " D");
      Check ("header 3", Read (CSV_File, 3), " C");

      Next_Row (CSV_File);
      Check ("row 1 commas", CSV_File.Commas.all, (2, 5, 8, 11));
      Check ("row 1 1", Read (CSV_File, 1), "1");
      Check ("row 1 5", Read (CSV_File, 5), " 5");
      Check ("row 1 2", Read (CSV_File, 2), " 2");
      Check ("row 1 4", Read (CSV_File, 4), " 4");
      Check ("row 1 3", Read (CSV_File, 3), " 3");

      Next_Row (CSV_File);
      Check ("row 2 commas", CSV_File.Commas.all, (2, 4, 5, 6));
      Check ("row 2 1", Read (CSV_File, 1), "6");
      Check ("row 2 5", Read (CSV_File, 5), " 7");
      Check ("row 2 2", Read (CSV_File, 2), " ");
      Check ("row 2 4", Read (CSV_File, 4), "");
      Check ("row 2 3", Read (CSV_File, 3), "");

   end Test_Read;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("SAL.CSV.Test");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Read'Access, "Test_Read");
   end Register_Tests;

end SAL.CSV.Test;
