--  Abstract :
--
--  See spec.
--
--  This is an example of using SAL.AUnit.Check_Files to convert an
--  old file-based test to AUnit, rather than rewriting it to be fully
--  AUnit.
--
--  Copyright (C) 2002, 2006 - 2007 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);
with AUnit.Test_Cases.Registration;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.AUnit.Text_IO;
with SAL.Gen_FIFO.Gen_Text_IO;
package body Test_Gen_FIFO is

   procedure Output_File
   is
      package Integer_FIFO is new SAL.Gen_FIFO (Element_Type => Integer, FIFO_Size => 10);
      package Integer_FIFO_Text_IO is new Integer_FIFO.Gen_Text_IO (Integer'Image);
      use Integer_FIFO;
      use Integer_FIFO_Text_IO;

      FIFO : FIFO_Type;
      Item : Integer;
   begin
      Fill (FIFO, 100);

      Put_Line ("just declared");
      Put (FIFO);
      Put_Line ("Is_Empty => " & Boolean'Image (Is_Empty (FIFO)));
      Put_Line ("Is_Full  => " & Boolean'Image (Is_Full (FIFO)));
      New_Line;

      Put_Line ("Add 1");
      Put (FIFO, 1);
      Put (FIFO);
      Put_Line ("Is_Empty => " & Boolean'Image (Is_Empty (FIFO)));
      Put_Line ("Is_Full  => " & Boolean'Image (Is_Full (FIFO)));
      New_Line;

      Put_Line ("Fill");
      for I in 2 .. 10 loop
         Put (FIFO, I);
      end loop;
      Put (FIFO);
      New_Line;

      Put_Line ("Get some");
      Get (FIFO, Item);
      Put_Line ("1 => " & Integer'Image (Item));
      Get (FIFO, Item);
      Put_Line ("2 => " & Integer'Image (Item));
      Get (FIFO, Item);
      Put_Line ("3 => " & Integer'Image (Item));
      Put (FIFO);
      New_Line;

      Put_Line ("Fill");
      for I in 21 .. 23 loop
         Put (FIFO, I);
      end loop;
      Put (FIFO);
      New_Line;
   end Output_File;

   ----------
   --  AUnit test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Computed_Name : constant String := "test_gen_fifo.out";
      Expected_Name : constant String := "../../Source_Common/Test/test_gen_fifo.good_out";
      Computed : File_Type;
   begin
      Create (Computed, Out_File, Computed_Name);
      Set_Output (Computed);
      Output_File;
      Close (Computed);
      Set_Output (Standard_Output);
      SAL.AUnit.Text_IO.Check_Files ("", Computed_Name, Expected_Name);
   end Run_Test;

   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Gen_FIFO");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Run_Test'Access, "Run_Test");
   end Register_Tests;

end Test_Gen_FIFO;
