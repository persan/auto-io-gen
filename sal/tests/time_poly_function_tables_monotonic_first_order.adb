--  Abstract:
--
--  Like the name says.
--
--  Copyright (C) 1999, 2000, 2002 Stephen Leake.  All Rights Reserved.
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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Poly_Function_Tables_Monotonic_First_Order_Aux;
procedure Time_Poly_Function_Tables_Monotonic_First_Order is

   package Aux renames Test_Poly_Function_Tables_Monotonic_First_Order_Aux;

   use Aux.First_Order_Monotonic_Float_Function_Tables;

   type Test_Result_Type is record
      X : Float;
      Y : Float;
      Inverse : Float;
   end record;

   Test_Count : Integer := 10_000;
   Table_Size : Integer := 100_000;
begin

   case Ada.Command_Line.Argument_Count is
   when 0 =>
      --  use defaults
      null;
   when 2 =>
      Table_Size := Integer'Value (Ada.Command_Line.Argument (1));
      Test_Count := Integer'Value (Ada.Command_Line.Argument (2));
      if Table_Size < Test_Count then
         Put_Line ("Sorry, Table_Size must be greater than Test_Count");
         return;
      end if;
   when others =>
      Put_Line ("arguments : <table_size> <test_count>");
      Put_Line ("defaults  : " & Integer'Image (Table_Size) & "     " & Integer'Image (Test_Count));
      return;
   end case;

   Aux.Table := new Aux.Float_Function_Tables.Table_Type (1 .. Table_Size);

   --  fill function table with parabola
   for I in Aux.Table.all'Range loop
      Aux.Table (I).Domain_Value := Float (I);
      Aux.Table (I).Range_Value := Float (I) ** 2;
   end loop;

   declare
      Function_Table : Function_Table_Type (Aux.Float_Function_Tables.Table_Access_Type (Aux.Table));
   begin
      Put_Line ("Timing function table operations");
      Put_Line ("Table size " & Integer'Image (Table_Size));
      Put_Line ("Test count " & Integer'Image (Test_Count));
      declare
         use Ada.Calendar;
         subtype Test_Index_Type is Integer range 1 .. Test_Count;
         type Test_Result_Array_Type is array (Test_Index_Type) of Test_Result_Type;
         Test_Results : Test_Result_Array_Type;
         Loop_Start_Time : Time;
         Loop_End_Time : Time;
         Loop_Total_Time : Float;
         Forward_Start_Time : Time;
         Forward_End_Time : Time;
         Forward_Total_Time : Float;
         Inverse_Start_Time : Time;
         Inverse_End_Time : Time;
         Inverse_Total_Time : Float;
         Average_Time : Float;
      begin
         --  set function arguments, estimate loop overhead
         Loop_Start_Time := Clock;
         for I in Test_Index_Type loop
            Test_Results (I).X := Float (I);
         end loop;
         Loop_End_Time := Clock;

         Forward_Start_Time := Clock;
         for I in Test_Index_Type loop
            Test_Results (I).Y := Compute (Function_Table, Test_Results (I).X);
         end loop;
         Forward_End_Time := Clock;

         Inverse_Start_Time := Clock;
         for I in Test_Index_Type loop
            Test_Results (I).Inverse := Compute_Inverse (Function_Table, Test_Results (I).Y);
         end loop;
         Inverse_End_Time := Clock;

         Put_Line ("Total time (seconds) :");
         Loop_Total_Time := Float (Loop_End_Time - Loop_Start_Time);
         Put ("Loop    "); Put (Loop_Total_Time, Fore => 3, Aft => 3, Exp => 3);
         New_Line;
         Forward_Total_Time := Float (Forward_End_Time - Forward_Start_Time);
         Put ("Forward "); Put (Forward_Total_Time, Fore => 3, Aft => 3, Exp => 3);
         New_Line;
         Inverse_Total_Time := Float (Inverse_End_Time - Inverse_Start_Time);
         Put ("Inverse "); Put (Inverse_Total_Time, Fore => 3, Aft => 3, Exp => 3);
         New_Line;

         Put_Line ("Average time per lookup (seconds) :");
         --  use Float, not Duration, to ensure we have enough precision
         Average_Time := Loop_Total_Time / Float (Test_Count);
         Put ("Loop    "); Put (Average_Time, Fore => 3, Aft => 3, Exp => 3);
         New_Line;
         Average_Time := (Forward_Total_Time - Loop_Total_Time) / Float (Test_Count);
         Put ("Forward "); Put (Average_Time, Fore => 3, Aft => 3, Exp => 3);
         New_Line;
         Average_Time := (Inverse_Total_Time - Loop_Total_Time) / Float (Test_Count);
         Put ("Inverse "); Put (Average_Time, Fore => 3, Aft => 3, Exp => 3);
         New_Line;
      end;
   end;

   Aux.Free (Aux.Table);
end Time_Poly_Function_Tables_Monotonic_First_Order;
