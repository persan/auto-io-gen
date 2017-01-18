--  Abstract:
--
--  Like the name says.
--
--  Copyright (C) 2004, 2007 Stephen Leake.  All Rights Reserved.
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

with SAL.Time_Me;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Time_Poly_Lists_Double_Aux;
procedure Time_Poly_Lists_Double is

   package Duration_IO is new Ada.Text_IO.Fixed_IO (Duration);
   use Duration_IO;

   use Time_Poly_Lists_Double_Aux.Integer_Lists;

   Test_Count : Integer := 100_000;
begin

   case Ada.Command_Line.Argument_Count is
   when 0 =>
      --  use defaults
      null;

   when 2 =>
      Test_Count := Integer'Value (Ada.Command_Line.Argument (1));

   when others =>
      Put_Line ("arguments : <test_count>");
      Put_Line ("defaults  : " & Integer'Image (Test_Count));
      return;
   end case;

   --  Time adding and deleting items
   declare
      List : List_Type;

      procedure Time_Me
      is begin
         Add (List, 1234);
         Delete_Tail (List);
      end Time_Me;

      function Time_Add is new SAL.Time_Me.Gen_Time_Me (Time_Me);

      Total_Time : constant Duration := Time_Add (Test_Count);
   begin
      Put_Line ("Timing Add");
      Put ("total time         : "); Put (Total_Time); New_Line;
      Put ("time per operation : "); Put (Total_Time / Test_Count); New_Line;
   end;
end Time_Poly_Lists_Double;
