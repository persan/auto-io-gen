--  Abstract:
--
--  Like the name says.
--
--  Copyright (C) 1999, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Test_Poly_Function_Tables_Monotonic_First_Order_Aux;
procedure Test_Poly_Function_Tables_Monotonic_First_Order is

   use Test_Poly_Function_Tables_Monotonic_First_Order_Aux.First_Order_Monotonic_Float_Function_Tables;

   Table : aliased constant
      Test_Poly_Function_Tables_Monotonic_First_Order_Aux.Float_Function_Tables.Table_Type :=
      (1 => (1.0, 2.0),
       2 => (2.0, 2.5),
       3 => (2.5, 3.0),
       4 => (3.0, 4.0));

   Function_Table : Function_Table_Type (Table'Unchecked_Access);

   Test_Count : constant := 10;
   Domain_Delta : constant Float :=
      (Table (Table'Last).Domain_Value - Table (Table'First).Domain_Value) / Float (Test_Count);
begin

   Put_Line (" x    y    Inverse (y)");
   for I in 0 .. Test_Count loop
      declare
         X : constant Float := Table (Table'First).Domain_Value + Float (I) * Domain_Delta;
         Y : constant Float := Compute (Function_Table, X);
         Inverse_Y : constant Float := Compute_Inverse (Function_Table, Y);
      begin
         Put (X, Aft => 2, Exp => 0);
         Put (Y, Aft => 2, Exp => 0);
         Put (Inverse_Y, Aft => 2, Exp => 0);
      end;
      New_Line;
   end loop;

   Finalize (Function_Table);

end Test_Poly_Function_Tables_Monotonic_First_Order;
