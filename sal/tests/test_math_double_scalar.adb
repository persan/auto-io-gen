--  Abstract:
--
--  Test SAL.Math_Double.Scalar. Generic is tested in
--  test_math_double_scalar; just ensure that Math_Double.Scalar,
--  Math_Double.Scalar.Text_IO, and Math_Double.Text_IO are
--  instantiated properly.
--
--  Copyright (C) 2001 - 2003, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;                    use Ada.Text_IO;
with SAL.Math_Double.Scalar;         use SAL.Math_Double.Scalar;
with SAL.Math_Double.Scalar.Text_IO; use SAL.Math_Double.Scalar.Text_IO;
with SAL.Math_Double.Text_IO;        use SAL.Math_Double.Text_IO;
procedure Test_Math_Double_Scalar
is
   S       : constant SAL.Math_Double.Real_Type := 10.0;
   A_Limit : constant Limit_Type                := To_Limit (-1.0, 1.0);
begin
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Fore := 3;
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Aft  := 5;
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Exp  := 0;

   Put ("S       => "); Put (S); New_Line;
   Put ("A_Limit => "); Put (A_Limit, Single_Line_Record => True); New_Line;

   Put_Line ("Math_Double.DOF_2 is instantiated properly.");
end Test_Math_Double_Scalar;
