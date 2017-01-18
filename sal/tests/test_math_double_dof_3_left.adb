--  Abstract :
--
--  Test SAL.Math_Double.DOF_3.Left. Generic is tested in
--  test_math_float_dof_3; just ensure that Math_Double.DOF_3.Left
--  and Math_Double.DOF_3.Left.Text_IO are instantiated properly.
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;                   use Ada.Text_IO;
with SAL.Math_Double.DOF_3.Left;    use SAL.Math_Double.DOF_3.Left;
with SAL.Math_Double.DOF_3.Text_IO; use SAL.Math_Double.DOF_3.Text_IO;
procedure Test_Math_Double_DOF_3_Left
is
   use SAL.Math_Double.DOF_3;
   A : constant Unit_Quaternion_Type := Zero_Unit_Quaternion;
   B : constant Cart_Vector_Type := (1.0, 2.0, 3.0);
   C : constant Cart_Vector_Type := A * B;

begin

   Put ("C => "); Put (C); New_Line;

   Put_Line ("Math_Double.DOF_3.Left is instantiated properly.");

end Test_Math_Double_DOF_3_Left;
