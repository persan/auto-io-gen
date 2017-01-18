--  Abstract :
--
--  Test SAL.Gen_Math.Gen_DOF_3.Gen_Image
--
--  Copyright (C) 2002, 2003 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Math_Float.DOF_3.Image;
with SAL.Math_Float.DOF_3; use SAL.Math_Float.DOF_3;
procedure Test_Gen_Math_Gen_DOF_3_Gen_Image
is
   A : constant Cart_Vector_Type := (1.0, 2.0, 3.0);

   B : constant Unit_Vector_Type := To_Unit_Vector (Cart_Vector_Type'(0.69254723, -0.39984233, 0.60042023));

   C : constant Inertia_Type := (1.0, 2.0, 3.0, 0.0, 0.0, 0.0);

begin
   Put_Line ("A => " & SAL.Math_Float.DOF_3.Image.Image (A));
   Put_Line ("B => " & SAL.Math_Float.DOF_3.Image.Image (B));
   Put_Line ("C => " & SAL.Math_Float.DOF_3.Image.Image (C));
end Test_Gen_Math_Gen_DOF_3_Gen_Image;
