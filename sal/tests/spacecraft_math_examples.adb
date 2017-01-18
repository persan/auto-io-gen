--  Abstract :
--
--  Examples for ../../doc/spacecraft_math.tex
--
--  Copyright (C) 2006, 2007, 2009 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with Ada.Numerics;
with Ada.Text_IO;                   use Ada.Text_IO;
with SAL.Math_Double.DOF_3.Left;
with SAL.Math_Double.DOF_3.Text_IO; use SAL.Math_Double.DOF_3.Text_IO;
with SAL.Math_Double.DOF_3.Wertz;
with SAL.Math_Double.DOF_6.Left;
with SAL.Math_Double.DOF_6.Text_IO; use SAL.Math_Double.DOF_6.Text_IO;
with SAL.Math_Double.DOF_6.Wertz;
with SAL.Math_Double.Elementary;    use SAL.Math_Double.Elementary;
with SAL.Math_Double.Text_IO;
procedure Spacecraft_Math_Examples is
   use SAL.Math_Double;
   use SAL.Math_Double.DOF_3;
   use SAL.Math_Double.DOF_3.Cart_Vector_Ops;
   use SAL.Math_Double.DOF_6;
   Pi : constant := Ada.Numerics.Pi;

begin
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Aft := 5;
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Exp := 0;

   SAL.Math_Double.DOF_3.Text_IO.Cart_Vector_Text_IO.Default_Aft := 5;
   SAL.Math_Double.DOF_3.Text_IO.Cart_Vector_Text_IO.Default_Exp := 0;

   SAL.Math_Double.DOF_3.Text_IO.Set_Unit_Vector_Text_IO_Default_Aft (5);
   SAL.Math_Double.DOF_3.Text_IO.Set_Unit_Vector_Text_IO_Default_Exp (0);

   SAL.Math_Double.DOF_6.Text_IO.Dual_Cart_Vector_Text_IO.Default_Aft := 5;
   SAL.Math_Double.DOF_6.Text_IO.Dual_Cart_Vector_Text_IO.Default_Exp := 0;

   Angle_Axis :
   declare
      --  exam:angle_axis
      --  Left-multiply SAL notation
      Base_Tran_1  : constant Cart_Vector_Type := (2.0, 0.0, 1.0);
      Base_Rot_1_2 : constant Mag_Axis_Type    := (0.1, Y_Unit);
      Base_Tran_2  : constant Cart_Vector_Type := Rotate (Base_Rot_1_2, Base_Tran_1);

      Base_Unit_Scz_0 : constant Cart_Vector_Type := (0.0, 0.0, 1.0);
      Base_Unit_Scz_1 : constant Cart_Vector_Type := Rotate (Base_Rot_1_2, Base_Unit_Scz_0);

      Sc0_Unit_Sun : constant Cart_Vector_Type := (0.0, 0.0, 1.0);
      Sc1_Rot_Sc0  : constant Mag_Axis_Type    := (-Base_Rot_1_2.Mag, Base_Rot_1_2.Axis);
      Sc1_Unit_Sun : constant Cart_Vector_Type := Rotate (Sc1_Rot_Sc0, Sc0_Unit_Sun);
   begin
      Put_Line ("exam:angle_axis 1");
      Put ("Base_Tran_1     => "); Put (Base_Tran_1); New_Line;
      Put ("Base_Rot_1_2    => "); Put (Base_Rot_1_2); New_Line;
      Put ("Base_Tran_2     => "); Put (Base_Tran_2); New_Line;

      Put_Line ("exam:angle_axis 2");
      Put ("Base_Unit_Scz_0 => "); Put (Base_Unit_Scz_0); New_Line;
      Put ("Base_Unit_Scz_1 => "); Put (Base_Unit_Scz_1); New_Line;

      Put_Line ("exam:angle_axis 3");
      Put ("Sc1_Rot_Sc0  => "); Put (Sc1_Rot_Sc0); New_Line;
      Put ("Sc1_Unit_Sun => "); Put (Sc1_Unit_Sun); New_Line;
      New_Line;
   end Angle_Axis;

   Quaternions_Left :
   declare
      use SAL.Math_Double.DOF_3.Left;
      --  exam:quaternions
      --  Left-multiply SAL notation
      Base_Tran_1   : constant Cart_Vector_Type     := (2.0, 0.0, 1.0);
      Base_Quat_1_2 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(0.1, Y_Unit));
      Base_Rot_1_2  : constant Mag_Axis_Type        := To_Mag_Axis (Base_Quat_1_2);
      Base_Tran_2   : constant Cart_Vector_Type     := Base_Quat_1_2 * Base_Tran_1;
      Base_Quat_2_3 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Pi / 2.0, X);
      Base_Quat_1_3 : constant Unit_Quaternion_Type := Base_Quat_2_3 * Base_Quat_1_2;
      Base_Rot_1_3  : constant Mag_Axis_Type        := To_Mag_Axis (Base_Quat_1_3);
      Base_Tran_3_A : constant Cart_Vector_Type     := Base_Quat_2_3 * Base_Tran_2;
      Base_Tran_3_B : constant Cart_Vector_Type     := Base_Quat_1_3 * Base_Tran_1;

      Base_Unit_Scz0    : constant Cart_Vector_Type     := (0.0, 0.0, 1.0);
      Base_Quat_Sc0_Sc1 : constant Unit_Quaternion_Type := To_Unit_Quaternion (0.1, Y);
      Base_Unit_Scz1    : constant Cart_Vector_Type     := Base_Quat_Sc0_Sc1 * Base_Unit_Scz0;
      Base_Quat_Sc1_Sc2 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Pi / 2.0, X);
      Base_Unit_Scz2_A  : constant Cart_Vector_Type     := Base_Quat_Sc1_Sc2 * Base_Unit_Scz1;
      Base_Quat_Sc0_Sc2 : constant Unit_Quaternion_Type := Base_Quat_Sc1_Sc2 * Base_Quat_Sc0_Sc1;
      Base_Unit_Scz2_B  : constant Cart_Vector_Type     := Base_Quat_Sc0_Sc2 * Base_Unit_Scz0;

      Base_Unit_Sc_Sun : constant Cart_Vector_Type := (0.0, 0.0, 1.0);

      Sc0_Quat_Base   : constant Unit_Quaternion_Type := Zero_Unit_Quaternion;
      Sc0_Unit_Sc_Sun : constant Cart_Vector_Type     := Sc0_Quat_Base * Base_Unit_Sc_Sun;

      Sc1_Quat_Base   : constant Unit_Quaternion_Type := Inverse (Base_Quat_Sc0_Sc1);
      Sc1_Unit_Sc_Sun : constant Cart_Vector_Type     := Sc1_Quat_Base * Base_Unit_Sc_Sun;

      Sc1_Quat_Sc1_Sc2 : constant Unit_Quaternion_Type := Sc1_Quat_Base * Base_Quat_Sc1_Sc2 * Inverse (Sc1_Quat_Base);
      Sc1_Rot_Sc1_Sc2  : constant Mag_Axis_Type        := To_Mag_Axis (Sc1_Quat_Sc1_Sc2);
      Sc2_Quat_Sc1     : constant Unit_Quaternion_Type := Inverse (Sc1_Quat_Sc1_Sc2);
      Sc2_Rot_Sc1      : constant Mag_Axis_Type        := To_Mag_Axis (Sc2_Quat_Sc1);

      Sc2_Unit_Sc_Sun_A : constant Cart_Vector_Type := Sc2_Quat_Sc1 * Sc1_Unit_Sc_Sun;

      Sc2_Quat_Base_B   : constant Unit_Quaternion_Type := Sc2_Quat_Sc1 * Sc1_Quat_Base;
      Sc2_Rot_Base_B    : constant Mag_Axis_Type        := To_Mag_Axis (Sc2_Quat_Base_B);
      Sc2_Unit_Sc_Sun_B : constant Cart_Vector_Type     := Sc2_Quat_Base_B * Base_Unit_Sc_Sun;

      Sc2_Quat_Base_C   : constant Unit_Quaternion_Type := Inverse (Base_Quat_Sc0_Sc2);
      Sc2_Rot_Base_C    : constant Mag_Axis_Type        := To_Mag_Axis (Sc2_Quat_Base_C);
      Sc2_Unit_Sc_Sun_C : constant Cart_Vector_Type     := Sc2_Quat_Base_C * Base_Unit_Sc_Sun;

   begin
      Put_Line ("exam:quaternions_left");
      Put ("Base_Quat_1_2 => "); Put (Base_Quat_1_2); New_Line;
      Put ("Base_Rot_1_2  => "); Put (Base_Rot_1_2); New_Line;
      Put ("Base_Tran_1   => "); Put (Base_Tran_1); New_Line;
      Put ("Base_Tran_2   => "); Put (Base_Tran_2); New_Line;
      Put ("Base_Quat_2_3 => "); Put (Base_Quat_2_3); New_Line;
      Put ("Base_Quat_1_3 => "); Put (Base_Quat_1_3); New_Line;
      Put ("Base_Rot_1_3  => "); Put (Base_Rot_1_3); New_Line;
      Put ("Base_Tran_3_A => "); Put (Base_Tran_3_A); New_Line;
      Put ("Base_Tran_3_B => "); Put (Base_Tran_3_B); New_Line;
      New_Line;

      Put ("Base_Unit_Scz0    => "); Put (Base_Unit_Scz0); New_Line;
      Put ("Base_Quat_Sc0_Sc1 => "); Put (Base_Quat_Sc0_Sc1); New_Line;
      Put ("Base_Unit_Scz1    => "); Put (Base_Unit_Scz1); New_Line;
      Put ("Base_Quat_Sc0_Sc2 => "); Put (Base_Quat_Sc0_Sc2); New_Line;
      Put ("Base_Unit_Scz2_A  => "); Put (Base_Unit_Scz2_A); New_Line;
      Put ("Base_Quat_Sc0_Sc2 => "); Put (Base_Quat_Sc0_Sc2); New_Line;
      Put ("Base_Unit_Scz2_B  => "); Put (Base_Unit_Scz2_B); New_Line;
      New_Line;

      Put ("Sc0_Quat_Base     => "); Put (Sc0_Quat_Base); New_Line;
      Put ("Sc0_Unit_Sc_Sun   => "); Put (Sc0_Unit_Sc_Sun); New_Line;
      Put ("Sc1_Quat_Base     => "); Put (Sc1_Quat_Base); New_Line;
      Put ("Sc1_Unit_Sc_Sun   => "); Put (Sc1_Unit_Sc_Sun); New_Line;
      New_Line;
      Put ("Sc1_Quat_Sc1_Sc2  => "); Put (Sc1_Quat_Sc1_Sc2); New_Line;
      Put ("Sc1_Rot_Sc1_Sc2   => "); Put (Sc1_Rot_Sc1_Sc2); New_Line;
      Put ("Sc2_Quat_Sc1      => "); Put (Sc2_Quat_Sc1); New_Line;
      Put ("Sc2_Rot_Sc1       => "); Put (Sc2_Rot_Sc1); New_Line;
      Put ("Sc2_Unit_Sc_Sun_A => "); Put (Sc2_Unit_Sc_Sun_A); New_Line;
      New_Line;
      Put ("Sc2_Quat_Base_B   => "); Put (Sc2_Quat_Base_B); New_Line;
      Put ("Sc2_Rot_Base_B    => "); Put (Sc2_Rot_Base_B); New_Line;
      Put ("Sc2_Unit_Sc_Sun_B => "); Put (Sc2_Unit_Sc_Sun_B); New_Line;
      New_Line;
      Put ("Sc2_Quat_Base_C   => "); Put (Sc2_Quat_Base_C); New_Line;
      Put ("Sc2_Rot_Base_C    => "); Put (Sc2_Rot_Base_C); New_Line;
      Put ("Sc2_Unit_Sc_Sun_C => "); Put (Sc2_Unit_Sc_Sun_C); New_Line;
      New_Line;
   end Quaternions_Left;

   Quaternions_Right :
   declare
      use SAL.Math_Double.DOF_3.Wertz;
      --  not in spacecraft_math.tex. Right-multiply SAL notation;
      --  plain numbers are prefixed with 'P' for positions.
      P1_Tran_Base    : constant Cart_Vector_Type     := (2.0, 0.0, 1.0);
      P1_P2_Quat_Base : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(0.1, Y_Unit));
      P2_Tran_Base    : constant Cart_Vector_Type     := P1_Tran_Base * P1_P2_Quat_Base;

      P2_P3_Quat_Base : constant Unit_Quaternion_Type := To_Unit_Quaternion (Pi / 2.0, X);
      P1_P3_Quat_Base : constant Unit_Quaternion_Type := P1_P2_Quat_Base * P2_P3_Quat_Base;
      P3_Tran_Base_A  : constant Cart_Vector_Type     := P2_Tran_Base * P2_P3_Quat_Base;
      P3_Tran_Base_B  : constant Cart_Vector_Type     := P1_Tran_Base * P1_P3_Quat_Base;

      Scz0_Unit_Base    : constant Cart_Vector_Type     := (0.0, 0.0, 1.0);
      Sc0_Sc1_Quat_Base : constant Unit_Quaternion_Type := To_Unit_Quaternion (0.1, Y);
      Scz1_Unit_Base    : constant Cart_Vector_Type     := Scz0_Unit_Base * Sc0_Sc1_Quat_Base;
      Sc1_Sc2_Quat_Base : constant Unit_Quaternion_Type := To_Unit_Quaternion (Pi / 2.0, X);
      Scz2_Unit_Base_A  : constant Cart_Vector_Type     := Scz1_Unit_Base * Sc1_Sc2_Quat_Base;
      Sc0_Sc2_Quat_Base : constant Unit_Quaternion_Type := Sc0_Sc1_Quat_Base * Sc1_Sc2_Quat_Base;
      Scz2_Unit_Base_B  : constant Cart_Vector_Type     := Scz0_Unit_Base * Sc0_Sc2_Quat_Base;

      Sc_Sun_Unit_Base : constant Cart_Vector_Type := (0.0, 0.0, 1.0);

      Base_Quat_Sc0   : constant Unit_Quaternion_Type := Zero_Unit_Quaternion;
      Sc_Sun_Unit_Sc0 : constant Cart_Vector_Type     := Sc_Sun_Unit_Base * Base_Quat_Sc0;

      Base_Quat_Sc1   : constant Unit_Quaternion_Type := Inverse (Sc0_Sc1_Quat_Base);
      Sc_Sun_Unit_Sc1 : constant Cart_Vector_Type     := Sc_Sun_Unit_Base * Base_Quat_Sc1;

      Sc1_Sc2_Quat_Sc1 : constant Unit_Quaternion_Type := Inverse (Base_Quat_Sc1) * Sc1_Sc2_Quat_Base * Base_Quat_Sc1;
      Sc1_Sc2_Rot_Sc1  : constant Mag_Axis_Type        := To_Mag_Axis (Sc1_Sc2_Quat_Sc1);
      Sc1_Quat_Sc2     : constant Unit_Quaternion_Type := Inverse (Sc1_Sc2_Quat_Sc1);
      Sc1_Rot_Sc2      : constant Mag_Axis_Type        := To_Mag_Axis (Sc1_Quat_Sc2);

      Sc_Sun_Unit_Sc2_A : constant Cart_Vector_Type := Sc_Sun_Unit_Sc1 * Sc1_Quat_Sc2;

      Base_Quat_Sc2_B   : constant Unit_Quaternion_Type := Base_Quat_Sc1 * Sc1_Quat_Sc2;
      Base_Rot_Sc2_B    : constant Mag_Axis_Type        := To_Mag_Axis (Base_Quat_Sc2_B);
      Sc_Sun_Unit_Sc2_B : constant Cart_Vector_Type     := Sc_Sun_Unit_Base * Base_Quat_Sc2_B;

      Base_Quat_Sc2_C   : constant Unit_Quaternion_Type := Inverse (Sc0_Sc2_Quat_Base);
      Base_Rot_Sc2_C    : constant Mag_Axis_Type        := To_Mag_Axis (Base_Quat_Sc2_C);
      Sc_Sun_Unit_Sc2_C : constant Cart_Vector_Type     := Sc_Sun_Unit_Base * Base_Quat_Sc2_C;

   begin
      Put_Line ("exam:quaternions_right");
      Put ("P1_P2_Quat_Base => "); Put (P1_P2_Quat_Base); New_Line;
      Put ("P1_Tran_Base    => "); Put (P1_Tran_Base); New_Line;
      Put ("P2_Tran_Base    => "); Put (P2_Tran_Base); New_Line;
      Put ("P2_P3_Quat_Base => "); Put (P2_P3_Quat_Base); New_Line;
      Put ("P1_P3_Quat_Base => "); Put (P1_P3_Quat_Base); New_Line;
      Put ("P3_Tran_Base_A  => "); Put (P3_Tran_Base_A); New_Line;
      Put ("P3_Tran_Base_B  => "); Put (P3_Tran_Base_B); New_Line;
      New_Line;

      Put ("Scz0_Unit_Base    => "); Put (Scz0_Unit_Base); New_Line;
      Put ("Sc0_Sc1_Quat_Base => "); Put (Sc0_Sc1_Quat_Base); New_Line;
      Put ("Scz1_Unit_Base    => "); Put (Scz1_Unit_Base); New_Line;
      Put ("Sc0_Sc2_Quat_Base => "); Put (Sc0_Sc2_Quat_Base); New_Line;
      Put ("Scz2_Unit_Base_A  => "); Put (Scz2_Unit_Base_A); New_Line;
      Put ("Sc0_Sc2_Quat_Base => "); Put (Sc0_Sc2_Quat_Base); New_Line;
      Put ("Scz2_Unit_Base_B  => "); Put (Scz2_Unit_Base_B); New_Line;
      New_Line;

      Put ("Base_Quat_Sc0     => "); Put (Base_Quat_Sc0); New_Line;
      Put ("Sc_Sun_Unit_Sc0   => "); Put (Sc_Sun_Unit_Sc0); New_Line;
      Put ("Base_Quat_Sc1     => "); Put (Base_Quat_Sc1); New_Line;
      Put ("Sc_Sun_Unit_Sc1   => "); Put (Sc_Sun_Unit_Sc1); New_Line;
      New_Line;
      Put ("Sc1_Sc2_Quat_Sc1  => "); Put (Sc1_Sc2_Quat_Sc1); New_Line;
      Put ("Sc1_Sc2_Rot_Sc1   => "); Put (Sc1_Sc2_Rot_Sc1); New_Line;
      Put ("Sc1_Quat_Sc2      => "); Put (Sc1_Quat_Sc2); New_Line;
      Put ("Sc1_Rot_Sc2       => "); Put (Sc1_Rot_Sc2); New_Line;
      Put ("Sc_Sun_Unit_Sc2_A => "); Put (Sc_Sun_Unit_Sc2_A); New_Line;
      New_Line;
      Put ("Base_Quat_Sc2_B   => "); Put (Base_Quat_Sc2_B); New_Line;
      Put ("Base_Rot_Sc2_B    => "); Put (Base_Rot_Sc2_B); New_Line;
      Put ("Sc_Sun_Unit_Sc2_B => "); Put (Sc_Sun_Unit_Sc2_B); New_Line;
      New_Line;
      Put ("Base_Quat_Sc2_C   => "); Put (Base_Quat_Sc2_C); New_Line;
      Put ("Base_Rot_Sc2_C    => "); Put (Base_Rot_Sc2_C); New_Line;
      Put ("Sc_Sun_Unit_Sc2_C => "); Put (Sc_Sun_Unit_Sc2_C); New_Line;
      New_Line;
   end Quaternions_Right;

   Rot_Matrix :
   declare
      use SAL.Math_Double.DOF_3.Left;
      --  exam:matrices
      --  Left-multiply SAL notation
      Base_Tran_1        : constant Cart_Vector_Type     := (2.0, 0.0, 1.0);
      Base_Rot_Mat_1_2   : constant Rot_Matrix_Type      := To_Rot_Matrix (Mag_Axis_Type'(0.1, Y_Unit));
      Base_Rot_1_2       : constant Mag_Axis_Type        := To_Mag_Axis (Base_Rot_Mat_1_2);
      Base_Quat_1_2      : constant Unit_Quaternion_Type := To_Unit_Quaternion (Base_Rot_Mat_1_2);
      Base_Rot_Mat_1_2_Q : constant Rot_Matrix_Type      := To_Rot_Matrix (Base_Quat_1_2);
      Base_Tran_2        : constant Cart_Vector_Type     := Base_Rot_Mat_1_2 * Base_Tran_1;
      Base_Rot_Mat_2_3   : constant Rot_Matrix_Type      := To_Rot_Matrix (Mag_Axis_Type'(Pi / 2.0, X_Unit));
      Base_Rot_Mat_1_3   : constant Rot_Matrix_Type      := Base_Rot_Mat_2_3 * Base_Rot_Mat_1_2;
      Base_Rot_1_3       : constant Mag_Axis_Type        := To_Mag_Axis (Base_Rot_Mat_1_3);
      Base_Tran_3_A      : constant Cart_Vector_Type     := Base_Rot_Mat_2_3 * Base_Tran_2;
      Base_Tran_3_B      : constant Cart_Vector_Type     := Base_Rot_Mat_1_3 * Base_Tran_1;

      Base_Unit_Scz0       : constant Cart_Vector_Type := (0.0, 0.0, 1.0);
      Base_Rot_Mat_Sc0_Sc1 : constant Rot_Matrix_Type  := To_Rot_Matrix (Mag_Axis_Type'(0.1, Y_Unit));
      Base_Unit_Scz1       : constant Cart_Vector_Type := Base_Rot_Mat_Sc0_Sc1 * Base_Unit_Scz0;
      Base_Rot_Mat_Sc1_Sc2 : constant Rot_Matrix_Type  := To_Rot_Matrix (Mag_Axis_Type'(Pi / 2.0, X_Unit));
      Base_Unit_Scz2_A     : constant Cart_Vector_Type := Base_Rot_Mat_Sc1_Sc2 * Base_Unit_Scz1;
      Base_Rot_Mat_Sc0_Sc2 : constant Rot_Matrix_Type  := Base_Rot_Mat_Sc1_Sc2 * Base_Rot_Mat_Sc0_Sc1;
      Base_Unit_Scz2_B     : constant Cart_Vector_Type := Base_Rot_Mat_Sc0_Sc2 * Base_Unit_Scz0;

      Base_Unit_Sc_Sun : constant Cart_Vector_Type := (0.0, 0.0, 1.0);

      Sc0_Rot_Mat_Base : constant Rot_Matrix_Type  := Zero_Matrix;
      Sc0_Unit_Sc_Sun  : constant Cart_Vector_Type := Sc0_Rot_Mat_Base * Base_Unit_Sc_Sun;

      Sc1_Rot_Mat_Base : constant Rot_Matrix_Type  := Inverse (Base_Rot_Mat_Sc0_Sc1);
      Sc1_Unit_Sc_Sun  : constant Cart_Vector_Type := Sc1_Rot_Mat_Base * Base_Unit_Sc_Sun;

      Sc1_Rot_Mat_Sc1_Sc2 : constant Rot_Matrix_Type :=
        Sc1_Rot_Mat_Base * Base_Rot_Mat_Sc1_Sc2 * Inverse (Sc1_Rot_Mat_Base);

      Sc1_Rot_Sc1_Sc2 : constant Mag_Axis_Type   := To_Mag_Axis (Sc1_Rot_Mat_Sc1_Sc2);
      Sc2_Rot_Mat_Sc1 : constant Rot_Matrix_Type := Inverse (Sc1_Rot_Mat_Sc1_Sc2);
      Sc2_Rot_Sc1     : constant Mag_Axis_Type   := To_Mag_Axis (Sc2_Rot_Mat_Sc1);

      Sc2_Unit_Sc_Sun_A : constant Cart_Vector_Type := Sc2_Rot_Mat_Sc1 * Sc1_Unit_Sc_Sun;

      Sc2_Rot_Mat_Base_B : constant Rot_Matrix_Type  := Sc2_Rot_Mat_Sc1 * Sc1_Rot_Mat_Base;
      Sc2_Rot_Base_B     : constant Mag_Axis_Type    := To_Mag_Axis (Sc2_Rot_Mat_Base_B);
      Sc2_Unit_Sc_Sun_B  : constant Cart_Vector_Type := Sc2_Rot_Mat_Base_B * Base_Unit_Sc_Sun;

      Sc2_Rot_Mat_Base_C : constant Rot_Matrix_Type  := Inverse (Base_Rot_Mat_Sc0_Sc2);
      Sc2_Rot_Base_C     : constant Mag_Axis_Type    := To_Mag_Axis (Sc2_Rot_Mat_Base_C);
      Sc2_Unit_Sc_Sun_C  : constant Cart_Vector_Type := Sc2_Rot_Mat_Base_C * Base_Unit_Sc_Sun;

   begin
      Put_Line ("exam:rot_matrix");
      Put_Line ("Base_Rot_Mat_1_2 => "); Put (Base_Rot_Mat_1_2); New_Line;
      Put ("Base_Rot_1_2     => "); Put (Base_Rot_1_2); New_Line;
      Put ("Base_Quat_1_2    => "); Put (Base_Quat_1_2); New_Line;
      Put_Line ("Base_Rot_Mat_1_2_Q => "); Put (Base_Rot_Mat_1_2_Q); New_Line;
      Put ("Base_Tran_1      => "); Put (Base_Tran_1); New_Line;
      Put ("Base_Tran_2      => "); Put (Base_Tran_2); New_Line;
      Put_Line ("Base_Rot_Mat_2_3 => "); Put (Base_Rot_Mat_2_3); New_Line;
      Put_Line ("Base_Rot_Mat_1_3 => "); Put (Base_Rot_Mat_1_3); New_Line;
      Put ("Base_Rot_1_3     => "); Put (Base_Rot_1_3); New_Line;
      Put ("Base_Tran_3_A    => "); Put (Base_Tran_3_A); New_Line;
      Put ("Base_Tran_3_B    => "); Put (Base_Tran_3_B); New_Line;
      New_Line;

      Put ("Base_Unit_Scz0       => "); Put (Base_Unit_Scz0); New_Line;
      Put_Line ("Base_Rot_Mat_Sc0_Sc1 => "); Put (Base_Rot_Mat_Sc0_Sc1); New_Line;
      Put ("Base_Unit_Scz1       => "); Put (Base_Unit_Scz1); New_Line;
      Put_Line ("Base_Rot_Mat_Sc0_Sc2 => "); Put (Base_Rot_Mat_Sc0_Sc2); New_Line;
      Put ("Base_Unit_Scz2_A     => "); Put (Base_Unit_Scz2_A); New_Line;
      Put_Line ("Base_Rot_Mat_Sc0_Sc2 => "); Put (Base_Rot_Mat_Sc0_Sc2); New_Line;
      Put ("Base_Unit_Scz2_B     => "); Put (Base_Unit_Scz2_B); New_Line;
      New_Line;

      Put_Line ("Sc0_Rot_Mat_Base => "); Put (Sc0_Rot_Mat_Base); New_Line;
      Put ("Sc0_Unit_Sc_Sun  => "); Put (Sc0_Unit_Sc_Sun); New_Line;
      Put_Line ("Sc1_Rot_Mat_Base => "); Put (Sc1_Rot_Mat_Base); New_Line;
      Put ("Sc1_Unit_Sc_Sun  => "); Put (Sc1_Unit_Sc_Sun); New_Line;
      New_Line;
      Put_Line ("Sc1_Rot_Mat_Sc1_Sc2 => "); Put (Sc1_Rot_Mat_Sc1_Sc2); New_Line;
      Put ("Sc1_Rot_Sc1_Sc2     => "); Put (Sc1_Rot_Sc1_Sc2); New_Line;
      Put_Line ("Sc2_Rot_Mat_Sc1     => "); Put (Sc2_Rot_Mat_Sc1); New_Line;
      Put ("Sc2_Rot_Sc1         => "); Put (Sc2_Rot_Sc1); New_Line;
      Put ("Sc2_Unit_Sc_Sun_A   => "); Put (Sc2_Unit_Sc_Sun_A); New_Line;
      New_Line;
      Put_Line ("Sc2_Rot_Mat_Base_B => "); Put (Sc2_Rot_Mat_Base_B); New_Line;
      Put ("Sc2_Rot_Base_B     => "); Put (Sc2_Rot_Base_B); New_Line;
      Put ("Sc2_Unit_Sc_Sun_B  => "); Put (Sc2_Unit_Sc_Sun_B); New_Line;
      New_Line;
      Put_Line ("Sc2_Rot_Mat_Base_C => "); Put (Sc2_Rot_Mat_Base_C); New_Line;
      Put ("Sc2_Rot_Base_C     => "); Put (Sc2_Rot_Base_C); New_Line;
      Put ("Sc2_Unit_Sc_Sun_C  => "); Put (Sc2_Unit_Sc_Sun_C); New_Line;
      New_Line;
   end Rot_Matrix;

   Rotation_Rate_Left :
   declare
      use SAL.Math_Double.DOF_3.Left;
      ST_Unit_Boresight       : constant Unit_Vector_Type     := Z_Unit;
      ICRF_Quat_ST_T1         : constant Unit_Quaternion_Type := To_Unit_Quaternion (0.1, X);
      ICRF_Unit_Boresight_T1  : constant Unit_Vector_Type     := ICRF_Quat_ST_T1 * ST_Unit_Boresight;
      Delta_Theta             : constant Real_Type            := 0.1;
      Delta_T                 : constant Real_Type            := 0.5;
      ST_Rot_Rate_ST_True     : constant Cart_Vector_Type     := (Delta_Theta / Delta_T, 0.0, 0.0);
      ICRF_Quat_ST_T1_T2_True : constant Unit_Quaternion_Type := To_Unit_Quaternion (ST_Rot_Rate_ST_True * Delta_T);
      ST_T1_Quat_ST_T2_True   : constant Unit_Quaternion_Type := ICRF_Quat_ST_T1_T2_True;
      ICRF_Quat_ST_T2         : constant Unit_Quaternion_Type := ICRF_Quat_ST_T1 * ST_T1_Quat_ST_T2_True;
      ICRF_Unit_Boresight_T2  : constant Unit_Vector_Type     := ICRF_Quat_ST_T2 * ST_Unit_Boresight;
      ICRF_Quat_ST_T1_T2      : constant Unit_Quaternion_Type :=
        To_Unit_Quaternion (ICRF_Unit_Boresight_T1, ICRF_Unit_Boresight_T2);
      ST_T1_Quat_ST_T2_A      : constant Unit_Quaternion_Type := ICRF_Quat_ST_T1_T2;
      ST_T1_Quat_ST_T2_B      : constant Unit_Quaternion_Type := Inverse_Times (ICRF_Quat_ST_T1, ICRF_Quat_ST_T2);
      ST_Rot_Rate_ST_Comp     : constant Cart_Vector_Type     := To_Rot_Vector (ST_T1_Quat_ST_T2_B) / Delta_T;

      ST_Tran_ST_Lens     : constant Cart_Vector_Type := 0.2 * ST_Unit_Boresight;
      ST_Vel_Lens         : constant Cart_Vector_Type := Cross (ST_Rot_Rate_ST_True, ST_Tran_ST_Lens);
      ICRF_Rot_Rate_ST_T1 : constant Cart_Vector_Type := ICRF_Quat_ST_T1 * ST_Rot_Rate_ST_True;
      ICRF_Vel_Lens_T1_A  : constant Cart_Vector_Type := ICRF_Quat_ST_T1 * ST_Vel_Lens;
      ICRF_Tran_ST_Lens   : constant Cart_Vector_Type := ICRF_Quat_ST_T1 * ST_Tran_ST_Lens;
      ICRF_Vel_Lens_T1_B  : constant Cart_Vector_Type := Cross (ICRF_Rot_Rate_ST_T1, ICRF_Tran_ST_Lens);

      Sc_Quat_ST      : constant Unit_Quaternion_Type := To_Unit_Quaternion (Pi / 2.0, Z);
      Sc_Rot_Rate_Sc  : constant Cart_Vector_Type     := Sc_Quat_ST * ST_Rot_Rate_ST_True;
      Sc_Vel_Lens_A   : constant Cart_Vector_Type     := Sc_Quat_ST * ST_Vel_Lens;
      Sc_Tran_ST_Lens : constant Cart_Vector_Type     := Inverse_Times (Sc_Quat_ST, ST_Tran_ST_Lens);
      Sc_Vel_Lens_B   : constant Cart_Vector_Type     := Cross (Sc_Rot_Rate_Sc, Sc_Tran_ST_Lens);
   begin
      Put_Line ("exam:rotation_rate_left");
      Put ("ICRF_Quat_ST_T1         => "); Put (ICRF_Quat_ST_T1); New_Line;
      Put ("ICRF_Unit_Boresight_T1  => "); Put (ICRF_Unit_Boresight_T1); New_Line;
      Put ("ST_Rot_Rate_ST_True     => "); Put (ST_Rot_Rate_ST_True); New_Line;
      Put ("ICRF_Quat_ST_T1_T2_True => "); Put (ICRF_Quat_ST_T1_T2_True); New_Line;
      Put ("ST_T1_Quat_ST_T2_True   => "); Put (ST_T1_Quat_ST_T2_True); New_Line;
      Put ("ICRF_Quat_ST_T2         => "); Put (ICRF_Quat_ST_T2); New_Line;
      Put ("ICRF_Unit_Boresight_T2  => "); Put (ICRF_Unit_Boresight_T2); New_Line;
      Put ("ST_T1_Quat_ST_T2_A      => "); Put (ST_T1_Quat_ST_T2_A); New_Line;
      Put ("ST_T1_Quat_ST_T2_B      => "); Put (ST_T1_Quat_ST_T2_B); New_Line;
      Put ("ST_Rot_Rate_ST_Comp     => "); Put (ST_Rot_Rate_ST_Comp); New_Line;
      New_Line;

      Put ("ST_Tran_ST_Lens     => "); Put (ST_Tran_ST_Lens); New_Line;
      Put ("ST_Vel_Lens         => "); Put (ST_Vel_Lens); New_Line;
      Put ("ICRF_Rot_Rate_ST_T1 => "); Put (ICRF_Rot_Rate_ST_T1); New_Line;
      Put ("ICRF_Vel_Lens_T1_A  => "); Put (ICRF_Vel_Lens_T1_A); New_Line;
      Put ("ICRF_Tran_ST_Lens   => "); Put (ICRF_Tran_ST_Lens); New_Line;
      Put ("ICRF_Vel_Lens_T1_B  => "); Put (ICRF_Vel_Lens_T1_B); New_Line;
      New_Line;

      Put ("Sc_Quat_ST      => "); Put (Sc_Quat_ST); New_Line;
      Put ("Sc_Rot_Rate_Sc  => "); Put (Sc_Rot_Rate_Sc); New_Line;
      Put ("Sc_Vel_Lens_A   => "); Put (Sc_Vel_Lens_A); New_Line;
      Put ("Sc_Tran_ST_Lens => "); Put (Sc_Tran_ST_Lens); New_Line;
      Put ("Sc_Vel_Lens_B   => "); Put (Sc_Vel_Lens_B); New_Line;

      New_Line (2);
   end Rotation_Rate_Left;

   Rotation_Rate_Right_SAL :
   declare
      use SAL.Math_Double.DOF_3.Wertz;
      ST_Unit_Boresight       : constant Unit_Vector_Type     := Z_Unit;
      ST_T1_Quat_ICRF         : constant Unit_Quaternion_Type := To_Unit_Quaternion (0.1, X);
      Boresight_T1_Unit_ICRF  : constant Unit_Vector_Type     := ST_Unit_Boresight * ST_T1_Quat_ICRF;
      Delta_Theta             : constant Real_Type            := 0.1;
      Delta_T                 : constant Real_Type            := 0.5;
      ST_Rot_Rate_ST_True     : constant Cart_Vector_Type     := (Delta_Theta / Delta_T, 0.0, 0.0);
      ST_T1_T2_Quat_ICRF_True : constant Unit_Quaternion_Type := To_Unit_Quaternion (ST_Rot_Rate_ST_True * Delta_T);
      ST_T2_Quat_ST_T1_True   : constant Unit_Quaternion_Type := ST_T1_T2_Quat_ICRF_True;
      ST_T2_Quat_ICRF         : constant Unit_Quaternion_Type := ST_T2_Quat_ST_T1_True * ST_T1_Quat_ICRF;
      Boresight_T2_Unit_ICRF  : constant Unit_Vector_Type     := ST_Unit_Boresight * ST_T2_Quat_ICRF;
      ST_T1_T2_Quat_ICRF      : constant Unit_Quaternion_Type :=
        To_Unit_Quaternion (Boresight_T1_Unit_ICRF, Boresight_T2_Unit_ICRF);
      ST_T2_Quat_ST_T1_A      : constant Unit_Quaternion_Type := ST_T1_T2_Quat_ICRF;
      ST_T2_Quat_ST_T1_B      : constant Unit_Quaternion_Type := Times_Inverse (ST_T2_Quat_ICRF, ST_T1_Quat_ICRF);
      ST_Rot_Rate_ST_Comp     : constant Cart_Vector_Type     := To_Rot_Vector (ST_T2_Quat_ST_T1_B) / Delta_T;
   begin
      Put_Line ("exam:rotation_rate_right_sal");
      Put ("ST_T1_Quat_ICRF         => "); Put (ST_T1_Quat_ICRF); New_Line;
      Put ("Boresight_T1_Unit_ICRF  => "); Put (Boresight_T1_Unit_ICRF); New_Line;
      Put ("ST_Rot_Rate_ST_True     => "); Put (ST_Rot_Rate_ST_True); New_Line;
      Put ("ST_T1_T2_Quat_ICRF_True => "); Put (ST_T1_T2_Quat_ICRF_True); New_Line;
      Put ("ST_T2_Quat_ST_T1_True   => "); Put (ST_T2_Quat_ST_T1_True); New_Line;
      Put ("ST_T2_Quat_ICRF         => "); Put (ST_T2_Quat_ICRF); New_Line;
      Put ("Boresight_T2_Unit_ICRF  => "); Put (Boresight_T2_Unit_ICRF); New_Line;
      Put ("ST_T2_Quat_ST_T1_A      => "); Put (ST_T2_Quat_ST_T1_A); New_Line;
      Put ("ST_T2_Quat_ST_T1_B      => "); Put (ST_T2_Quat_ST_T1_B); New_Line;
      Put ("ST_Rot_Rate_ST_Comp     => "); Put (ST_Rot_Rate_ST_Comp); New_Line;
      New_Line;
   end Rotation_Rate_Right_SAL;

   Rotation_Rate_Right_GNC :
   declare
      use SAL.Math_Double.DOF_3.Wertz;
      ST_Unit_Boresight       : constant Unit_Vector_Type     := Z_Unit;
      ICRF_Quat_ST_T1         : constant Unit_Quaternion_Type := To_Unit_Quaternion (-0.1, X);
      Boresight_T1_Unit_ICRF  : constant Unit_Vector_Type     := ST_Unit_Boresight * Inverse (ICRF_Quat_ST_T1);
      Delta_Theta             : constant Real_Type            := 0.1;
      Delta_T                 : constant Real_Type            := 0.5;
      ST_Rot_Rate_ST_True     : constant Cart_Vector_Type     := (Delta_Theta / Delta_T, 0.0, 0.0);
      ST_T1_T2_Quat_ICRF_True : constant Unit_Quaternion_Type := To_Unit_Quaternion (ST_Rot_Rate_ST_True * Delta_T);
      ST_T1_Quat_ST_T2_True   : constant Unit_Quaternion_Type := Inverse (ST_T1_T2_Quat_ICRF_True);
      ICRF_Quat_ST_T2         : constant Unit_Quaternion_Type := ICRF_Quat_ST_T1 * ST_T1_Quat_ST_T2_True;
      Boresight_T2_Unit_ICRF  : constant Unit_Vector_Type     := ST_Unit_Boresight * Inverse (ICRF_Quat_ST_T2);
      ST_T1_T2_Quat_ICRF      : constant Unit_Quaternion_Type :=
        To_Unit_Quaternion (Boresight_T1_Unit_ICRF, Boresight_T2_Unit_ICRF);
      ST_T2_Quat_ST_T1_A      : constant Unit_Quaternion_Type := ST_T1_T2_Quat_ICRF;
      ST_T2_Quat_ST_T1_B      : constant Unit_Quaternion_Type := Inverse_Times (ICRF_Quat_ST_T2, ICRF_Quat_ST_T1);
      ST_Rot_Rate_ST_Comp     : constant Cart_Vector_Type     := To_Rot_Vector (ST_T2_Quat_ST_T1_B) / Delta_T;
   begin
      Put_Line ("exam:rotation_rate_right_gnc");
      Put ("ICRF_Quat_ST_T1         => "); Put (ICRF_Quat_ST_T1); New_Line;
      Put ("Boresight_T1_Unit_ICRF  => "); Put (Boresight_T1_Unit_ICRF); New_Line;
      Put ("ST_Rot_Rate_ST_True     => "); Put (ST_Rot_Rate_ST_True); New_Line;
      Put ("ST_T1_T2_Quat_ICRF_True => "); Put (ST_T1_T2_Quat_ICRF_True); New_Line;
      Put ("ST_T1_Quat_ST_T2_True   => "); Put (ST_T1_Quat_ST_T2_True); New_Line;
      Put ("ICRF_Quat_ST_T2         => "); Put (ICRF_Quat_ST_T2); New_Line;
      Put ("Boresight_T2_Unit_ICRF  => "); Put (Boresight_T2_Unit_ICRF); New_Line;
      Put ("ST_T2_Quat_ST_T1_A      => "); Put (ST_T2_Quat_ST_T1_A); New_Line;
      Put ("ST_T2_Quat_ST_T1_B      => "); Put (ST_T2_Quat_ST_T1_B); New_Line;
      Put ("ST_Rot_Rate_ST_Comp     => "); Put (ST_Rot_Rate_ST_Comp); New_Line;
      New_Line;
   end Rotation_Rate_Right_GNC;

   Pose_Left :
   declare
      use SAL.Math_Double.DOF_3.Left;
      use SAL.Math_Double.DOF_6.Left;
      BcsF_Pose_Gimbal_0     : constant Pose_Type := ((0.0, 2.0, 0.0), To_Unit_Quaternion (Pi / 2.0, Y));
      Gimbal_0_Pose_Gimbal_1 : constant Pose_Type := ((1.0, 0.0, 0.0), To_Unit_Quaternion (Pi / 2.0, X));
      BcsF_Pose_Gimbal_1     : constant Pose_Type := BcsF_Pose_Gimbal_0 * Gimbal_0_Pose_Gimbal_1;
   begin
      Put_Line ("exam:pose_left");
      Put ("BcsF_Pose_Gimbal_0     => "); Put (BcsF_Pose_Gimbal_0); New_Line;
      Put ("Gimbal_0_Pose_Gimbal_1 => "); Put (Gimbal_0_Pose_Gimbal_1); New_Line;
      Put ("BcsF_Pose_Gimbal_1     => "); Put (BcsF_Pose_Gimbal_1); New_Line;
      New_Line;
   end Pose_Left;

   Pose_Right :
   declare
      use SAL.Math_Double.DOF_3.Wertz;
      use SAL.Math_Double.DOF_6.Wertz;
      Gimbal_0_Pose_BcsF     : constant Pose_Type := ((0.0, 2.0, 0.0), To_Unit_Quaternion (Pi / 2.0, Y));
      Gimbal_1_Pose_Gimbal_0 : constant Pose_Type := ((1.0, 0.0, 0.0), To_Unit_Quaternion (Pi / 2.0, X));
      Gimbal_1_Pose_BcsF     : constant Pose_Type := Gimbal_1_Pose_Gimbal_0 * Gimbal_0_Pose_BcsF;
   begin
      Put_Line ("exam:pose_right");
      Put ("Gimbal_0_Pose_BcsF     => "); Put (Gimbal_0_Pose_BcsF); New_Line;
      Put ("Gimbal_1_Pose_Gimbal_0 => "); Put (Gimbal_1_Pose_Gimbal_0); New_Line;
      Put ("Gimbal_1_Pose_BcsF     => "); Put (Gimbal_1_Pose_BcsF); New_Line;
      New_Line;
   end Pose_Right;

   DOF_6_Rate_Left :
   declare
      use SAL.Math_Double.DOF_3.Left;
      use SAL.Math_Double.DOF_6.Left;
      Sc_Rate_Sc            : constant Dual_Cart_Vector_Type := (0.1, 0.2, 0.0, 0.0, 0.0, 0.3);
      Sc_Pose_Gimbal_0      : constant Pose_Type             := ((0.0, 0.0, 0.0), To_Unit_Quaternion (Pi / 2.0, Y));
      Gimbal_0_Rate_Sc_A    : constant Dual_Cart_Vector_Type := Transform_Rate (Sc_Pose_Gimbal_0, Sc_Rate_Sc);
      Gimbal_0_Rate_Trsf_Sc : constant Rate_Transform_Type   := To_Rate_Transform (Sc_Pose_Gimbal_0);
      Gimbal_0_Rate_Sc_B    : constant Dual_Cart_Vector_Type := Gimbal_0_Rate_Trsf_Sc * Sc_Rate_Sc;

      Gimbal_0_Pose_Gimbal_1      : constant Pose_Type             :=
        ((1.0, 0.0, 0.0), Zero_Unit_Quaternion); --  To_Unit_Quaternion (Pi / 2.0, X));
      Sc_Pose_Gimbal_1            : constant Pose_Type             := Sc_Pose_Gimbal_0 * Gimbal_0_Pose_Gimbal_1;
      Gimbal_1_Rate_Sc_A          : constant Dual_Cart_Vector_Type := Transform_Rate (Sc_Pose_Gimbal_1, Sc_Rate_Sc);
      Gimbal_1_Rate_Trsf_Sc_A     : constant Rate_Transform_Type   := To_Rate_Transform (Sc_Pose_Gimbal_1);
      Gimbal_1_Rate_Sc_BA         : constant Dual_Cart_Vector_Type := Gimbal_1_Rate_Trsf_Sc_A * Sc_Rate_Sc;
      Gimbal_1_Rate_Trsf_Gimbal_0 : constant Rate_Transform_Type   := To_Rate_Transform (Gimbal_0_Pose_Gimbal_1);
      Gimbal_1_Rate_Trsf_Sc_B     : constant Rate_Transform_Type   :=
        Gimbal_1_Rate_Trsf_Gimbal_0 * Gimbal_0_Rate_Trsf_Sc;
      Gimbal_1_Rate_Sc_BB         : constant Dual_Cart_Vector_Type := Gimbal_1_Rate_Trsf_Sc_B * Sc_Rate_Sc;

   begin
      Put_Line ("exam:dof_6_rate_left");
      Put_Line ("Sc_Rate_Sc");
      Put (Sc_Rate_Sc);
      New_Line;
      Put_Line ("Sc_Pose_Gimbal_0");
      Put (Sc_Pose_Gimbal_0);
      New_Line;
      Put_Line ("Gimbal_0_Rate_Sc_A");
      Put (Gimbal_0_Rate_Sc_A);
      New_Line;
      Put_Line ("Gimbal_0_Rate_Trsf_Sc");
      Put (Gimbal_0_Rate_Trsf_Sc, Single_Line_Record => False);
      New_Line;
      Put_Line ("Gimbal_0_Rate_Sc_B");
      Put (Gimbal_0_Rate_Sc_B);
      New_Line;
      New_Line;

      Put_Line ("Gimbal_0_Pose_Gimbal_1");
      Put (Gimbal_0_Pose_Gimbal_1);
      New_Line;
      Put_Line ("Sc_Pose_Gimbal_1");
      Put (Sc_Pose_Gimbal_1);
      New_Line;
      Put_Line ("Gimbal_1_Rate_Sc_A");
      Put (Gimbal_1_Rate_Sc_A);
      New_Line;
      Put_Line ("Gimbal_1_Rate_Trsf_Sc_A");
      Put (Gimbal_1_Rate_Trsf_Sc_A, Single_Line_Record => False);
      New_Line;
      Put_Line ("Gimbal_1_Rate_Sc_BA");
      Put (Gimbal_1_Rate_Sc_BA);
      New_Line;
      Put_Line ("Gimbal_1_Rate_Trsf_Gimbal_0");
      Put (Gimbal_1_Rate_Trsf_Gimbal_0, Single_Line_Record => False);
      New_Line;
      Put_Line ("Gimbal_1_Rate_Trsf_Sc_B");
      Put (Gimbal_1_Rate_Trsf_Sc_B, Single_Line_Record => False);
      New_Line;
      Put_Line ("Gimbal_1_Rate_Sc_BB");
      Put (Gimbal_1_Rate_Sc_BB);
      New_Line;

      New_Line (2);
   end DOF_6_Rate_Left;

   Aberration :
   declare
      use SAL.Math_Double.DOF_3.Wertz;

      --  GNC naming convention, just to prove it is confusing.

      Vel_ICRF_ScWrtSun : constant Cart_Vector_Type := (3.0e4, 0.0, 0.0);

      procedure Demo (Unit_ICRF_StarWrtSc : in Unit_Vector_Type)
      is
         Vel_ICRF_PhotonWrtSun : constant Cart_Vector_Type := -Speed_Of_Light * Unit_ICRF_StarWrtSc;

         --  Classical velocity transform
         Vel_Sci_PhotonWrtSun_1  : constant Cart_Vector_Type := Vel_ICRF_PhotonWrtSun - Vel_ICRF_ScWrtSun;
         Unit_Sci_PhotonWrtSun_1 : constant Unit_Vector_Type := To_Unit_Vector (Vel_Sci_PhotonWrtSun_1);
         Unit_Sci_StarWrtSc_1    : constant Unit_Vector_Type := -Unit_Sci_PhotonWrtSun_1;

         --  Full Lorentz transform
         Vel_Sci_PhotonWrtSun_2  : constant Cart_Vector_Type :=
           Lorentz_Transform (Vel_ICRF_PhotonWrtSun, -Vel_ICRF_ScWrtSun);
         Unit_Sci_PhotonWrtSun_2 : constant Unit_Vector_Type := To_Unit_Vector (Vel_Sci_PhotonWrtSun_2);
         Unit_Sci_StarWrtSc_2    : constant Unit_Vector_Type := -Unit_Sci_PhotonWrtSun_2;

         --  eqn 4.1.1-2
         Unit_Sci_StarWrtSc_3 : constant Unit_Vector_Type := Light_Vector_Transform
           (Unit_ICRF_StarWrtSc, -Vel_ICRF_ScWrtSun);

         --  eqn 4.1.1-4 Light_Vector_Rotation returns an active
         --  quaternion; Quat_IcrfToAb is passive. But to apply it we
         --  need the active form; this is _not_ a change of frame.
         Unit_ICRF_Bore    : constant Unit_Vector_Type     := Unit_ICRF_StarWrtSc;
         Icrf_Ab_Quat_Icrf : constant Unit_Quaternion_Type :=
           Light_Vector_Rotation (Unit_ICRF_Bore, Vel_ICRF_ScWrtSun); -- 4.1.1-4

         Quat_IcrfToAb : constant Unit_Quaternion_Type := Icrf_Ab_Quat_Icrf; -- 2.1.1-11, 2.3.1-24

         Unit_Sci_StarWrtSc_4 : constant Unit_Vector_Type := Unit_ICRF_StarWrtSc * Icrf_Ab_Quat_Icrf;

         S_ICRF_Ab : constant Cart_Vector_Type := Unit_Sci_StarWrtSc_2 - Unit_ICRF_StarWrtSc;
      begin
         Put ("Unit_ICRF_StarWrtSc     => "); Put (Unit_ICRF_StarWrtSc); New_Line;
         Put ("Vel_ICRF_ScWrtSun       => "); Put (Vel_ICRF_ScWrtSun); New_Line;
         Put ("Vel_ICRF_PhotonWrtSun   => "); Put (Vel_ICRF_PhotonWrtSun); New_Line;
         Put ("Vel_Sci_PhotonWrtSun_1  => "); Put (Vel_Sci_PhotonWrtSun_1); New_Line;
         Put ("Vel_Sci_PhotonWrtSun_2  => "); Put (Vel_Sci_PhotonWrtSun_2); New_Line;
         Put ("Unit_Sci_PhotonWrtSun_1 => "); Put (Unit_Sci_PhotonWrtSun_1); New_Line;
         Put ("Unit_Sci_PhotonWrtSun_2 => "); Put (Unit_Sci_PhotonWrtSun_2); New_Line;
         Put ("Unit_Sci_StarWrtSc_1    => "); Put (Unit_Sci_StarWrtSc_1); New_Line;
         Put ("Unit_Sci_StarWrtSc_2    => "); Put (Unit_Sci_StarWrtSc_2); New_Line;
         Put ("Unit_Sci_StarWrtSc_3    => "); Put (Unit_Sci_StarWrtSc_3); New_Line;
         Put ("Unit_Sci_StarWrtSc_4    => "); Put (Unit_Sci_StarWrtSc_4); New_Line;
         Put ("Icrf_Ab_Quat_Icrf       => "); Put (Icrf_Ab_Quat_Icrf); New_Line;
         Put ("Quat_IcrfToAb           => "); Put (Quat_IcrfToAb); New_Line;
         Put ("S_ICRF_Ab               => "); Put (S_ICRF_Ab); New_Line;
         New_Line;
      end Demo;

   begin
      SAL.Math_Double.Text_IO.Real_Text_IO.Default_Aft := 9;
      SAL.Math_Double.Text_IO.Real_Text_IO.Default_Exp := 0;

      SAL.Math_Double.DOF_3.Text_IO.Cart_Vector_Text_IO.Default_Aft := 9;
      SAL.Math_Double.DOF_3.Text_IO.Cart_Vector_Text_IO.Default_Exp := 0;

      SAL.Math_Double.DOF_3.Text_IO.Set_Unit_Vector_Text_IO_Default_Aft (9);
      SAL.Math_Double.DOF_3.Text_IO.Set_Unit_Vector_Text_IO_Default_Exp (0);

      Demo (Z_Unit);

      Demo (Rotate (Pi / 4.0, Y, Z_Unit));

      Demo (Rotate (-Pi/4.0, Y, Z_Unit));

      New_Line (2);
   end Aberration;

end Spacecraft_Math_Examples;
