--  Abstract :
--
--  See spec.
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


with SAL.Math_Float.AUnit;       use SAL.Math_Float.AUnit;
with SAL.Math_Float.DOF_3.AUnit; use SAL.Math_Float.DOF_3.AUnit;
with SAL.Math_Float.DOF_3.Left;  use SAL.Math_Float.DOF_3.Left;
with SAL.Math_Float.Scalar;      use SAL.Math_Float.Scalar;

package body Test_Math_Float_DOF_3_Left is

   use SAL.Math_Float;
   use SAL.Math_Float.DOF_3;

   ----------
   --  Test cases

   procedure Unit_Vector (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Au : constant Unit_Vector_Type     := +((0.26726, 0.53452, 0.80178));
      Aq : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(Pi / 2.0, +((1.0, 0.0, 0.0))));
   begin
      Check ("quaternion times", Aq * Au, 0.26726,  -0.80178,   0.53452);
   end Unit_Vector;

   procedure To_Mag_Axis (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Sc_N_Sun_Desired : constant Unit_Vector_Type     := Z_Unit;
      Mag_Axis         : constant Mag_Axis_Type        := (0.1, X_Unit);
      Rot              : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis);
      Sc_N_Sun         : constant Unit_Vector_Type     := Rot * Sc_N_Sun_Desired;
   begin
      --  Verify definition.
      Check ("1", To_Mag_Axis (Sc_N_Sun_Desired, Sc_N_Sun), Mag_Axis, Tolerance => 1.0e-6);
   end To_Mag_Axis;

   procedure Quaternion_Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Au : constant Unit_Vector_Type := +((0.26726, 0.53452, 0.80178));

      Ma_2 : constant Mag_Axis_Type := (0.1, Au);
      Ma_3 : constant Mag_Axis_Type := (Pi / 2.0, Au);

      Q_2 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Ma_2);
      Q_3 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Ma_3);
      Q_6 : constant Unit_Quaternion_Type := To_Unit_Quaternion (2.0, 3.0, 4.0, 1.0);

      R_2 : constant Cart_Vector_Type := To_Rot_Vector (Q_2);
      R_3 : constant Cart_Vector_Type := To_Rot_Vector (Q_3);
   begin
      Check ("Mag_Axis_To_Quat 2",   Q_2, 0.01336,   0.02671,   0.04007,   0.99875);
      Check ("Quat_To_Mag_Axis 2",   To_Mag_Axis (Q_2), Ma_2);
      Check ("Quat_To_Rot_Vector 2", R_2, (0.02673,   0.05345,   0.08018));
      Check ("Rot_Vector_To_Quat 2", To_Unit_Quaternion (R_2), Q_2);

      Check ("Mag_Axis_To_Quat 3",   Q_3, 0.18898,   0.37796,   0.56695,   0.70711);
      Check ("Quat_To_Mag_Axis 3",   To_Mag_Axis (Q_3), Ma_3);
      Check ("Quat_To_Rot_Vector 3", R_3, (0.41981,   0.83963,   1.25944));
      Check ("Rot_Vector_To_Quat 3", To_Unit_Quaternion (R_3), Q_3);

      Check ("To_Quat (0.1, X)", To_Unit_Quaternion (0.1, X), 0.04998,   0.00000,   0.00000,   0.99875);
      Check ("To_Quat (0.1, Y)", To_Unit_Quaternion (0.1, Y), 0.00000,   0.04998,   0.00000,   0.99875);
      Check ("To_Quat (0.1, Z)", To_Unit_Quaternion (0.1, Z), 0.00000,   0.00000,   0.04998,   0.99875);

      Check ("X_Axis", To_Cart_Vector (X_Axis (Q_6)), (-0.66667,   0.66667,   0.33333));
      Check ("Y_Axis", To_Cart_Vector (Y_Axis (Q_6)), (0.13333,  -0.33333,   0.93333));
      Check ("Z_Axis", To_Cart_Vector (Z_Axis (Q_6)), (0.73333,   0.66667,   0.13333));

   end Quaternion_Nominal;

   procedure Quaternion_Zero (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Au  : constant Unit_Vector_Type     := +((0.26726, 0.53452, 0.80178));
      Q_1 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(0.0, Au));
      R_1 : constant Cart_Vector_Type     := To_Rot_Vector (Q_1);
   begin
      Check ("Mag_To_Quat 1",        Q_1, 0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Quat_To_Mag_Axis 1",   To_Mag_Axis (Q_1), 0.00000, 1.0,   0.0,   0.0);
      Check ("Quat_To_Rot_Vector 1", R_1, (0.00000,   0.00000,   0.00000));
      Check ("Rot_Vector_To_Quat 1", To_Unit_Quaternion (R_1), 0.00000,   0.00000,   0.00000,   1.00000);
   end Quaternion_Zero;

   procedure Quaternion_Pi (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Au  : constant Unit_Vector_Type     := +((0.26726, 0.53452, 0.80178));
      Q_1 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(Pi - First_Order_Trig, Au));
      Q_2 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(Pi + First_Order_Trig, Au));
      Q_3 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(Pi, X_Unit));
      Q_4 : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(Pi - First_Order_Trig, X_Unit));

      R_1 : constant Cart_Vector_Type := To_Rot_Vector (Q_1);
      R_2 : constant Cart_Vector_Type := To_Rot_Vector (Q_2);
      R_3 : constant Cart_Vector_Type := To_Rot_Vector (Q_3);
      R_4 : constant Cart_Vector_Type := To_Rot_Vector (Q_4);

   begin
      Check ("Mag_Axis_To_Quat 1",   Q_1, 0.26726,   0.53452,   0.80178,   0.00017);
      Check ("Quat_To_Mag_Axis 1",   To_Mag_Axis (Q_1), 3.14125, 0.26726,   0.53452,   0.80178);
      Check ("Quat_To_Rot_Vector 1", R_1, (0.83953,   1.67907,   2.51860));
      Check ("Rot_Vector_To_Quat 1", To_Unit_Quaternion (R_1), 0.26726,   0.53452,   0.80178,   0.00017);

      Check ("Mag_Axis_To_Quat 2",   Q_2, 0.26726,   0.53452,   0.80178,  -0.00017);
      Check ("Quat_To_Mag_Axis 2",   To_Mag_Axis (Q_2), 3.14125, -0.26726, -0.53452, -0.80178);
      Check ("Quat_To_Rot_Vector 2", R_2, (-0.83953,  -1.67907,  -2.51860));
      Check ("Rot_Vector_To_Quat 2", To_Unit_Quaternion (R_2), -0.26726,  -0.53452,  -0.80178,   0.00017);

      Check ("Mag_Axis_To_Quat 3",   Q_3, 1.0,   0.0,   0.0,  0.0);
      Check ("Quat_To_Mag_Axis 3",   Fold_Pi (To_Mag_Axis (Q_3)), Pi, 1.0, 0.0, 0.0);
      Check ("Quat_To_Rot_Vector 3", Fold_Pi (R_3), (Pi, 0.0, 0.0));
      Check ("Rot_Vector_To_Quat 3", To_Unit_Quaternion (R_3), 1.0, 0.0, 0.0, 0.0);

      Check ("Mag_Axis_To_Quat 4",   Q_4, 1.0, 0.0,  0.0,   0.00017);
      Check ("Quat_To_Mag_Axis 4",   To_Mag_Axis (Q_4), 3.14125, 1.0, 0.0, 0.0);
      Check ("Quat_To_Rot_Vector 4", R_4, (Pi - First_Order_Trig, 0.0, 0.0));
      Check ("Rot_Vector_To_Quat 4", To_Unit_Quaternion (R_4), 1.0, 0.0, 0.0, 0.00017);

   end Quaternion_Pi;

   procedure Cart_Vector (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      C_3 : constant Cart_Vector_Type := (2.0, 3.0, 4.0);

      Q_1 : constant Unit_Quaternion_Type := To_Unit_Quaternion (0.1, 0.0, 0.0, 0.9);
      Q_2 : constant Unit_Quaternion_Type := To_Unit_Quaternion (0.0, 0.1, 0.0, 0.9);
      Q_3 : constant Unit_Quaternion_Type := To_Unit_Quaternion (0.0, 0.0, 0.1, 0.9);
      Q_4 : constant Unit_Quaternion_Type := To_Unit_Quaternion (0.2, 0.3, 0.4, 0.1);

   begin
      Check ("Q * C                1",                Q_1  * C_3,  (2.00000,   2.04878,   4.56098));
      Check ("Inverse (Q) * C      1", Inverse       (Q_1) * C_3,  (2.00000,   3.80488,   3.24390));
      Check ("Inverse_Times (Q, C) 1", Inverse_Times (Q_1,   C_3), (2.00000,   3.80488,   3.24390));

      Check ("Q * C                2",                Q_2  * C_3,  (2.82927,   3.00000,   3.46341));
      Check ("Inverse (Q) * C      2", Inverse       (Q_2) * C_3,  (1.07317,   3.00000,   4.34146));
      Check ("Inverse_Times (Q, C) 2", Inverse_Times (Q_2,   C_3), (1.07317,   3.00000,   4.34146));

      Check ("Q * C                3",                Q_3  * C_3,  (1.29268,   3.36585,   4.00000));
      Check ("Inverse (Q) * C      3", Inverse       (Q_3) * C_3,  (2.60976,   2.48780,   4.00000));
      Check ("Inverse_Times (Q, C) 3", Inverse_Times (Q_3,   C_3), (2.60976,   2.48780,   4.00000));

      Check ("Q * C                4",                Q_4  * C_3,  (2.00000,   3.00000,   4.00000));
      Check ("Inverse (Q) * C      4", Inverse       (Q_4) * C_3,  (2.00000,   3.00000,   4.00000));
      Check ("Inverse_Times (Q, C) 4", Inverse_Times (Q_4,   C_3), (2.00000,   3.00000,   4.00000));

      Check ("To_Quat * C      X", To_Unit_Quaternion (0.1,  X) * C_3,  (2.00000,   2.58568,   4.27952));

      Check ("To_Quat * C      Y", To_Unit_Quaternion (0.1,  Y) * C_3,  (2.38934,   3.00000,   3.78035));

      Check ("To_Quat * C      Z", To_Unit_Quaternion (0.1,  Z) * C_3,  (1.69051,   3.18468,   4.00000));
   end Cart_Vector;

   procedure ZYX_Euler (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      E_1 : constant ZYX_Euler_Type := (0.0,  0.0,  0.0);
      E_2 : constant ZYX_Euler_Type := (0.1,  0.0,  0.0);
      E_3 : constant ZYX_Euler_Type := (0.0,  0.1,  0.0);
      E_4 : constant ZYX_Euler_Type := (0.0,  0.0,  0.1);
      E_5 : constant ZYX_Euler_Type := (0.1,  0.1,  0.0);
      E_6 : constant ZYX_Euler_Type := (0.0,  0.1,  0.1);
      E_7 : constant ZYX_Euler_Type := (0.1,  0.1,  0.1);
      E_8 : constant ZYX_Euler_Type := (3.5,  0.0,  0.0); --  Q.S < 0.0

      Q_1 : constant Unit_Quaternion_Type := To_Unit_Quaternion (E_1);
      Q_2 : constant Unit_Quaternion_Type := To_Unit_Quaternion (E_2);
      Q_3 : constant Unit_Quaternion_Type := To_Unit_Quaternion (E_3);
      Q_4 : constant Unit_Quaternion_Type := To_Unit_Quaternion (E_4);
      Q_5 : constant Unit_Quaternion_Type := To_Unit_Quaternion (E_5);
      Q_6 : constant Unit_Quaternion_Type := To_Unit_Quaternion (E_6);
      Q_7 : constant Unit_Quaternion_Type := To_Unit_Quaternion (E_7);
      Q_8 : constant Unit_Quaternion_Type := To_Unit_Quaternion (E_8);

   begin
      Check ("ZYX_To_Quat 1", To_Unit_Quaternion (E_1), 0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Quat_To_ZYX 1", To_ZYX_Euler       (Q_1), 0.00000,   0.00000,   0.00000);

      Check ("ZYX_To_Quat 2", To_Unit_Quaternion (E_2), 0.00000,   0.00000,   0.04998,   0.99875);
      Check ("Quat_To_ZYX 2", To_ZYX_Euler       (Q_2), 0.10000,   0.00000,   0.00000);

      Check ("ZYX_To_Quat 3", To_Unit_Quaternion (E_3), 0.00000,   0.04998,   0.00000,   0.99875);
      Check ("Quat_To_ZYX 3", To_ZYX_Euler       (Q_3), 0.00000,   0.10000,   0.00000);

      Check ("ZYX_To_Quat 4", To_Unit_Quaternion (E_4), 0.04998,   0.00000,   0.00000,   0.99875);
      Check ("Quat_To_ZYX 4", To_ZYX_Euler       (Q_4), 0.00000,   0.00000,   0.10000);

      Check ("ZYX_To_Quat 5", To_Unit_Quaternion (E_5), -0.00250,   0.04992,   0.04992,   0.99750);
      Check ("Quat_To_ZYX 5", To_ZYX_Euler       (Q_5), 0.10000,   0.10000,   0.00000);

      Check ("ZYX_To_Quat 6", To_Unit_Quaternion (E_6), 0.04992,   0.04992,  -0.00250,   0.99750);
      Check ("Quat_To_ZYX 6", To_ZYX_Euler       (Q_6), 0.00000,   0.10000,   0.10000);

      Check ("ZYX_To_Quat 7", To_Unit_Quaternion (E_7), 0.04736,   0.05235,   0.04736,   0.99638);
      Check ("Quat_To_ZYX 7", To_ZYX_Euler       (Q_7), 0.10000,   0.10000,   0.10000);

      Check ("ZYX_To_Quat 8", To_Unit_Quaternion (E_8), 0.00000,   0.00000,   0.98399,  -0.17825);
      Check ("Quat_To_ZYX 8", To_ZYX_Euler       (Q_8), -2.78319,   0.00000,  -0.00000);

   end ZYX_Euler;

   procedure Rot_Matrix_Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      RM_1  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(0.1, +(1.0, 2.0, 3.0)));
      RM_2  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(0.1, X_Unit));
      RM_3  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(0.1, Y_Unit));
      RM_4  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(0.1, Z_Unit));
      RM_5  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(Pi / 2.0, +(1.0, 2.0, 3.0)));
      RM_6  : constant Rot_Matrix_Type := Unchecked_Rot_Matrix
        (((1.0, 0.0,  0.0),
          (0.0, 0.0, -1.0),
          (0.0, 1.0,  0.0)));

      Q_1  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_1);
      Q_2  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_2);
      Q_3  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_3);
      Q_4  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_4);
      Q_5  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_5);
      Q_6  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_6);

   begin
      Check ("Mag_Axis to Rot_Matrix 1",                     RM_1,  ((0.99536,  -0.07933,   0.05443),
                                                                     (0.08076,   0.99643,  -0.02454),
                                                                     (-0.05229,   0.02882,   0.99822)));
      Check ("Rot_Matrix to Mag_Axis 1", To_Mag_Axis        (RM_1),  0.10000,  0.26726,   0.53452,   0.80178);
      Check ("Rot_Matrix to Quat     1", To_Unit_Quaternion (RM_1),  0.01336,   0.02671,   0.04007,   0.99875);
      Check ("Quat to Rot_Matrix     1", To_Rot_Matrix       (Q_1), ((0.99536,  -0.07933,   0.05443),
                                                                     (0.08076,   0.99643,  -0.02454),
                                                                     (-0.05229,   0.02882,   0.99822)));

      Check ("Mag_Axis to Rot_Matrix 2",                     RM_2,  ((1.00000,   0.00000,   0.00000),
                                                                     (0.00000,   0.99500,  -0.09983),
                                                                     (0.00000,   0.09983,   0.99500)));
      Check ("Rot_Matrix to Mag_Axis 2", To_Mag_Axis        (RM_2),  0.10000,  1.00000,   0.00000,   0.00000);
      Check ("Rot_Matrix to Quat     2", To_Unit_Quaternion (RM_2),  0.04998,   0.00000,   0.00000,   0.99875);
      Check ("Quat to Rot_Matrix     2", To_Rot_Matrix       (Q_2), ((1.00000,   0.00000,   0.00000),
                                                                     (0.00000,   0.99500,  -0.09983),
                                                                     (0.00000,   0.09983,   0.99500)));

      Check ("Mag_Axis to Rot_Matrix 3",                     RM_3,  ((0.99500,   0.00000,   0.09983),
                                                                     (0.00000,   1.00000,   0.00000),
                                                                     (-0.09983,   0.00000,   0.99500)));
      Check ("Rot_Matrix to Mag_Axis 3", To_Mag_Axis        (RM_3),  0.10000,  0.00000,   1.00000,   0.00000);
      Check ("Rot_Matrix to Quat     3", To_Unit_Quaternion (RM_3),  0.00000,   0.04998,   0.00000,   0.99875);
      Check ("Quat to Rot_Matrix     3", To_Rot_Matrix       (Q_3), ((0.99500,   0.00000,   0.09983),
                                                                     (0.00000,   1.00000,   0.00000),
                                                                     (-0.09983,   0.00000,   0.99500)));

      Check ("Mag_Axis to Rot_Matrix 4",                     RM_4,  ((0.99500,  -0.09983,   0.00000),
                                                                     (0.09983,   0.99500,   0.00000),
                                                                     (0.00000,   0.00000,   1.00000)));
      Check ("Rot_Matrix to Mag_Axis 4", To_Mag_Axis        (RM_4),  0.10000,  0.00000,   0.00000,   1.00000);
      Check ("Rot_Matrix to Quat     4", To_Unit_Quaternion (RM_4),  0.00000,   0.00000,   0.04998,   0.99875);
      Check ("Quat to Rot_Matrix     4", To_Rot_Matrix       (Q_4), ((0.99500,  -0.09983,   0.00000),
                                                                     (0.09983,   0.99500,   0.00000),
                                                                     (0.00000,   0.00000,   1.00000)));

      Check ("Mag_Axis to Rot_Matrix 5",                     RM_5,  ((0.07143,  -0.65893,   0.74881),
                                                                     (0.94464,   0.28571,   0.16131),
                                                                     (-0.32024,   0.69583,   0.64286)));
      Check ("Rot_Matrix to Mag_Axis 5", To_Mag_Axis        (RM_5),  1.57080,  0.26726,   0.53452,   0.80178);
      Check ("Rot_Matrix to Quat     5", To_Unit_Quaternion (RM_5),  0.18898,   0.37796,   0.56695,   0.70711);
      Check ("Quat to Rot_Matrix     5", To_Rot_Matrix       (Q_5), ((0.07143,  -0.65893,   0.74881),
                                                                     (0.94464,   0.28571,   0.16131),
                                                                     (-0.32024,   0.69583,   0.64286)));

      Check ("Rot_Matrix to Mag_Axis 6", To_Mag_Axis   (RM_6),  1.57080,  1.0, 0.0, 0.0);
      Check ("Rot_Matrix to Quat     6",                Q_6,  0.70711,   0.0,   0.0,   0.70711);
      Check ("Quat to Rot_Matrix     6", To_Rot_Matrix (Q_6), RM_6);

   end Rot_Matrix_Nominal;

   procedure Rot_Matrix_Zero (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      RM_1  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(0.0, +(1.0, 2.0, 3.0)));
      RM_2  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(First_Order_Trig, X_Unit));

      Q_1  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_1);
      Q_2  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_2);
   begin
      Check ("Mag_Axis to Rot_Matrix 1", RM_1, ((1.00000,   0.00000,   0.00000),
                                                (0.00000,   1.00000,   0.00000),
                                                (0.00000,   0.00000,   1.00000)));
      Check ("Rot_Matrix to Mag_Axis 1", To_Mag_Axis (RM_1), 0.00000, 1.00000,   0.00000,   0.00000);
      Check ("Rot_Matrix to Quat     1", To_Unit_Quaternion (RM_1), 0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Quat to Rot_Matrix     1", To_Rot_Matrix (Q_1), ((1.00000,   0.00000,   0.00000),
                                                               (0.00000,   1.00000,   0.00000),
                                                               (0.00000,   0.00000,   1.00000)));

      Check ("Mag_Axis to Rot_Matrix 2",                     RM_2,  ((1.00000,   0.00000,   0.00000),
                                                                     (0.00000,   1.00000,  -0.00035),
                                                                     (0.00000,   0.00035,   1.00000)));
      Check ("Rot_Matrix to Mag_Axis 2", To_Mag_Axis        (RM_2),  0.00035,  1.00000,   0.00000,   0.00000);
      Check ("Rot_Matrix to Quat     2", To_Unit_Quaternion (RM_2),  0.00017,   0.00000,   0.00000,   1.00000);
      Check ("Quat to Rot_Matrix     2", To_Rot_Matrix       (Q_2), ((1.00000,   0.00000,   0.00000),
                                                                     (0.00000,   1.00000,  -0.00035),
                                                                     (0.00000,   0.00035,   1.00000)));
   end Rot_Matrix_Zero;

   procedure Rot_Matrix_Pi (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      RM_0_X  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(Pi, X_Unit));
      RM_0_XY : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(Pi, +(1.0, First_Order_Trig, 0.0)));
      --  IMPROVEME: test other axes
--        RM_0_Y  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(Pi, Y_Unit));
--        RM_0_Z  : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(Pi, Z_Unit));
      RM_1    : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(Pi - First_Order_Trig, X_Unit));
      RM_2    : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(Pi + First_Order_Trig, X_Unit));
      RM_3    : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(Pi - First_Order_Trig, +(1.0, 2.0, 3.0)));
      RM_4    : constant Rot_Matrix_Type := To_Rot_Matrix (Mag_Axis_Type'(Pi + First_Order_Trig, +(1.0, 2.0, 3.0)));

      Q_0_X  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_0_X);
      Q_0_XY  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_0_XY);
      Q_1  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_1);
      Q_2  : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_2);
      Q_3 : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_3);
      Q_4 : constant Unit_Quaternion_Type := To_Unit_Quaternion (RM_4);

   begin
      Check ("Mag_Axis to Rot_Matrix 0 X",                 RM_0_X,  ((1.00000,   0.00000,   0.00000),
                                                                     (0.00000,  -1.00000,   0.00000),
                                                                     (0.00000,   0.00000,  -1.00000)));
      Check ("Rot_Matrix to Mag_Axis 0 X", Fold_Pi (To_Mag_Axis (RM_0_X)),  3.14159,  1.00000,  0.0,  0.0);
      Check ("Rot_Matrix to Quat     0 X", To_Unit_Quaternion (RM_0_X),  1.0,  0.0,  0.0, 0.0);
      Check ("Quat to Rot_Matrix     0 X", To_Rot_Matrix   (Q_0_X), RM_0_X);

      Check ("Mag_Axis to Rot_Matrix 0 XY",                RM_0_XY, ((1.00000,   0.00069,   0.00000),
                                                                     (0.00069,  -1.00000,   0.00000),
                                                                     (0.00000,   0.00000,  -1.00000)));
      Check ("Rot_Matrix to Mag_Axis 0 XY", Fold_Pi (To_Mag_Axis (RM_0_XY)),  3.14159,  1.00000,  0.00035,  0.0);
      Check ("Rot_Matrix to Quat     0 XY", To_Unit_Quaternion (RM_0_XY),  1.0,  0.00035,  0.0, 0.0);
      Check ("Quat to Rot_Matrix     0 XY", To_Rot_Matrix (Q_0_XY), RM_0_XY);

      Check ("Mag_Axis to Rot_Matrix 1",                     RM_1,  ((1.00000,   0.00000,   0.00000),
                                                                     (0.00000,  -1.00000,  -0.00035),
                                                                     (0.00000,   0.00035,  -1.00000)));
      Check ("Rot_Matrix to Mag_Axis 1", To_Mag_Axis        (RM_1),  3.14125,  1.00000,  0.0,  0.0);
      Check ("Rot_Matrix to Quat     1", To_Unit_Quaternion (RM_1),  1.0,  0.0,  0.0, 0.00017);
      Check ("Quat to Rot_Matrix     1", To_Rot_Matrix       (Q_1), RM_1);

      Check ("Mag_Axis to Rot_Matrix 2",                     RM_2,  ((1.00000,   0.00000,   0.00000),
                                                                     (0.00000,  -1.00000,   0.00035),
                                                                     (0.00000,  -0.00035,  -1.00000)));
      Check ("Rot_Matrix to Mag_Axis 2", To_Mag_Axis        (RM_2),  3.14125,  -1.00000,   0.00000,   0.00000);
      Check ("Rot_Matrix to Quat     2", To_Unit_Quaternion (RM_2),  1.00000,  0.0,  0.0,   -0.00017);
      Check ("Quat to Rot_Matrix     2", To_Rot_Matrix       (Q_2), RM_2);

      Check ("Mag_Axis to Rot_Matrix 3",                     RM_3,  ((-0.85714,   0.28544,   0.42876),
                                                                     (0.28599,  -0.42857,   0.85705),
                                                                     (0.42839,   0.85724,   0.28571)));
      Check ("Rot_Matrix to Mag_Axis 3", To_Mag_Axis        (RM_3),  3.14125,  0.26726,   0.53452,   0.80178);
      Check ("Rot_Matrix to Quat     3", To_Unit_Quaternion (RM_3),  0.26726,   0.53452,   0.80178,   0.00000,
             Tolerance => 2.0e-04);
      Check ("Quat to Rot_Matrix     3", To_Rot_Matrix       (Q_3), RM_3, Tolerance => 3.0e-03);

      Check ("Mag_Axis to Rot_Matrix 4",                     RM_4,  ((-0.85714,   0.28599,   0.42839),
                                                                     (0.28544,  -0.42857,   0.85724),
                                                                     (0.42876,   0.85705,   0.28571)));
      Check ("Rot_Matrix to Mag_Axis 4", To_Mag_Axis        (RM_4),  3.14125,  -0.26726,   -0.53452,   -0.80178);
      Check ("Rot_Matrix to Quat     4", To_Unit_Quaternion (RM_4),  0.26726,   0.53452,   0.80178,   0.00000,
             Tolerance => 2.0e-04);
      Check ("Quat to Rot_Matrix     4", To_Rot_Matrix       (Q_4), RM_4, Tolerance => 3.0e-03);
   end Rot_Matrix_Pi;

   procedure Inertia (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use CACV_Ops;
      Right : constant Inertia_Type         := (11.0, 12.0, 13.0, 14.0, 15.0, 16.0);
      MA_1  : constant Mag_Axis_Type        := (0.1, +(0.2, 0.3, 0.4));
      Q_1   : constant Unit_Quaternion_Type := To_Unit_Quaternion (MA_1);
      RM_1  : constant Rot_Matrix_Type      := To_Rot_Matrix (MA_1);
   begin
      Check ("quat * Right", Q_1 * Right, (10.54238,  12.89815,  12.55947,  14.22896,  14.43602,  16.29466));
      Check ("Rot_Matrix * Right", RM_1 * Right, (10.54238,  12.89816,  12.55947,  14.22896,  14.43602,  16.29466));
   end Inertia;

   ----------
   --  Public routines

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Unit_Vector'Access, "Unit_Vector");
      Register_Routine (T, To_Mag_Axis'Access, "To_Mag_Axis");
      Register_Routine (T, Quaternion_Nominal'Access, "Quaternion_Nominal");
      Register_Routine (T, Quaternion_Zero'Access, "Quaternion_Zero");
      Register_Routine (T, Quaternion_Pi'Access, "Quaternion_Pi");
      Register_Routine (T, Cart_Vector'Access, "Cart_Vector");
      Register_Routine (T, ZYX_Euler'Access, "ZYX_Euler");
      Register_Routine (T, Rot_Matrix_Nominal'Access, "Rot_Matrix_Nominal");
      Register_Routine (T, Rot_Matrix_Zero'Access, "Rot_Matrix_Zero");
      Register_Routine (T, Rot_Matrix_Pi'Access, "Rot_Matrix_Pi");
      Register_Routine (T, Inertia'Access, "Inertia");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_DOF_3_Left");
   end Name;

   procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 1.0e-4;
   end Set_Up_Case;

   procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Float_DOF_3_Left;
