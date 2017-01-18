--  Abstract :
--
--  See spec
--
--  Copyright (C) 2005, 2007 - 2008 Stephen Leake.  All Rights Reserved.
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
with SAL.Math_Double.Scalar;     use SAL.Math_Double.Scalar;
with SAL.Math_Float.DOF_3.Left;  use SAL.Math_Float.DOF_3.Left;
with SAL.Math_Float.DOF_3.AUnit; use SAL.Math_Float.DOF_3.AUnit;
with SAL.Math_Float.DOF_6.Left;  use SAL.Math_Float.DOF_6.Left;
with SAL.Math_Float.DOF_6.AUnit; use SAL.Math_Float.DOF_6.AUnit;
with SAL.Math_Float.AUnit;       use SAL.Math_Float.AUnit;
package body Test_Math_Float_DOF_6_Left is
   use SAL.Math_Float;
   use SAL.Math_Float.DOF_3;
   use SAL.Math_Float.DOF_6;

   function Transpose (Item : in DC_Array_DCV_Type) return DC_Array_DCV_Type
      --  Inherited from original Text_IO test, where it was used to
      --  make output more readable
   is
      Result : DC_Array_DCV_Type;
   begin
      for I in Dual_Cart_Axis_Type
      loop
         for J in Dual_Cart_Axis_Type
         loop
            Result (J) (I) := Item (I) (J);
         end loop;
      end loop;
      return Result;
   end Transpose;

   ----------
   --  Test routines

   procedure Test_Dual_Cart_Vector (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      A_Dual_Cart_Vector : constant Dual_Cart_Vector_Type := (1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
      A_Quaternion       : constant Unit_Quaternion_Type  := To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit));
   begin
      Check
        ("Quat * DCV",
         A_Quaternion * A_Dual_Cart_Vector,
         (1.00000,  1.69051,  3.18468,  4.00000,  4.37602,  6.46919));

      Check
        ("Inverse(Quat) * DCV",
         Inverse (A_Quaternion) * A_Dual_Cart_Vector,
         (1.00000,  2.28951,  2.78535,  4.00000,  5.57402,  5.47086));

      Check
        ("Inverse_Times (Quat, DCV)",
         Inverse_Times (A_Quaternion, A_Dual_Cart_Vector),
         (1.00000,  2.28951,  2.78535,  4.00000,  5.57402,  5.47086));
   end Test_Dual_Cart_Vector;

   procedure Test_Pose_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      A_Quaternion : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit));
      A_Pose       : constant Pose_Type            := ((1.0, 2.0, 3.0), A_Quaternion);
   begin
      Check
        ("pose",
         A_Pose,
         ((1.0, 2.0, 3.0), To_Unit_Quaternion (0.04998, 0.00000, 0.00000, 0.99875)));

      Check
        ("To_Dual_Cart_Vector",
         To_Dual_Cart_Vector (A_Pose),
         (1.00000,  2.00000,  3.00000,  0.10000,  0.00000,  0.00000));

      Check ("and back", To_Pose (To_Dual_Cart_Vector (A_Pose)), A_Pose);

      Check ("Mag", Mag (A_Pose), (3.74166,  0.10000));

      Check
        ("Inverse",
         Inverse (A_Pose),
         ((-1.00000, -2.28951, -2.78535), To_Unit_Quaternion (-0.04998, 0.00000, 0.00000,  0.99875)));

      Check ("Pose * Inverse", A_Pose * Inverse (A_Pose), Zero_Pose);

      Check ("Inverse_Times (Pose, Pose)", Inverse_Times (A_Pose, A_Pose), Zero_Pose);
   end Test_Pose_1;

   procedure Test_Pose_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      A_Quaternion : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit));
      A_Pose       : constant Pose_Type            := ((1.0, 2.0, 3.0), A_Quaternion);

      Left  : constant Pose_Type := A_Pose;
      Right : constant Pose_Type := A_Pose;
   begin
      Check
        ("Left * Right",
         Left * Right,
         ((2.00000,  3.69051,  6.18468), To_Unit_Quaternion (0.09983,  0.00000,  0.00000,  0.99500)));

      --  Perform these checks in opposite order from _wertz, to make the results be the same.
      Check
        ("Left.Rot * Right",
         Left.Rotation * Right,
         ((1.00000,  1.69051,  3.18468), To_Unit_Quaternion (0.09983,  0.00000,  0.00000,  0.99500)));

      Check
        ("Left * Right.Rot",
         Left * Right.Rotation,
         ((1.00000,  2.00000,  3.00000), To_Unit_Quaternion (0.09983,  0.00000,  0.00000,  0.99500)));

      Check
        ("(Left * Right.Tran).tran",
         Cart_Vector_Type'(Left * Right.Translation),
         (2.00000,  3.69051,  6.18468));

      Check ("Inverse (Left) * Right", Inverse (Left) * Right, Zero_Pose);

      Check ("Inverse_Times (Left, Right)", Inverse_Times (Left, Right), Zero_Pose);
   end Test_Pose_2;

   procedure Test_Pose_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Base_T_One : constant Pose_Type := ((1.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit)));
      Base_T_Two : constant Pose_Type := ((1.0, 0.1, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(-0.2, X_Unit)));

      One_T_Two  : constant Dual_Cart_Vector_Type := Base_T_Two - Base_T_One;
   begin
      Check
        ("1 Base_T_Two - Base_T_One",
         One_T_Two,
         To_Dual_Cart_Vector (Inverse (Base_T_One) * Base_T_Two));

      Check
        ("2 Base_T_Two - Base_T_One",
         One_T_Two,
         (0.0, 0.0995, -0.00998, -0.3, 0.0, 0.0));

      Check
        ("Base_T_One + One_T_Two ",
         Base_T_One + One_T_Two,
         Base_T_Two);

      Check
        ("Base_T_One + One_T_Two.Tran",
         Pose_Type'(Base_T_One + Translation (One_T_Two)),
         (Base_T_Two.Translation, Base_T_One.Rotation));

      Check
        ("(1.0, 0.0, 0.0) + Base_T_Two",
         Cart_Vector_Type'(1.0, 0.0, 0.0) + Base_T_Two,
         ((2.0, 0.1, 0.0), Base_T_Two.Rotation));

      Check
        ("Base_T_Two - One_T_Two ",
         Base_T_Two - One_T_Two,
         Base_T_One);

      Check
        ("One_T_Two + Two_T_Base",
         One_T_Two + Inverse (Base_T_Two),
         Inverse (Base_T_One));

   end Test_Pose_3;

   procedure Test_Pose_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      A_Quaternion : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit));
      A_Pose       : constant Pose_Type            := ((1.0, 2.0, 3.0), A_Quaternion);

      Base_T_One : constant Pose_Type             := A_Pose;
      Base_T_Two : constant Pose_Type             := A_Pose;
      One_T_Two  : constant Dual_Cart_Vector_Type := Base_T_Two - Base_T_One;
   begin
      Check
        ("Base_T_Two - Base_T_One",
         One_T_Two,
         To_Dual_Cart_Vector (Inverse (Base_T_One) * Base_T_Two));

      Check
        ("Base_T_Two - Base_T_One",
         One_T_Two,
         (0.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.00000));

      Check
        ("Base_T_One + One_T_Two ",
         Base_T_One + One_T_Two,
         Base_T_Two);

      Check
        ("Base_T_One + One_T_Two.Tran",
         Pose_Type'(Base_T_One + Translation (One_T_Two)),
         (Base_T_Two.Translation, Base_T_One.Rotation));

      Check
        ("(1.0, 0.0, 0.0) + Base_T_Two",
         Cart_Vector_Type'(1.0, 0.0, 0.0) + Base_T_Two,
         ((2.0, 2.0, 3.0), Base_T_Two.Rotation));

      Check
        ("Base_T_Two - One_T_Two ",
         Base_T_Two - One_T_Two,
         Base_T_One);

      Check
        ("One_T_Two + Two_T_Base",
         One_T_Two + Inverse (Base_T_Two),
         Inverse (Base_T_One));

   end Test_Pose_4;

   procedure Test_Pose_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      X_Quaternion : constant Unit_Quaternion_Type  := To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit));
      A_T_B        : constant Pose_Type             := ((1.0, 0.0, 0.0), X_Quaternion);

      Y_Quaternion : constant Unit_Quaternion_Type  := To_Unit_Quaternion (Mag_Axis_Type'(0.1, Y_Unit));
      B_T_C        : constant Pose_Type             := ((0.0, 1.0, 0.0), Y_Quaternion);
   begin
      Check
        ("A_T_B * B_T_C",
         A_T_B * B_T_C,
         ((1.0, 0.995, 0.0998), To_Unit_Quaternion (0.049917525, 0.049917525, 0.0024980004, 0.9975015625)));

   end Test_Pose_5;

   procedure Test_Rate_Wrench_Transform_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use DC_Array_DCV_Ops;

      X_Quaternion : constant Unit_Quaternion_Type  := To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit));
      A_T_B        : constant Pose_Type             := ((1.0, 0.0, 0.0), X_Quaternion);
      B_Tr_A       : constant Rate_Transform_Type   := To_Rate_Transform (A_T_B);
      B_Tw_A       : constant Wrench_Transform_Type := To_Wrench_Transform (A_T_B);

      Y_Quaternion : constant Unit_Quaternion_Type  := To_Unit_Quaternion (Mag_Axis_Type'(0.1, Y_Unit));
      B_T_C        : constant Pose_Type             := ((0.0, 1.0, 0.0), Y_Quaternion);
      C_Tr_B       : constant Rate_Transform_Type   := To_Rate_Transform (B_T_C);
      C_Tw_B       : constant Wrench_Transform_Type := To_Wrench_Transform (B_T_C);

      A_T_C  : constant Pose_Type             := A_T_B * B_T_C;
      C_Tr_A : constant Rate_Transform_Type   := To_Rate_Transform (A_T_C);
      C_Tw_A : constant Wrench_Transform_Type := To_Wrench_Transform (A_T_C);
   begin
      --  To_Rate_Transform (Pose)
      Check
        ("B_Tr_A",
         B_Tr_A,
         Unchecked_Rate_Transform
           (((1.00000,  0.00000,  0.00000),
             (0.00000,  0.99500,  0.09983),
             (0.00000, -0.09983,  0.99500)),
            ((0.00000,  0.00000,  0.00000),
             (0.00000, -0.09983,  0.99500),
             (0.00000, -0.99500, -0.09983))));

      Check
        ("C_Tr_B",
         C_Tr_B,
         Unchecked_Rate_Transform
           (((0.99500,  0.00000, -0.09983),
             (0.00000,  1.00000,  0.00000),
             (0.09983,  0.00000, 0.99500)),
            ((-0.09983,  0.00000, -0.99500),
             (0.00000, 0.00000,  0.00000),
             (0.99500, 0.00000, -0.09983))));

      --  To_Wrench_Transform (Pose)
      Check
        ("B_Tw_A",
         B_Tw_A,
         Unchecked_Wrench_Transform
           (((1.00000,  0.00000,  0.00000),
             (0.00000,  0.99500,  0.09983),
             (0.00000, -0.09983,  0.99500)),
            ((0.00000,  0.00000,  0.00000),
             (0.00000, -0.09983,  0.99500),
             (0.00000, -0.99500, -0.09983))));

      Check
        ("C_Tw_B",
         C_Tw_B,
         Unchecked_Wrench_Transform
           (((0.99500,  0.00000, -0.09983),
             (0.00000,  1.00000,  0.00000),
             (0.09983,  0.00000, 0.99500)),
            ((-0.09983,  0.00000, -0.99500),
             (0.00000, 0.00000,  0.00000),
             (0.99500, 0.00000, -0.09983))));

      --  To_Rate_Transform (Tran, Rot_Matrix)
      Check
        ("To_Rate_Transform (Tran, Rot_Matrix) B_Tr_A",
         To_Rate_Transform (A_T_B.Translation, To_Rot_Matrix (A_T_B.Rotation)),
         B_Tr_A);

      Check
        ("To_Rate_Transform (Tran, Rot_Matrix) C_Tr_B",
         To_Rate_Transform (B_T_C.Translation, To_Rot_Matrix (B_T_C.Rotation)),
         C_Tr_B);

      --  To_Wrench_Transform (Tran, Rot_Matrix)
      Check
        ("To_Wrench_Transform (Tran, Rot_Matrix) B_Tw_A",
         To_Wrench_Transform (A_T_B.Translation, To_Rot_Matrix (A_T_B.Rotation)),
         B_Tw_A);

      --  To_Wrench_Transform (Tran, Rot_Matrix)
      Check
        ("To_Wrench_Transform (Tran, Rot_Matrix) C_Tw_B",
         To_Wrench_Transform (B_T_C.Translation, To_Rot_Matrix (B_T_C.Rotation)),
         C_Tw_B);

      --  To_DC_Array_DCV
      Check
        ("To_DC_Array_DCV (B_Tr_A)",
         To_DC_Array_DCV (B_Tr_A),
         ((1.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.99500,  0.09983,  0.00000, -0.09983,  0.99500),
          (0.00000, -0.09983,  0.99500,  0.00000, -0.99500, -0.09983),
          (0.00000,  0.00000,  0.00000,  1.00000,  0.00000,  0.00000),
          (0.00000,  0.00000,  0.00000,  0.00000,  0.99500,  0.09983),
          (0.00000,  0.00000,  0.00000,  0.00000, -0.09983,  0.99500)));

      Check
        ("To_DC_Array_DCV (B_Tw_A)",
         To_DC_Array_DCV (B_Tw_A),
         ((1.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.99500,  0.09983,  0.00000,  0.00000,  0.00000),
          (0.00000, -0.09983,  0.99500,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.00000,  0.00000,  1.00000,  0.00000,  0.00000),
          (0.00000, -0.09983,  0.99500,  0.00000,  0.99500,  0.09983),
          (0.00000, -0.99500, -0.09983,  0.00000, -0.09983,  0.99500)));

      --  Rate_Transform * Rate_Transform
      Check
        ("C_Tr_B * B_Tr_A",
         C_Tr_B * B_Tr_A,
         C_Tr_A);

      --  Rate_Transform * Dc_array_dcv
      Check
        ("C_Tr_B * To_DC_Array_DCV (B_Tr_A)",
         C_Tr_B * To_DC_Array_DCV (B_Tr_A),
         To_DC_Array_DCV (C_Tr_A));

      --  Dc_array_dcv * Rate_Transform
      Check
        ("To_DC_Array_DCV (C_Tr_B) * B_Tr_A",
         To_DC_Array_DCV (C_Tr_B) * B_Tr_A,
         To_DC_Array_DCV (C_Tr_A));

      --  Wrench_Transform * Wrench_Transform
      Check
        ("C_Tw_B * B_Tw_A",
         C_Tw_B * B_Tw_A,
         C_Tw_A);

      --  Wrench_Transform * dc_array_dcv
      Check
        ("B_Tw_A * To_DC_Array_DCV (C_Tw_B)",
         B_Tw_A * To_DC_Array_DCV (C_Tw_B),
        ((0.99500,  0.00000, -0.09983,  0.00000,  0.00000,  0.00000),
         (0.00997,  0.99500,  0.09933,  0.00000,  0.00000,  0.00000),
         (0.09933, -0.09983,  0.99003,  0.00000,  0.00000,  0.00000),
         (-0.09983,  0.00000, -0.99500,  0.99500,  0.00000, -0.09983),
         (0.19867, -0.09983,  0.98007,  0.00997,  0.99500,  0.09933),
         (0.98007, -0.99500, -0.19867,  0.09933, -0.09983,  0.99003)));

      --  dc_array_dcv * Wrench_Transform
      Check
        ("To_DC_Array_DCV (B_Tw_A) * C_Tw_B",
         To_DC_Array_DCV (B_Tw_A) * C_Tw_B,
         ((0.99500,  0.00000, -0.09983,  0.00000,  0.00000,  0.00000),
          (0.00997,  0.99500,  0.09933,  0.00000,  0.00000,  0.00000),
          (0.09933, -0.09983,  0.99003,  0.00000,  0.00000,  0.00000),
          (-0.09983,  0.00000, -0.99500,  0.99500,  0.00000, -0.09983),
          (0.19867, -0.09983,  0.98007,  0.00997,  0.99500,  0.09933),
          (0.98007, -0.99500, -0.19867,  0.09933, -0.09983,  0.99003)));

   end Test_Rate_Wrench_Transform_1;

   procedure Test_Rate_Wrench_Transform_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Xform     : constant Pose_Type := ((1.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit)));
      Xform_Rot : constant Pose_Type := ((0.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit)));

      Rate_Transform   : constant Rate_Transform_Type   := To_Rate_Transform (Xform);
      Wrench_Transform : constant Wrench_Transform_Type := To_Wrench_Transform (Xform);

   begin
      --  This (and similar tests following) is inherited from the
      --  Text_IO based tests, where it was easier to compare results
      --  as a transpose.
      Check
        ("1 matrix from Rate_Transform * DCV",
         Transpose
           ((Rate_Transform * (1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
             Rate_Transform * (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
             Rate_Transform * (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
             Rate_Transform * (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
             Rate_Transform * (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
             Rate_Transform * (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))),
         ((1.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.99500,  0.09983,  0.00000, -0.09983,  0.99500),
          (0.00000, -0.09983,  0.99500,  0.00000, -0.99500, -0.09983),
          (0.00000,  0.00000,  0.00000,  1.00000,  0.00000,  0.00000),
          (0.00000,  0.00000,  0.00000,  0.00000,  0.99500,  0.09983),
          (0.00000,  0.00000,  0.00000,  0.00000, -0.09983,  0.99500)));

      Check
        ("1 matrix from Transform_Rate (Xform, DCV)",
         Transpose
           ((Transform_Rate (Xform, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)))),
         ((1.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.99500,  0.09983,  0.00000, -0.09983,  0.99500),
          (0.00000, -0.09983,  0.99500,  0.00000, -0.99500, -0.09983),
          (0.00000,  0.00000,  0.00000,  1.00000,  0.00000,  0.00000),
          (0.00000,  0.00000,  0.00000,  0.00000,  0.99500,  0.09983),
          (0.00000,  0.00000,  0.00000,  0.00000, -0.09983,  0.99500)));

      Check
        ("1 matrix from Transform_Rate (Xform.Rot, DCV)",
         DC_Array_DCV_Type'
         (Transform_Rate (Xform.Rotation, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))),
         (Transform_Rate (Xform_Rot, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))));

      Check
        ("1 matrix from Wrench_Transform * DCV",
         Transpose
           ((Wrench_Transform * (1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))),
         ((1.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.99500,  0.09983,  0.00000,  0.00000,  0.00000),
          (0.00000, -0.09983,  0.99500,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.00000,  0.00000,  1.00000,  0.00000,  0.00000),
          (0.00000, -0.09983,  0.99500,  0.00000,  0.99500,  0.09983),
          (0.00000, -0.99500, -0.09983,  0.00000, -0.09983,  0.99500)));

      Check
        ("1 matrix from Transform_Wrench (Xform, DCV)",
         Transpose
           ((Transform_Wrench (Xform, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)))),
         ((1.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.99500,  0.09983,  0.00000,  0.00000,  0.00000),
          (0.00000, -0.09983,  0.99500,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.00000,  0.00000,  1.00000,  0.00000,  0.00000),
          (0.00000, -0.09983,  0.99500,  0.00000,  0.99500,  0.09983),
          (0.00000, -0.99500, -0.09983,  0.00000, -0.09983,  0.99500)));

      Check
        ("1 matrix from Transform_Wrench (Xform.Rot, DCV)",
         DC_Array_DCV_Type'
         (Transform_Wrench (Xform.Rotation, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))),
         (Transform_Wrench (Xform_Rot, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))));

   end Test_Rate_Wrench_Transform_2;

   procedure Test_Rate_Wrench_Transform_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Xform     : constant Pose_Type := ((1.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, Y_Unit)));
      Xform_Rot : constant Pose_Type := ((0.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, Y_Unit)));

      Rate_Transform   : constant Rate_Transform_Type   := To_Rate_Transform (Xform);
      Wrench_Transform : constant Wrench_Transform_Type := To_Wrench_Transform (Xform);

   begin
      Check
        ("2 matrix from Rate_Transform * DCV",
         Transpose
           ((Rate_Transform * (1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
             Rate_Transform * (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
             Rate_Transform * (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
             Rate_Transform * (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
             Rate_Transform * (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
             Rate_Transform * (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))),
         ((0.99500,  0.00000, -0.09983,  0.00000,  0.09983,  0.00000),
          (0.00000,  1.00000,  0.00000,  0.00000,  0.00000,  1.00000),
          (0.09983,  0.00000,  0.99500,  0.00000, -0.99500,  0.00000),
          (0.00000,  0.00000,  0.00000,  0.99500,  0.00000, -0.09983),
          (0.00000,  0.00000,  0.00000,  0.00000,  1.00000,  0.00000),
          (0.00000,  0.00000,  0.00000,  0.09983,  0.00000,  0.99500)));

      Check
        ("2 matrix from Transform_Rate (Xform, DCV)",
         Transpose
           ((Transform_Rate (Xform, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)))),
         ((0.99500,  0.00000, -0.09983,  0.00000,  0.09983,  0.00000),
          (0.00000,  1.00000,  0.00000,  0.00000,  0.00000,  1.00000),
          (0.09983,  0.00000,  0.99500,  0.00000, -0.99500,  0.00000),
          (0.00000,  0.00000,  0.00000,  0.99500,  0.00000, -0.09983),
          (0.00000,  0.00000,  0.00000,  0.00000,  1.00000,  0.00000),
          (0.00000,  0.00000,  0.00000,  0.09983,  0.00000,  0.99500)));

      Check
        ("2 matrix from Transform_Rate (Xform.Rot, DCV)",
         DC_Array_DCV_Type'
         (Transform_Rate (Xform.Rotation, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
          Transform_Rate (Xform.Rotation, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))),
         (Transform_Rate (Xform_Rot, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
          Transform_Rate (Xform_Rot, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))));

      Check
        ("2 matrix from Wrench_Transform * DCV",
         Transpose
           ((Wrench_Transform * (1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))),
         ((0.99500,  0.00000, -0.09983,  0.00000,  0.00000,  0.00000),
          (0.00000,  1.00000,  0.00000,  0.00000,  0.00000,  0.00000),
          (0.09983,  0.00000,  0.99500,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.09983,  0.00000,  0.99500,  0.00000, -0.09983),
          (0.00000,  0.00000,  1.00000,  0.00000,  1.00000,  0.00000),
          (0.00000, -0.99500,  0.00000,  0.09983,  0.00000,  0.99500)));

      Check
        ("2 matrix from Transform_Wrench (Xform, DCV)",
         Transpose
           ((Transform_Wrench (Xform, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)))),
         ((0.99500,  0.00000, -0.09983,  0.00000,  0.00000,  0.00000),
          (0.00000,  1.00000,  0.00000,  0.00000,  0.00000,  0.00000),
          (0.09983,  0.00000,  0.99500,  0.00000,  0.00000,  0.00000),
          (0.00000,  0.09983,  0.00000,  0.99500,  0.00000, -0.09983),
          (0.00000,  0.00000,  1.00000,  0.00000,  1.00000,  0.00000),
          (0.00000, -0.99500,  0.00000,  0.09983,  0.00000,  0.99500)));

      Check
        ("2 matrix from Transform_Wrench (Xform.Rot, DCV)",
         DC_Array_DCV_Type'
         (Transform_Wrench (Xform.Rotation, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
          Transform_Wrench (Xform.Rotation, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))),
         (Transform_Wrench (Xform_Rot, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
          Transform_Wrench (Xform_Rot, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0))));

   end Test_Rate_Wrench_Transform_3;

   procedure Test_Dual_Mag_Axis (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      A_Dual_Mag_Axis : constant Dual_Mag_Axis_Type := (Translation => (4.0, X_Unit), Rotation => (2.0, Y_Unit));
   begin
      Check
        ("Pose",
         To_Pose (A_Dual_Mag_Axis),
         ((4.00000,  0.00000,  0.00000), To_Unit_Quaternion (0.00000,  0.84147,  0.00000,  0.54030)));

      Check
        (" and back",
         To_Dual_Mag_Axis (To_Pose (A_Dual_Mag_Axis)),
         ((4.00000, To_Unit_Vector (1.00000,  0.00000,  0.00000)),
          (2.00000, To_Unit_Vector (0.00000,  1.00000,  0.00000))));

   end Test_Dual_Mag_Axis;

   procedure Test_Mass_Type (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      A_Mass : constant Mass_Type := To_Mass
         (Total          => 2.0,
          Center         => (5.0, 0.0, 0.0),
          Center_Inertia => (2.0, 0.0, 0.0, 0.0, 0.0, 0.0));

      Another_Mass : constant Mass_Type := To_Mass
         (Total          => 2.0,
          Center         => (5.0, 0.0, 0.0),
          Center_Inertia => (2.0, 3.0, 4.0, 0.0, 0.0, 0.0));

      procedure Test_Change_Frame
        (Label            : in String;
         Current_T_New    : in Pose_Type;
         Current_Mass     : in Mass_Type;
         Current_Inertia  : in Inertia_Type;
         Expected_Mass    : in Mass_Type;
         Expected_Inertia : in Inertia_Type)
      is
         New_Mass : constant Mass_Type := Current_T_New * Current_Mass;
      begin
         Check (Label & " Inertia", Inertia (Current_Mass), Current_Inertia);
         Check (Label & " New Mass", New_Mass, Expected_Mass);
         Check (Label & " New Inertia", Inertia (New_Mass), Expected_Inertia);
      end Test_Change_Frame;

      procedure Test_Add
        (Label           : in String;
         Base            : in Mass_Type;
         Child           : in Mass_Type;
         Base_Pose_Child : in Pose_Type;
         Expected_Sum    : in Mass_Type)
      is
         Sum : constant Mass_Type := Add (Base, Child, Base_Pose_Child);
      begin
         Check (Label & " Sum", Sum, Expected_Sum);
         Check (Label & " Sum - Child", Subtract (Sum, Child, Base_Pose_Child), Base);
      end Test_Add;

   begin
      Test_Change_Frame
        ("1",
         Current_T_New    => Zero_Pose,
         Current_Mass     => Another_Mass,
         Current_Inertia  => (2.00000, 53.00000, 54.00000,  0.00000,  0.00000,  0.00000),
         Expected_Mass    => To_Mass
           (2.00000, (5.00000,  0.00000,  0.00000), (2.00000,  3.00000,  4.00000,  0.00000,  0.00000,  0.00000)),
         Expected_Inertia => (2.00000, 53.00000, 54.00000,  0.00000,  0.00000,  0.00000));

      Test_Change_Frame
        ("2",
         Current_T_New    => ((1.0, 2.0, 0.0), Zero_Unit_Quaternion),
         Current_Mass     => Another_Mass,
         Current_Inertia  => (2.00000, 53.00000, 54.00000,  0.00000,  0.00000,  0.00000),
         Expected_Mass    => To_Mass
           (2.00000, (6.00000,  2.00000,  0.00000), (2.00000,  3.00000,  4.00000,  0.00000,  0.00000,  0.00000)),
         Expected_Inertia => (10.00000, 75.00000, 84.00000, -24.00000,  0.00000,  0.00000));

      Test_Change_Frame
        ("3",
         Current_T_New    => ((0.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, Z_Unit))),
         Current_Mass     => A_Mass,
         Current_Inertia  => (2.0, 50.0, 50.0,  0.0,  0.0,  0.0),
         Expected_Mass    => To_Mass (2.0, (4.97502,  0.49917,  0.0), (1.98007,  0.01993,  0.0, 0.19867,  0.0,  0.0)),
         Expected_Inertia => (2.47840, 49.52160, 50.0, -4.76806,  0.0,  0.0));

      Test_Change_Frame
        ("4",
         Current_T_New    => ((1.0, 2.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, Z_Unit))),
         Current_Mass     => Another_Mass,
         Current_Inertia  => (2.00000, 53.00000, 54.00000,  0.00000,  0.00000,  0.00000),
         Expected_Mass    => To_Mass (2.0, (5.97502,  2.49917, 0.0), (2.00997,  2.99003,  4.0, -0.09933,  0.0, 0.0)),
         Expected_Inertia => (14.50169, 74.39178, 87.89342, -29.96448,  0.00000,  0.00000));

      Test_Change_Frame
        ("Pi/2",
         Current_T_New    => ((0.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(Half_Pi, Z_Unit))),
         Current_Mass     => To_Mass (2.0, (0.0, 0.0, 0.0), (2.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
         Current_Inertia  => (2.0, 0.0, 0.0, 0.0, 0.0, 0.0),
         Expected_Mass    => To_Mass
           (2.0, (0.0, 0.0, 0.0), (0.0, 2.0,  0.0, 0.0, 0.0, 0.0)),
         Expected_Inertia => (0.0, 2.0,  0.0, 0.0, 0.0, 0.0));

      Test_Add
        ("5",
         Base            => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
         Child           => To_Mass (1.0, (0.0, 1.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
         Base_Pose_Child => Zero_Pose,
         Expected_Sum    => To_Mass
           (3.00000, (0.66667,  0.33333,  0.00000), (2.66667,  2.66667,  3.33333,  0.66667,  0.00000,  0.00000)));

      Test_Add
        ("6",
         Base            => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
         Child           => To_Mass (1.0, (0.0, 1.0, 0.0), (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
         Base_Pose_Child => Zero_Pose,
         Expected_Sum    => To_Mass
           (3.00000, (0.66667,  0.33333,  0.00000), (2.66667,  4.66667,  7.33333,  8.66667, 10.00000, 12.00000)));

      Test_Add
        ("7",
         Base            => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
         Child           => To_Mass (1.0, (0.0, 1.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
         Base_Pose_Child => ((1.0, 2.0, 0.0), Zero_Unit_Quaternion),
         Expected_Sum    => To_Mass
           (3.00000, (1.00000,  1.00000,  0.00000), (8.00000,  2.00000,  8.00000,  0.00000,  0.00000,  0.00000)));

      Test_Add
        ("8",
         Base            => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
         Child           => To_Mass (1.0, (0.0, 1.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
         Base_Pose_Child => ((0.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit))),
         Expected_Sum    => To_Mass
           (3.00000, (0.66667,  0.33167,  0.03328), (2.66667,  2.67331,  3.32669,  0.66334,  0.06656, -0.06622)));

   end Test_Mass_Type;

   ----------
   --  Public routines

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Dual_Cart_Vector'Access, "Test_Dual_Cart_Vector");
      Register_Routine (T, Test_Pose_1'Access, "Test_Pose_1");
      Register_Routine (T, Test_Pose_2'Access, "Test_Pose_2");
      Register_Routine (T, Test_Pose_3'Access, "Test_Pose_3");
      Register_Routine (T, Test_Pose_4'Access, "Test_Pose_4");
      Register_Routine (T, Test_Pose_5'Access, "Test_Pose_5");
      Register_Routine (T, Test_Dual_Mag_Axis'Access, "Test_Dual_Mag_Axis");
      Register_Routine (T, Test_Rate_Wrench_Transform_1'Access, "Test_Rate_Wrench_Transform_1");
      Register_Routine (T, Test_Rate_Wrench_Transform_2'Access, "Test_Rate_Wrench_Transform_2");
      Register_Routine (T, Test_Rate_Wrench_Transform_3'Access, "Test_Rate_Wrench_Transform_3");
      Register_Routine (T, Test_Mass_Type'Access, "Test_Mass_Type");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_DOF_6_Left");
   end Name;

   procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 10.0e-5;
   end Set_Up_Case;

   procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Float_DOF_6_Left;
