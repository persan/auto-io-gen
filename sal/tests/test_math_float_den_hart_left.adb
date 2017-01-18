--  Abstract:
--
--  see spec
--
--  Copyright (C) 2003, 2005 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with SAL.Math_Float.AUnit;          use SAL.Math_Float;
with SAL.Math_Float.Scalar;         use SAL.Math_Float.Scalar;
with SAL.Math_Float.DOF_3;          use SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_3.Left;     use SAL.Math_Float.DOF_3.Left;
with SAL.Math_Float.DOF_6;          use SAL.Math_Float.DOF_6;
with SAL.Math_Float.DOF_6.AUnit;    use SAL.Math_Float.DOF_6.AUnit;
with SAL.Math_Float.DOF_6.Left;     use SAL.Math_Float.DOF_6.Left;
with SAL.Math_Float.Den_Hart;       use SAL.Math_Float.Den_Hart;
with SAL.Math_Float.Den_Hart.Left;  use SAL.Math_Float.Den_Hart.Left;
package body Test_Math_Float_Den_Hart_Left is

   procedure Test_Den_Hart (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_One
        (Message                   : in String;
         Den_Hart                  : in Den_Hart_Type;
         Position                  : in Real_Type;
         Expected_Pose             : in Pose_Type;
         Expected_Partial_Jacobian : in Dual_Cart_Vector_Type)
      is begin
         Check (Message & ".Pose", To_Pose (Den_Hart, Position), Expected_Pose);
         Check (Message & ".Inverse Pose", To_Inverse_Pose (Den_Hart, Position), Inverse (Expected_Pose));
         Check (Message & ".Rate Transform", To_Rate_Transform (Den_Hart, Position), To_Rate_Transform (Expected_Pose));
         Check (Message & ".Inverse Rate Transform",
                To_Inverse_Rate_Transform (Den_Hart, Position), To_Rate_Transform (Inverse (Expected_Pose)));
         Check (Message & ".Wrench Transform",
                To_Wrench_Transform (Den_Hart, Position), To_Wrench_Transform (Expected_Pose));
         Check (Message & ".Inverse Wrench Transform",
                To_Inverse_Wrench_Transform (Den_Hart, Position), To_Wrench_Transform (Inverse (Expected_Pose)));
         Check (Message & ".Partial Jacobian",
                Partial_Jacobian (To_Pose (Den_Hart, Position)), Expected_Partial_Jacobian);
      end Test_One;

   begin

      Test_One
        ("1",
         (REVOLUTE, 0.0, Sin_Cos (0.0), 0.0),
         0.0,
         Expected_Pose             => Zero_Pose,
         Expected_Partial_Jacobian => (0.00000,   0.00000,  -0.00000,   0.00000,   0.00000,   1.00000));

      Test_One
        ("2",
         (REVOLUTE, 0.1, Sin_Cos (0.0), 0.0),
         0.0,
         Expected_Pose =>
           ((0.10000,  -0.00000,   0.00000), To_Unit_Quaternion (0.00000, 0.00000, 0.00000, 1.00000)),
         Expected_Partial_Jacobian =>
           (0.00000,   0.10000,  -0.00000,   0.00000,   0.00000,   1.00000));

      Test_One
        ("3",
           (REVOLUTE, 0.0, Sin_Cos (0.1), 0.0),
         0.0,
         Expected_Pose =>
           ((0.00000,  -0.00000,   0.00000), To_Unit_Quaternion (0.04998,  0.00000,   0.00000,   0.99875)),
         Expected_Partial_Jacobian =>
           (0.00000,   0.00000,  -0.00000,   0.00000,   0.09983,   0.99500));

      Test_One
        ("4",
           (REVOLUTE, 0.0, Sin_Cos (0.0), 0.1),
         0.0,
         Expected_Pose =>
           ((0.00000,  -0.00000,   0.10000), To_Unit_Quaternion (0.00000,  0.00000,   0.00000,   1.00000)),
         Expected_Partial_Jacobian =>
           (0.00000,   0.00000,  -0.00000,   0.00000,   0.00000,   1.00000));

      Test_One
        ("5",
           (REVOLUTE, 0.0, Sin_Cos (0.0), 0.0),
         0.1,
         Expected_Pose =>
           ((0.00000,  -0.00000,   0.00000), To_Unit_Quaternion (0.00000,  0.00000,   0.04998,   0.99875)),
         Expected_Partial_Jacobian =>
           (0.00000,   0.00000,  -0.00000,   0.00000,   0.00000,   1.00000));

      Test_One
        ("6",
           (PRISMATIC, 0.0, Sin_Cos (0.0), Sin_Cos (0.0)),
         0.0,
         Expected_Pose =>
           ((0.00000,  0.00000,   0.00000), To_Unit_Quaternion (0.00000,  0.00000,   0.00000,   1.00000)),
         Expected_Partial_Jacobian =>
           (0.00000,   0.00000,  -0.00000,   0.00000,   0.00000,   1.00000));

      Test_One
        ("7",
           (PRISMATIC, 0.1, Sin_Cos (0.0), Sin_Cos (0.0)),
         0.0,
         Expected_Pose =>
           ((0.10000,  0.00000,   0.00000), To_Unit_Quaternion (0.00000,  0.00000,   0.00000,   1.00000)),
         Expected_Partial_Jacobian =>
           (0.00000,   0.10000,  -0.00000,   0.00000,   0.00000,   1.00000));

      Test_One
        ("8",
           (PRISMATIC, 0.0, Sin_Cos (0.1), Sin_Cos (0.0)),
         0.0,
         Expected_Pose =>
           ((0.00000,  -0.00000,   0.00000), To_Unit_Quaternion (0.04998,  0.00000,   0.00000,   0.99875)),
         Expected_Partial_Jacobian =>
           (0.00000,   0.00000,  -0.00000,   0.00000,   0.09983,   0.99500));

      Test_One
        ("9",
           (PRISMATIC, 0.0, Sin_Cos (0.0), Sin_Cos (0.1)),
         0.0,
         Expected_Pose =>
           ((0.00000,  -0.00000,   0.00000), To_Unit_Quaternion (0.00000,  0.00000,   0.04998,   0.99875)),
         Expected_Partial_Jacobian =>
           (0.00000,   0.00000,  -0.00000,   0.00000,   0.00000,   1.00000));

      Test_One
        ("10",
           (PRISMATIC, 0.0, Sin_Cos (0.0), Sin_Cos (0.0)),
         0.1,
         Expected_Pose =>
           ((0.00000,  -0.00000,   0.10000), To_Unit_Quaternion (0.00000,  0.00000,   0.00000,   1.00000)),
         Expected_Partial_Jacobian =>
           (0.00000,   0.00000,  -0.00000,   0.00000,   0.00000,   1.00000));

   end Test_Den_Hart;

   procedure Test_Mult (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_One
        (Message                : in String;
         Pose                   : in Pose_Type;
         Den_Hart               : in Den_Hart_Type;
         Position               : in Real_Type;
         Expected_Pose_Den_Hart : in Pose_Type;
         Expected_Den_Hart_Pose : in Pose_Type)
      is begin
         Check (Message & "Pose_Den_Hart", Mult (Pose, Den_Hart, Position), Expected_Pose_Den_Hart);
         Check (Message & "Den_Hart_Pose", Mult (Den_Hart, Position, Pose), Expected_Den_Hart_Pose);
      end Test_One;
   begin
      Test_One
        ("1",
         Pose     =>  Zero_Pose,
         Den_Hart => (REVOLUTE, 0.0, Sin_Cos (0.0), 0.0),
         Position =>  0.0,
         Expected_Pose_Den_Hart =>
           ((0.00000, 0.00000, 0.00000), To_Unit_Quaternion (0.00000, 0.00000, 0.00000, 1.00000)),
         Expected_Den_Hart_Pose =>
           ((0.00000, 0.00000, 0.00000), To_Unit_Quaternion (0.00000, 0.00000, 0.00000, 1.00000)));

      Test_One
        ("2",
         Pose =>  ((1.0, 2.0, 3.0),
                   (To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit)))),
         Den_Hart => (REVOLUTE, 0.1, Sin_Cos (0.0), 0.1),
         Position =>  0.1,
         Expected_Pose_Den_Hart =>
           ((1.10000,   1.99002,   3.09950), To_Unit_Quaternion (0.04992,  -0.00250,   0.04992,   0.99750)),
         Expected_Den_Hart_Pose =>
           ((0.89534,   2.08984,   3.10000), To_Unit_Quaternion (0.04992,   0.00250,   0.04992,   0.99750)));

      Test_One
        ("3",
         Pose =>  ((1.0, 2.0, 3.0),
                   (To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit)))),
         Den_Hart => (REVOLUTE, 0.5, Sin_Cos (0.0), 0.1),
         Position =>  0.1,
         Expected_Pose_Den_Hart =>
           ((1.50000,   1.99002,   3.09950), To_Unit_Quaternion (0.04992,  -0.00250,   0.04992,   0.99750)),
         Expected_Den_Hart_Pose =>
           ((1.29534,   2.08984,   3.10000), To_Unit_Quaternion (0.04992,   0.00250,   0.04992,   0.99750)));

      Test_One
        ("4",
         Pose =>  ((1.0, 2.0, 3.0),
                   (To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit)))),
         Den_Hart => (Revolute, 0.5, Sin_Cos (0.5), 0.1),
         Position =>  0.1,
         Expected_Pose_Den_Hart =>
           ((1.50000,   1.94354,   3.08253), To_Unit_Quaternion (0.29515,  -0.01477,   0.04775,   0.95414)),
         Expected_Den_Hart_Pose =>
           ((1.29534,   0.34779,   3.72243), To_Unit_Quaternion (0.29515,  -0.00993,   0.04898,   0.95414)));

      Test_One
        ("4",
         Pose =>  ((1.0, 2.0, 3.0),
                   (To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit)))),
         Den_Hart => (REVOLUTE, 0.5, Sin_Cos (0.5), 0.5),
         Position =>  0.1,
         Expected_Pose_Den_Hart =>
           ((1.50000,   1.71768,   3.41267), To_Unit_Quaternion (0.29515,  -0.01477,   0.04775,   0.95414)),
         Expected_Den_Hart_Pose =>
           ((1.29534,   0.15602,   4.07346), To_Unit_Quaternion (0.29515,  -0.00993,   0.04898,   0.95414)));

      Test_One
        ("4",
         Pose =>  ((1.0, 2.0, 3.0),
                   (To_Unit_Quaternion (0.5, 0.5, 0.5, 0.5))),
         Den_Hart => (REVOLUTE, 0.5, Sin_Cos (0.5), 0.5),
         Position =>  0.5,
         Expected_Pose_Den_Hart =>
           ((1.43879,   2.50000,   2.76029), To_Unit_Quaternion (0.73971,   0.43879,   0.43879,   0.26029)),
         Expected_Den_Hart_Pose =>
           ((0.41873,   0.28305,   4.14286), To_Unit_Quaternion (0.43879,   0.43879,   0.73971,   0.26029)));
   end Test_Mult;

   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_Den_Hart_Left");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Den_Hart'Access, "Test_Den_Hart");
      Register_Routine (T, Test_Mult'Access, "Test_Mult");
   end Register_Tests;

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

end Test_Math_Float_Den_Hart_Left;
