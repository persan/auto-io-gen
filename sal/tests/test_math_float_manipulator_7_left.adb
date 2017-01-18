--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2005, 2007 - 2008 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;

with SAL.Math_Float.AUnit;
with SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_6.Left;
with SAL.Math_Float.Scalar; use SAL.Math_Float.Scalar;
with SAL.Math_Float_Manipulator_7.Left;
with SAL.Math_Float_RRC_K1607_Nominal;
package body Test_Math_Float_Manipulator_7_Left is

   use SAL.Math_Float;
   use SAL.Math_Float.DOF_3;
   use SAL.Math_Float.DOF_6;
   use SAL.Math_Float.DOF_6.Left;
   use SAL.Math_Float_Manipulator_7;
   use SAL.Math_Float_Manipulator_7.Math;
   use SAL.Math_Float_Manipulator_7.Math.Joint_Array_Real_Ops;
   use SAL.Math_Float_Manipulator_7.Left;

   Threshold : constant := 10.0e-5;

   Geometry         : constant Joint_Array_Den_Hart_Type := SAL.Math_Float_RRC_K1607_Nominal.Geometry;
   Mass             : constant Joint_Array_Mass_Type     := SAL.Math_Float_RRC_K1607_Nominal.Mass;
   Nominal_Position : constant Joint_Array_Real_Type     := (0.0, -Pi / 2.0, 0.0, Pi / 2.0, 0.0, Pi / 2.0, 0.0);
   Tlast_T_Tp       : constant Pose_Type                 := ((0.0, 0.0, 0.16764), Zero_Unit_Quaternion);
   Tp_T_Obj         : constant Pose_Type                 := ((0.0, 0.0, 0.1), Zero_Unit_Quaternion);
   T0_A_Grav        : constant Cart_Vector_Type          := (0.0, 0.0, -9.80665);

   procedure Check
     (Message            : in String;
      Computed, Expected : in Dual_Cart_Vector_Type;
      Threshold          : in Real_Type)
   is begin
      for I in Computed'Range loop
         AUnit.Assertions.Assert (abs (Computed (I) - Expected (I)) < Threshold, Message & " failed");
      end loop;
   end Check;

   procedure Check (Message : in String; Computed, Expected : in Joint_Array_Real_Type)
   is begin
      for I in Computed'Range loop
         AUnit.Assertions.Assert (abs (Computed (I) - Expected (I)) < Threshold, Message & " failed");
      end loop;
   end Check;

   procedure Check (Message : in String; Computed, Expected : in Projector_Type)
   is begin
      for I in Computed'Range loop
         for J in Computed (I)'Range loop
            AUnit.Assertions.Assert (abs (Computed (I)(J) - Expected (I)(J)) < Threshold, Message & " failed");
         end loop;
      end loop;
   end Check;

   ----------
   --  Test routines

   procedure Projector (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_Projector
        (Message                : in String;
         Joint_Pos              : in Joint_Array_Real_Type;
         Joint_Delta            : in Joint_Array_Real_Type;
         Expected_Projector     : in Projector_Type;
         Expected_P_Joint_Delta : in Joint_Array_Real_Type)
      is
         Ti_T_Obj         : Joint_Array_Pose_Type;
         T0_T_Obj         : Pose_Type;
         Jacobian         : Jacobian_Type;
         Projector        : Projector_Type;
         Inverse_Jacobian : Inverse_Jacobian_Type;
         P_Joint_Delta    : Joint_Array_Real_Type;
      begin
         Slow_Ti_T_Obj (Joint_Pos, Geometry, Tlast_T_Tp, Tp_T_Obj, Ti_T_Obj, T0_T_Obj);
         Jacobian         := Slow_Jacobian (Ti_T_Obj);
         Inverse_Jacobian := Inverse (Jacobian);
         Projector        := Null_Space_Projector (Jacobian, Inverse_Jacobian);
         Check (Message & " projector", Projector, Expected_Projector);
         P_Joint_Delta    := Projector * Joint_Delta;
         Check (Message & " P_Joint_Delta", P_Joint_Delta, Expected_P_Joint_Delta);
         Check
           (Message & " Cart_Delta",
            T0_T_Obj - Slow_T0_T_Obj (Joint_Pos + P_Joint_Delta, Geometry, Tlast_T_Tp, Tp_T_Obj),
            (others => 0.0),
            Threshold => 3.0e-3);
      end Test_Projector;

   begin
      Test_Projector
        (Message            => "Nominal",
         Joint_Pos          => Nominal_Position,
         Joint_Delta        => (others => 0.1),
         Expected_Projector =>
           ((0.27376,  -0.00000,   0.24887,   0.00000,  -0.27376,   0.00000,   0.24887),
            (-0.00000,  -0.00000,  -0.00000,  -0.00000,   0.00000,  -0.00000,  -0.00000),
            (0.24887,  -0.00000,   0.22624,   0.00000,  -0.24887,   0.00000,   0.22624),
            (0.00000,   0.00000,   0.00000,  -0.00000,  -0.00000,  -0.00000,   0.00000),
            (-0.27376,   0.00000,  -0.24887,  -0.00000,   0.27376,  -0.00000,  -0.24887),
            (0.00000,   0.00000,   0.00000,   0.00000,  -0.00000,   0.00000,   0.00000),
            (0.24887,  -0.00000,   0.22624,   0.00000,  -0.24887,   0.00000,   0.22624)),
         Expected_P_Joint_Delta => (0.04977,  -0.00000,   0.04525,  -0.00000,  -0.04977,   0.00000,   0.04525));

      Test_Projector
        (Message            => "Wrist Singular",
         Joint_Pos          => (0.0, -Pi / 2.0, 0.0, Pi / 2.0, 0.0, 0.0, 0.0),
         Joint_Delta        => (5 => 0.1, others => 0.0),
         Expected_Projector =>
           ((0.00000,  -0.00000,  -0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
            (0.00000,  -0.00000,  -0.00000,   0.00000,  -0.00000,   0.00000,  -0.00000),
            (0.00000,  -0.00000,  -0.00000,  -0.00000,  -0.00000,   0.00000,  -0.00000),
            (0.00000,  -0.00000,  -0.00000,   0.00000,   0.00000,  -0.00000,   0.00000),
            (0.00000,   0.00000,   0.00000,  -0.00000,   0.50000,   0.00000,  -0.50000),
            (0.00000,   0.00000,   0.00000,  -0.00000,   0.00000,   0.00000,   0.00000),
            (0.00000,   0.00000,   0.00000,  -0.00000,  -0.50000,   0.00000,   0.50000)),
         Expected_P_Joint_Delta => (0.00000,  -0.00000,  -0.00000,   0.00000,   0.05000,   0.00000,  -0.05000));

      Test_Projector
        (Message            => "Elbow Singular",
         Joint_Pos          => (0.0, -Pi / 2.0, 0.0, 0.0, 0.0, Pi / 2.0, 0.0),
         Joint_Delta        => (5 => 0.1, others => 0.0),
         Expected_Projector =>
           ((0.00000,   0.00000,  -0.00000,   0.00000,  -0.00000,   0.00000,  -0.00000),
            (0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000,  -0.00000),
            (0.00000,  -0.00000,   0.50000,  -0.00000,  -0.50000,  -0.00000,   0.00000),
            (0.00000,  -0.00000,  -0.00000,   0.00000,  -0.00000,  -0.00000,  -0.00000),
            (0.00000,  -0.00000,  -0.50000,  -0.00000,   0.50000,  -0.00000,   0.00000),
            (0.00000,   0.00000,  -0.00000,  -0.00000,  -0.00000,   0.00000,   0.00000),
            (0.00000,   0.00000,   0.00000,  -0.00000,   0.00000,   0.00000,   0.00000)),
         Expected_P_Joint_Delta => (0.00000,   0.00000,  -0.05000,  -0.00000,   0.05000,  -0.00000,   0.00000));

      Test_Projector
        (Message            => "Shoulder +Singular",
         Joint_Pos          => (0.0, -Pi / 2.0, Pi / 2.0, Pi / 2.0, 0.0, Pi / 2.0, 0.0),
         Joint_Delta        => (2 => 0.1, others => 0.0),
         Expected_Projector =>
           ((0.00002,  -0.00233,  -0.00212,   0.00051,   0.00233,  -0.00049,  -0.00212),
            (-0.00233,   0.26705,   0.24277,  -0.05834,  -0.26705,   0.05600,   0.24277),
            (-0.00212,   0.24277,   0.22070,  -0.05303,  -0.24277,   0.05091,   0.22070),
            (0.00051,  -0.05834,  -0.05303,   0.01274,   0.05834,  -0.01223,  -0.05303),
            (0.00233,  -0.26705,  -0.24277,   0.05834,   0.26705,  -0.05600,  -0.24277),
            (-0.00049,   0.05600,   0.05091,  -0.01223,  -0.05600,   0.01174,   0.05091),
            (-0.00212,   0.24277,   0.22070,  -0.05303,  -0.24277,   0.05091,   0.22070)),
         Expected_P_Joint_Delta => (-0.00023,   0.02670,   0.02428,  -0.00583,  -0.02670,   0.00560,   0.02428));

      Test_Projector
        (Message            => "Shoulder -Singular",
         Joint_Pos          => (0.0, -Pi / 2.0, -Pi / 2.0, Pi / 2.0, 0.0, Pi / 2.0, 0.0),
         Joint_Delta        => (2 => 0.1, others => 0.0),
         Expected_Projector =>
           ((0.00002,   0.00233,  -0.00212,  -0.00051,   0.00233,   0.00049,  -0.00212),
            (0.00233,   0.26705,  -0.24277,  -0.05834,   0.26705,   0.05600,  -0.24277),
            (-0.00212,  -0.24277,   0.22070,   0.05303,  -0.24277,  -0.05091,   0.22070),
            (-0.00051,  -0.05834,   0.05303,   0.01274,  -0.05834,  -0.01223,   0.05303),
            (0.00233,   0.26705,  -0.24277,  -0.05834,   0.26705,   0.05600,  -0.24277),
            (0.00049,   0.05600,  -0.05091,  -0.01223,   0.05600,   0.01174,  -0.05091),
            (-0.00212,  -0.24277,   0.22070,   0.05303,  -0.24277,  -0.05091,   0.22070)),
         Expected_P_Joint_Delta => (0.00023,   0.02670,  -0.02428,  -0.00583,   0.02670,   0.00560,  -0.02428));
   end Projector;

   procedure Gravity (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      function Compute_Gravity (Joints : in Joint_Array_Real_Type) return Joint_Array_Real_Type
      is begin
         return Slow_Gravity_Torque (Slow_T0_T_Ti (Joints, Geometry), T0_A_Grav, Mass);
      end Compute_Gravity;

   begin
      Check
        (Message => "zero",
         Computed => Compute_Gravity ((others => 0.0)),
         Expected => (0.0, 111.76547,   0.00000, -25.10846,   0.00000,  -2.82928,   0.00000));

      Check
        (Message => "zero + J1",
         Computed => Compute_Gravity ((2.00000,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000)),
         Expected => (0.0, 111.76558,  -0.00000, -25.10847,  -0.00000,  -2.82928,   0.00000));

      Check
        (Message => "shoulder",
         Computed => Compute_Gravity ((0.00000,   1.57080,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000)),
         Expected => (0.0, 488.12375,  -3.85611, 105.15102,  -0.66430,   0.56936,   0.00000));

      Check
        (Message => "shoulder + elbow",
         Computed => Compute_Gravity ((0.00000,   1.57080,   0.00000,  -1.57080,   0.00000,   0.00000,   0.00000)),
         Expected => (0.0, 357.86423,  -3.85612, -25.1085,   0.00000,  -2.82928,   0.00000));

      Check
        (Message => "shoulder + elbow + wrist",
         Computed => Compute_Gravity ((0.00000,  1.57080,   0.00000,  -1.57080,   0.00000,   1.57080,   0.00000)),
         Expected => (0.0, 361.26285, -3.85611, -21.7098,   0.00000,   0.569361,  0.00000));

   end Gravity;

   ----------
   --  Public routines

   function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_math_float_manipulator_7_left");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Projector'Access, "Projector");
      Register_Routine (T, Gravity'Access, "Gravity");
   end Register_Tests;

   overriding procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 10.0e-5;
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Float_Manipulator_7_Left;
