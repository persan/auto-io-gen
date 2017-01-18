--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003 - 2008 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with SAL.Math_Float.AUnit;
with SAL.Math_Float.DOF_3.Left;
with SAL.Math_Float.DOF_6.Left;
with SAL.Math_Float.DOF_6;
with SAL.Math_Float.Scalar; use SAL.Math_Float.Scalar;
with SAL.Math_Float_Kraft_HC_Nominal;
with SAL.Math_Float_Manipulator_6.AUnit; use SAL.Math_Float_Manipulator_6.AUnit;
with SAL.Math_Float_Manipulator_6.Left;
package body Test_Math_Float_Manipulator_6_Left is

   use SAL.Math_Float;
   use SAL.Math_Float.DOF_3;
   use SAL.Math_Float.DOF_3.Left;
   use SAL.Math_Float_Manipulator_6;
   use SAL.Math_Float_Manipulator_6.Left;
   use SAL.Math_Float_Manipulator_6.Math;
   use SAL.Math_Float_Manipulator_6.Math.Joint_Array_Real_Ops;

   Geometry         : constant Joint_Array_Den_Hart_Type      := SAL.Math_Float_Kraft_HC_Nominal.Geometry;
   Nominal_Position : constant Joint_Array_Real_Type          := (0.0, -Pi/2.0, Pi / 2.0, 0.0, -Pi/2.0, 0.0);
   Tlast_T_Tp       : constant SAL.Math_Float.DOF_6.Pose_Type := ((0.0, 0.0,  0.1), Zero_Unit_Quaternion);
   Tp_T_Obj         : constant SAL.Math_Float.DOF_6.Pose_Type := ((0.0, 0.0, -0.1), Zero_Unit_Quaternion);

   procedure Jacobian (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      --  Inverse is just a call to an instantiation of
      --  Generic_Inverse_Array_Math, so we don't need to test it
      --  here.

      procedure Test_Jacobian
        (Message    : in String;
         Joints     : in Joint_Array_Real_Type;
         Tlast_T_TP : in SAL.Math_Float.DOF_6.Pose_Type := SAL.Math_Float.DOF_6.Zero_Pose;
         TP_T_Obj   : in SAL.Math_Float.DOF_6.Pose_Type := SAL.Math_Float.DOF_6.Zero_Pose;
         Expected   : in Jacobian_Type)
      is
         Ti_T_Obj : Joint_Array_Pose_Type;
         T0_T_Obj : SAL.Math_Float.DOF_6.Pose_Type;
      begin
         Slow_Ti_T_Obj (Joints, Geometry, Tlast_T_TP, TP_T_Obj, Ti_T_Obj, T0_T_Obj);
         Check (Message, Slow_Jacobian (Ti_T_Obj), Expected);
      end Test_Jacobian;

   begin
      Test_Jacobian
        (Message  => "Nominal",
         Joints   => Nominal_Position,
         Expected =>
           ((-0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
            (-0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
            (-0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
            (1.00000,   -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
            (-0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
            (0.00000,    0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message    => "Nominal + Tlast_T_Tp",
         Joints     => Nominal_Position,
         Tlast_T_TP => Tlast_T_Tp,
         Expected   =>
           ((-0.00000,  -0.30500,  -0.30500,   0.00000,  -0.10000,   0.00000),
            (-0.30500,   0.00000,   0.00000,   0.10000,   0.00000,   0.00000),
            (-0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
            (1.00000,   -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
            (-0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
            (0.00000,    0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message    => "Nominal + Tlast_T_Tp + Tp_T_Obj",
         Joints     => Nominal_Position,
         Tlast_T_TP => Tlast_T_Tp,
         TP_T_Obj   => Tp_T_Obj,
         Expected   =>
           ((-0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
            (-0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
            (-0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
            (1.00000,   -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
            (-0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
            (0.00000,    0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J6",
         Joints   => Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.0, 0.1),
         Expected =>
           ((-0.02047,  -0.20398,  -0.20398,   0.00000,   0.00000,   0.00000),
            (-0.20398,   0.02047,   0.02047,   0.00000,   0.00000,   0.00000),
            (-0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
            (0.99500,   -0.09983,  -0.09983,  -0.99500,  -0.09983,   0.00000),
            (-0.09983,  -0.99500,  -0.99500,   0.09983,  -0.99500,   0.00000),
            (0.00000,    0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J5",
         Joints   => Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.1, 0.0),
         Expected =>
           ((-0.01348,  -0.19449,  -0.21246,   0.00050,   0.00000,   0.00000),
            (-0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
            (-0.13433,   0.11499,  -0.06411,   0.00498,   0.00000,   0.00000),
            (0.99500,   -0.00000,   0.00000,  -0.99500,  -0.00000,   0.00000),
            (-0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
            (-0.09983,   0.00000,   0.00000,   0.09983,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J4",
         Joints   => Nominal_Position + (0.0, 0.0, 0.0, 0.1, 0.0, 0.0),
         Expected =>
            ((-0.00000,  -0.20550,  -0.20550,   0.00000,   0.00000,   0.00000),
             (-0.19100,  -0.00948,   0.00849,   0.00000,   0.00000,   0.00000),
             (-0.15482,   0.09453,  -0.08458,   0.00500,   0.00000,   0.00000),
             (1.00000,   -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
             (0.00000,   -0.99500,  -0.99500,   0.00000,  -1.00000,   0.00000),
             (0.00000,   -0.09983,  -0.09983,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J3",
         Joints   => Nominal_Position + (0.0, 0.0, 0.1, 0.0, 0.0, 0.0),
         Expected =>
            ((-0.01348,  -0.18703,  -0.20500,   0.00000,   0.00000,   0.00000),
             (-0.19549,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
             (-0.13433,   0.09410,  -0.08500,   0.00500,   0.00000,   0.00000),
             (0.99500,   -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
             (-0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
             (-0.09983,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J2",
         Joints   => Nominal_Position + (0.0, 0.1, 0.0, 0.0, 0.0, 0.0),
         Expected =>
            ((-0.01348,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
             (-0.21346,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
             (-0.13433,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
             (0.99500,   -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
             (-0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
             (-0.09983,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J1",
         Joints   => Nominal_Position + (0.1, 0.0, 0.0, 0.0, 0.0, 0.0),
         Expected =>
            ((-0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
             (-0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
             (-0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
             (1.00000,   -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
             (-0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
             (0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

   end Jacobian;

   procedure Jacobian_Change_Frame (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Ti_T_Obj : Joint_Array_Pose_Type;
      T0_T_Obj : SAL.Math_Float.DOF_6.Pose_Type;
      Jacobian : Jacobian_Type;

      procedure Test_Change_Frame
        (Message       : in String;
         Jacobian      : in Jacobian_Type;
         Current_T_New : in SAL.Math_Float.DOF_6.Pose_Type;
         Expected      : in Jacobian_Type)
      is
         New_Jacob_1 : constant Jacobian_Type := Transform_Jacobian (Current_T_New, Jacobian);
         New_Jacob_2 : constant Jacobian_Type := SAL.Math_Float.DOF_6.Left.To_Rate_Transform (Current_T_New) * Jacobian;
      begin
         Check (Message & "Transform_Jacobian", New_Jacob_1, Expected);
         Check (Message & "To_Rate_Transform", New_Jacob_2, Expected);
      end Test_Change_Frame;

   begin
      Slow_Ti_T_Obj (Nominal_Position, Geometry, Tlast_T_Tp, Tp_T_Obj, Ti_T_Obj, T0_T_Obj);
      Jacobian := Slow_Jacobian (Ti_T_Obj);

      Test_Change_Frame
        ("Zero",   Jacobian, (Zero_Cart_Vector, Zero_Unit_Quaternion),
         ((-0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
          (-0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
          (-0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
          (1.00000,  -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
          (-0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
          (0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Change_Frame
        ("0.1 TX", Jacobian, ((0.1, 0.0, 0.0), Zero_Unit_Quaternion),
        ((-0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
         (-0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.10000),
         (-0.13500,   0.19500,   0.01500,   0.00500,   0.10000,   0.00000),
         (1.00000,   0.00000,   0.00000,  -1.00000,   0.00000,   0.00000),
         (0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
         (0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Change_Frame
        ("0.1 TY", Jacobian, ((0.0, 0.1, 0.0), Zero_Unit_Quaternion),
        ((-0.00000,  -0.20500,  -0.20500,  -0.00000,  -0.00000,  -0.10000),
         (-0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
         (-0.03500,   0.09500,  -0.08500,  -0.09500,   0.00000,   0.00000),
         (1.00000,   0.00000,   0.00000,  -1.00000,   0.00000,   0.00000),
         (0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
         (0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Change_Frame
        ("0.1 TZ", Jacobian, ((0.0, 0.0, 0.1), Zero_Unit_Quaternion),
        ((-0.00000,  -0.30500,  -0.30500,   0.00000,  -0.10000,   0.00000),
         (-0.30500,   0.00000,   0.00000,   0.10000,   0.00000,   0.00000),
         (-0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
         (1.00000,   0.00000,   0.00000,  -1.00000,   0.00000,   0.00000),
         (0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
         (0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Change_Frame
        ("0.1 RX", Jacobian, (Zero_Cart_Vector, To_Unit_Quaternion (0.1, X)),
        ((-0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
         (-0.21745,   0.00948,  -0.00849,   0.00050,   0.00000,   0.00000),
         (-0.11386,   0.09453,  -0.08458,   0.00498,   0.00000,   0.00000),
         (1.00000,   0.00000,   0.00000,  -1.00000,   0.00000,   0.00000),
         (0.00000,  -0.99500,  -0.99500,   0.00000,  -0.99500,   0.09983),
         (0.00000,   0.09983,   0.09983,   0.00000,   0.09983,   0.99500)));

      Test_Change_Frame
        ("0.1 RY", Jacobian, (Zero_Cart_Vector, To_Unit_Quaternion (0.1, Y)),
        ((0.01348,  -0.21346,  -0.19549,  -0.00050,   0.00000,   0.00000),
         (-0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
         (-0.13433,   0.07406,  -0.10504,   0.00498,   0.00000,   0.00000),
         (0.99500,  -0.00000,  -0.00000,  -0.99500,  -0.00000,  -0.09983),
         (0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
         (0.09983,   0.00000,   0.00000,  -0.09983,   0.00000,   0.99500)));

      Test_Change_Frame
        ("0.1 RZ", Jacobian, (Zero_Cart_Vector, To_Unit_Quaternion (0.1, Z)),
        ((-0.02047,  -0.20398,  -0.20398,   0.00000,   0.00000,   0.00000),
         (-0.20398,   0.02047,   0.02047,  -0.00000,   0.00000,   0.00000),
         (-0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
         (0.99500,  -0.09983,  -0.09983,  -0.99500,  -0.09983,   0.00000),
         (-0.09983,  -0.99500,  -0.99500,   0.09983,  -0.99500,   0.00000),
         (0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

   end Jacobian_Change_Frame;

   procedure T0_T_Ti_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("",
         Slow_T0_T_Ti (Nominal_Position, Geometry),
         (1 =>
            (Translation => (0.00000,   0.00000,  -0.00000),
             Rotation => To_Unit_Quaternion (-1.0, 0.0, 0.0, 0.0)),
          2 =>
            (Translation => (0.00000,  -0.07500,   0.00000),
             Rotation => To_Unit_Quaternion (-0.5, -0.5, 0.5, -0.5)),
          3 =>
            (Translation => (0.00000,  -0.13000,  -0.18000),
             Rotation => To_Unit_Quaternion (-0.70711, 0.00000, 0.00000, -0.70711)),
          4 =>
            (Translation => (0.20500,  -0.13000,  -0.09500),
             Rotation => To_Unit_Quaternion (0.0, 0.0, 0.0, -1.0)),
          5 =>
            (Translation => (0.20500,  -0.13500,  -0.09500),
             Rotation => To_Unit_Quaternion (-0.50000, -0.50000,  0.50000, -0.50000)),
          6 =>
            (Translation => (0.20500,  -0.13500,  -0.09500),
             Rotation => To_Unit_Quaternion (0.00000, -0.70711,  0.00000, -0.70711))));
   end T0_T_Ti_1;

   procedure T0_T_Ti_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("",
         Slow_T0_T_Ti (Nominal_Position + (0.1, 0.0, 0.0, 0.0, 0.0, 0.0), Geometry),
         (1 =>
            (Translation => (0.00000,   0.00000,  -0.00000),
             Rotation => To_Unit_Quaternion (-0.99875, 0.04998,   0.00000,   0.00000)),
          2 =>
            (Translation => (-0.00749,  -0.07463,   0.00000),
             Rotation => To_Unit_Quaternion (-0.52436, -0.47438, 0.52436, -0.47438)),
          3 =>
            (Translation => (-0.01298,  -0.12935,  -0.18000),
             Rotation => To_Unit_Quaternion (-0.70622,  0.03534,  0.03534, -0.70622)),
          4 =>
            (Translation => (0.19100,  -0.14982,  -0.09500),
             Rotation => To_Unit_Quaternion (-0.00000, -0.00000,  0.04998, -0.99875)),
          5 =>
            (Translation => (0.19050,  -0.15479,  -0.09500),
             Rotation => To_Unit_Quaternion (-0.52436, -0.47438,  0.52436, -0.47438)),
          6 =>
            (Translation => (0.19050,  -0.15479,  -0.09500),
             Rotation => To_Unit_Quaternion (-0.03534, -0.70622,  0.03534, -0.70622))));

   end T0_T_Ti_2;

   procedure T0_T_Ti_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("",
         Slow_T0_T_Ti (Nominal_Position + (0.0, 0.1, 0.0, 0.0, 0.0, 0.0), Geometry),
         (1 =>
            (Translation => (0.00000,   0.00000,  -0.00000),
             Rotation => To_Unit_Quaternion (-1.0, 0.0, 0.0, 0.0)),
          2 =>
            (Translation => (0.00000,  -0.07500,   0.00000),
             Rotation => To_Unit_Quaternion (-0.52436, -0.47438, 0.47438, -0.52436)),
          3 =>
            (Translation => (0.01797,  -0.13000,  -0.17910),
             Rotation => To_Unit_Quaternion (-0.70622, 0.03534, -0.03534, -0.70622)),
          4 =>
            (Translation => (0.21346,  -0.13000,  -0.07406),
             Rotation => To_Unit_Quaternion (0.00000, 0.04998, 0.00000, -0.99875)),
          5 =>
            (Translation => (0.21346,  -0.13500,  -0.07406),
             Rotation => To_Unit_Quaternion (-0.52436, -0.47438,  0.47438, -0.52436)),
          6 =>
            (Translation => (0.21346,  -0.13500,  -0.07406),
             Rotation => To_Unit_Quaternion (0.00000, -0.67088,  0.00000, -0.74156))));

   end T0_T_Ti_3;

   procedure T0_T_Ti_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("",
         Slow_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.1, 0.0, 0.0, 0.0), Geometry),
         (1 =>
            (Translation => (0.00000,   0.00000,  -0.00000),
             Rotation => To_Unit_Quaternion (-1.0, 0.0, 0.0, 0.0)),
          2 =>
            (Translation => (0.00000,  -0.07500,   0.00000),
             Rotation => To_Unit_Quaternion (-0.5, -0.5, 0.5, -0.5)),
          3 =>
            (Translation => (0.00000,  -0.13000,  -0.18000),
             Rotation => To_Unit_Quaternion (-0.70622, 0.03534, -0.03534, -0.70622)),
          4 =>
            (Translation => (0.19549,  -0.13000,  -0.07496),
             Rotation => To_Unit_Quaternion (0.00000, 0.04998, 0.00000, -0.99875)),
          5 =>
            (Translation => (0.19549,  -0.13500,  -0.07496),
             Rotation => To_Unit_Quaternion (-0.52436, -0.47438,  0.47438, -0.52436)),
          6 =>
            (Translation => (0.19549,  -0.13500,  -0.07496),
             Rotation => To_Unit_Quaternion (0.00000, -0.67088,  0.00000, -0.74156))));
   end T0_T_Ti_4;

   procedure T0_T_Ti_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("",
         Slow_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.0, 0.1, 0.0, 0.0), Geometry),
         (1 =>
            (Translation => (0.00000,   0.00000,  -0.00000),
             Rotation => To_Unit_Quaternion (-1.0, 0.0, 0.0, 0.0)),
          2 =>
            (Translation => (0.00000,  -0.07500,   0.00000),
             Rotation => To_Unit_Quaternion (-0.5, -0.5, 0.5, -0.5)),
          3 =>
            (Translation => (0.00000,  -0.13000,  -0.18000),
             Rotation => To_Unit_Quaternion (-0.70711, 0.00000, 0.00000, -0.70711)),
          4 =>
            (Translation => (0.20500,  -0.13000,  -0.09500),
             Rotation => To_Unit_Quaternion (0.00000, 0.00000, -0.04998, -0.99875)),
          5 =>
            (Translation => (0.20550,  -0.13498,  -0.09500),
             Rotation => To_Unit_Quaternion (-0.47438, -0.52436,  0.47438, -0.52436)),
          6 =>
            (Translation => (0.20550,  -0.13498,  -0.09500),
             Rotation => To_Unit_Quaternion (0.03534, -0.70622, -0.03534, -0.70622))));
   end T0_T_Ti_5;

   procedure T0_T_Ti_6 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("",
         Slow_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.1, 0.0), Geometry),
         (1 =>
            (Translation => (0.00000,   0.00000,  -0.00000),
             Rotation => To_Unit_Quaternion (-1.0, 0.0, 0.0, 0.0)),
          2 =>
            (Translation => (0.00000,  -0.07500,   0.00000),
             Rotation => To_Unit_Quaternion (-0.5, -0.5, 0.5, -0.5)),
          3 =>
            (Translation => (0.00000,  -0.13000,  -0.18000),
             Rotation => To_Unit_Quaternion (-0.70711, 0.00000, 0.00000, -0.70711)),
          4 =>
            (Translation => (0.20500,  -0.13000,  -0.09500),
             Rotation => To_Unit_Quaternion (0.0, 0.0, 0.0, -1.0)),
          5 =>
            (Translation => (0.20500,  -0.13500,  -0.09500),
             Rotation => To_Unit_Quaternion (-0.52436, -0.47438,  0.47438, -0.52436)),
          6 =>
            (Translation => (0.20500,  -0.13500,  -0.09500),
             Rotation => To_Unit_Quaternion (0.00000, -0.67088,  0.00000, -0.74156))));
   end T0_T_Ti_6;

   procedure T0_T_Ti_7 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("",
         Slow_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.0, 0.1), Geometry),
         (1 =>
            (Translation => (0.00000,   0.00000,  -0.00000),
             Rotation => To_Unit_Quaternion (-1.0, 0.0, 0.0, 0.0)),
          2 =>
            (Translation => (0.00000,  -0.07500,   0.00000),
             Rotation => To_Unit_Quaternion (-0.5, -0.5, 0.5, -0.5)),
          3 =>
            (Translation => (0.00000,  -0.13000,  -0.18000),
             Rotation => To_Unit_Quaternion (-0.70711, 0.00000, 0.00000, -0.70711)),
          4 =>
            (Translation => (0.20500,  -0.13000,  -0.09500),
             Rotation => To_Unit_Quaternion (0.0, 0.0, 0.0, -1.0)),
          5 =>
            (Translation => (0.20500,  -0.13500,  -0.09500),
             Rotation => To_Unit_Quaternion (-0.50000, -0.50000, 0.50000, -0.50000)),
          6 =>
            (Translation => (0.20500,  -0.13500,  -0.09500),
             Rotation => To_Unit_Quaternion (-0.03534, -0.70622, -0.03534, -0.70622))));
   end T0_T_Ti_7;

   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_Manipulator_6_Left");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Jacobian'Access, "Jacobian");
      Register_Routine (T, Jacobian_Change_Frame'Access, "Jacobian_Change_Frame");
      Register_Routine (T, T0_T_Ti_1'Access, "T0_T_Ti_1");
      Register_Routine (T, T0_T_Ti_2'Access, "T0_T_Ti_2");
      Register_Routine (T, T0_T_Ti_3'Access, "T0_T_Ti_3");
      Register_Routine (T, T0_T_Ti_4'Access, "T0_T_Ti_4");
      Register_Routine (T, T0_T_Ti_5'Access, "T0_T_Ti_5");
      Register_Routine (T, T0_T_Ti_6'Access, "T0_T_Ti_6");
      Register_Routine (T, T0_T_Ti_7'Access, "T0_T_Ti_7");
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

end Test_Math_Float_Manipulator_6_Left;
