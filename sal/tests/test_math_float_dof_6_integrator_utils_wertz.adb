--  Abstract :
--
--  See spec
--
--  Copyright (C) 2005, 2006 Stephen Leake.  All Rights Reserved.
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
with SAL.Gen_Math.Gen_Runge_Kutta_4th_Wertz;
with SAL.Math_Float.AUnit;                        use SAL.Math_Float.AUnit;
with SAL.Math_Float.DOF_3.AUnit;                  use SAL.Math_Float.DOF_3.AUnit;
with SAL.Math_Float.DOF_3.Wertz;
with SAL.Math_Float.DOF_6.Integrator_Utils.Wertz; use SAL.Math_Float.DOF_6.Integrator_Utils.Wertz;
with SAL.Math_Float.DOF_6.Integrator_Utils;       use SAL.Math_Float.DOF_6.Integrator_Utils;
with SAL.Math_Float.DOF_6.Wertz;                  use SAL.Math_Float.DOF_6.Wertz;
package body Test_Math_Float_DOF_6_Integrator_Utils_Wertz
is
   use SAL.Math_Float;
   use SAL.Math_Float.DOF_3;
   use SAL.Math_Float.DOF_3.Cart_Vector_Ops;
   use SAL.Math_Float.DOF_3.Wertz;
   use SAL.Math_Float.DOF_6;

   --  common function between tests
   function Zero_Indep_Wrench return Dual_Cart_Vector_Type
   is begin
      return (others => 0.0);
   end Zero_Indep_Wrench;

   function Zero_Dep_Wrench (State : in State_Type) return Dual_Cart_Vector_Type
   is
      pragma Unreferenced (State);
   begin
      return (others => 0.0);
   end Zero_Dep_Wrench;

   function Constant_Wrench (State : in State_Type) return Dual_Cart_Vector_Type
   is
      pragma Unreferenced (State);
   begin
      return (Tx => 0.1, Rz => -0.1, others => 0.0);
   end Constant_Wrench;

   function Zero_Momentum return Cart_Vector_Type
   is
   begin
      return (others => 0.0);
   end Zero_Momentum;

   generic
      with function State_Dep_Wrench (State : in State_Type) return Dual_Cart_Vector_Type;
      with function State_Indep_Wrench return Dual_Cart_Vector_Type;
      with function State_Indep_Rot_Mom return Cart_Vector_Type;
      --  Return wrench on object due to actuators, gravity, etc.
      --  Euler torque will be added internally.
   procedure Gen_State_Integrator
     (State        : in out State_Type;
      Inverse_Mass : in     CM_Inverse_Mass_Type;
      Time_Step    : in     Real_Type);

   procedure Gen_State_Integrator
     (State        : in out State_Type;
      Inverse_Mass : in     CM_Inverse_Mass_Type;
      Time_Step    : in     Real_Type)
   is
      Cached_State_Indep_Wrench : constant Dual_Cart_Vector_Type := State_Indep_Wrench;

      function Compute_Derivative (State : in State_Type) return State_Dot_Type
      is begin
         return Derivative
           (State,
            Inverse_Mass,
            Translation (Cached_State_Indep_Wrench) + Translation (State_Dep_Wrench (State)),
            Rotation (Cached_State_Indep_Wrench) + Rotation (State_Dep_Wrench (State)),
            State_Indep_Rot_Mom);
      end Compute_Derivative;

      procedure Runge_Kutta is new SAL.Math_Float.Gen_Runge_Kutta_4th_Wertz
        (State_Type                 => State_Type,
         Derivative_Type            => State_Dot_Type,
         Compute_Derivative         => Compute_Derivative,
         Derivative_Plus_Derivative => Derivative_Plus_Derivative,
         Derivative_Plus_State      => Derivative_Plus_State,
         Derivative_Times_Time      => Derivative_Times_Time);
   begin
      Runge_Kutta (State, Time_Step);
   end Gen_State_Integrator;

   Delta_T : constant Real_Type := 0.1;

   function Rate_Obj (Base_Quat_T1, Base_Quat_T2 : in Unit_Quaternion_Type) return Cart_Vector_Type
   is
      T2_Quat_T1 : constant Unit_Quaternion_Type := Inverse_Times (Base_Quat_T2, Base_Quat_T1);
   begin
      --  Compute rate from state sequence
      return To_Rot_Vector (T2_Quat_T1) / Delta_T;
   end Rate_Obj;

   ----------
   --  Tests

   procedure Zero_Wrench (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Zero_Force_Integrator is new Gen_State_Integrator
        (State_Dep_Wrench    => Zero_Dep_Wrench,
         State_Indep_Wrench  => Zero_Indep_Wrench,
         State_Indep_Rot_Mom => Zero_Momentum);

      CM_State : State_Type :=
        (Translation => Zero_Cart_Vector,
         Rotation    => Zero_Unit_Quaternion,
         CM_Tran_Mom => (X | Y | Z => 0.0),
         CM_Rot_Mom  => (X | Y => 0.0, Z => 0.2));

      Mass : constant CM_Mass_Type :=
           (Total          => 5.0,
            Center_Inertia => ((1.0, 1.0, 2.0, 0.0, 0.0, 0.0)));

      Inverse_Mass : constant CM_Inverse_Mass_Type := Inverse (Mass);

      Expected_Rate_Obj : constant Cart_Vector_Type := (0.0, 0.0, 0.1);
      --  Rate = Mom / Mass = 0.2 / 2.0 = 0.1

      Base_Quat_Tn : Unit_Quaternion_Type renames CM_State.Rotation;

      --  For right multiply passive rotations, Base_Quat_Sc component
      --  has same sign as active rot_vector component (= sign of
      --  Rate_Obj component). See spacecraft_math_examples.adb,
      --  sec:quaternion_examples_right.

      Base_Quat_T1 : Unit_Quaternion_Type;
      Base_Quat_T2 : Unit_Quaternion_Type;
      Base_Quat_T3 : Unit_Quaternion_Type;
      Base_Quat_T4 : Unit_Quaternion_Type;

   begin
      Check ("1.Tran", CM_State.Translation, (0.00,  0.00,  0.00));
      Check ("1.Quat", Base_Quat_Tn, 0.00, 0.00, 0.0, 1.0);
      Check ("1.tran_mom", CM_State.CM_Tran_Mom, (0.00,  0.00,  0.00));
      Check ("1.rot_mom", CM_State.CM_Rot_Mom, (0.00,  0.00,  0.20));
      Base_Quat_T1 := Base_Quat_Tn;

      Zero_Force_Integrator (CM_State, Inverse_Mass, Delta_T);
      Check ("2.Tran", CM_State.Translation, (0.00,  0.00,  0.00));
      Check ("2.Quat", Base_Quat_Tn, 0.00, 0.00, 0.005, 0.999_99);
      Check ("2.tran_mom", CM_State.CM_Tran_Mom, (0.00,  0.00,  0.00));
      Check ("2.rot_mom", CM_State.CM_Rot_Mom, (0.00,  0.00,  0.20));
      Check ("2.rate", Rate_Obj (Base_Quat_T1, Base_Quat_Tn), Expected_Rate_Obj);
      Base_Quat_T2 := Base_Quat_Tn;

      Zero_Force_Integrator (CM_State, Inverse_Mass, Delta_T);
      Check ("3.Tran", CM_State.Translation, (0.00,  0.00,  0.00));
      Check ("3.Quat", Base_Quat_Tn, To_Unit_Quaternion (0.00, 0.00, 0.01, 0.999_99));
      Check ("3.tran_mom", CM_State.CM_Tran_Mom, (0.00,  0.00,  0.00));
      Check ("3.rot_mom", CM_State.CM_Rot_Mom, (0.00,  0.00,  0.20));
      Check ("3.rate", Rate_Obj (Base_Quat_T2, Base_Quat_Tn), Expected_Rate_Obj);
      Base_Quat_T3 := Base_Quat_Tn;

      Zero_Force_Integrator (CM_State, Inverse_Mass, Delta_T);
      Check ("4.Tran", CM_State.Translation, (0.00,  0.00,  0.00));
      Check ("4.Quat", Base_Quat_Tn, To_Unit_Quaternion (0.00, 0.00, 0.015, 0.999_99));
      Check ("4.tran_mom", CM_State.CM_Tran_Mom, (0.00,  0.00,  0.00));
      Check ("4.rot_mom", CM_State.CM_Rot_Mom, (0.00,  0.00,  0.20));
      Base_Quat_T4 := Base_Quat_Tn;
      Check ("4.rate", Rate_Obj (Base_Quat_T3, Base_Quat_T4), Expected_Rate_Obj);

   end Zero_Wrench;

   procedure Constant_Wrench (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Constant_Force_Integrator is new Gen_State_Integrator
        (State_Dep_Wrench    => Constant_Wrench,
         State_Indep_Wrench  => Zero_Indep_Wrench,
         State_Indep_Rot_Mom => Zero_Momentum);

      CM_State : State_Type :=
        (Translation => Zero_Cart_Vector,
         Rotation    => Zero_Unit_Quaternion,
         CM_Tran_Mom => (X | Y | Z => 0.0),
         CM_Rot_Mom  => (X | Y => 0.0, Z => 0.1));

      Base_Quat_Tn : Unit_Quaternion_Type renames CM_State.Rotation;
      CM_Mom_CM    : Cart_Vector_Type     renames CM_State.CM_Rot_Mom;

      Mass : constant CM_Mass_Type :=
           (Total          => 5.0,
            Center_Inertia => ((1.0, 1.0, 2.0, 0.0, 0.0, 0.0)));
      --  Translation acceleration = wrench TZ / mass.total = 0.1 / 5.0 = 0.02
      --  Rotation    acceleration = wrench RZ / mass.Center(Izz) = -0.1 / 2.0 = -0.05
      --  Initial rotation rate    = cm_mom_cm / mass.center(izz) = 0.1 / 2.0 = 0.05

      Inverse_Mass : constant CM_Inverse_Mass_Type := Inverse (Mass);

   begin
      Constant_Force_Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("1.Tran", CM_State.Translation, (0.0001,  0.00,  0.00));
      Check ("1.Quat", Base_Quat_Tn, 0.00, 0.00, 0.002_375, 0.999_99);
      Check ("1.Rot", -To_Rot_Vector (CM_State.Rotation), (0.00,  0.00,  0.00475));
      Check ("1.tran_mom", CM_State.CM_Tran_Mom, (0.01,  0.00,  0.00));
      Check ("1.rot_mom", CM_Mom_CM, (0.0, 0.0, 0.090));

      Constant_Force_Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("2.Tran", CM_State.Translation, (0.0004,  0.00,  0.00));
      Check ("2.Quat", Base_Quat_Tn, 0.00, 0.00, 0.004_5, 0.999_99);
      Check ("2.Rot", -To_Rot_Vector (CM_State.Rotation), (0.00,  0.00,  0.009));
      Check ("2.tran_mom", CM_State.CM_Tran_Mom, (0.02,  0.00,  0.00));
      Check ("2.rot_mom", CM_Mom_CM, (0.00,  0.00,  0.080));

      Constant_Force_Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("3.Tran", CM_State.Translation, (0.0009,  0.00,  0.00));
      Check ("3.Quat", Base_Quat_Tn, 0.00, 0.00, 0.006_375, 0.999_99);
      Check ("3.Rot", -To_Rot_Vector (CM_State.Rotation), (0.00,  0.00,  0.01275));
      Check ("3.tran_mom", CM_State.CM_Tran_Mom, (0.03,  0.00,  0.00));
      Check ("3.rot_mom", CM_Mom_CM, (0.00,  0.00,  0.070));

      Constant_Force_Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("4.Tran", CM_State.Translation, (0.0016,  0.00,  0.00));
      Check ("4.Rot", -To_Rot_Vector (CM_State.Rotation), (0.00,  0.00,  0.016));
      Check ("4.tran_mom", CM_State.CM_Tran_Mom, (0.04,  0.00,  0.00));
      Check ("4.rot_mom", CM_Mom_CM, (0.00,  0.00,  0.060));

   end Constant_Wrench;

   procedure Child_Rot_Momentum (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      function Child_Rot_Mom return Cart_Vector_Type
      is begin
         return (X => 0.5, others => 0.0);
      end Child_Rot_Mom;

      procedure Integrator is new Gen_State_Integrator
        (State_Dep_Wrench    => Constant_Wrench,
         State_Indep_Wrench  => Zero_Indep_Wrench,
         State_Indep_Rot_Mom => Child_Rot_Mom);

      Mass : constant CM_Mass_Type :=
           (Total          => 5.0,
            Center_Inertia => ((1.0, 1.0, 2.0, 0.0, 0.0, 0.0)));
      --  Translation acceleration = wrench TZ / mass.total = 0.1 / 5.0 = 0.02
      --  Rotation    acceleration = wrench RZ / mass.Center(Izz) = -0.1 / 2.0 = -0.05 + Euler

      Inverse_Mass : constant CM_Inverse_Mass_Type := Inverse (Mass);

      CM_State : State_Type :=
        (Translation => Zero_Cart_Vector,
         Rotation    => Zero_Unit_Quaternion,
         CM_Tran_Mom => (X | Y | Z => 0.0),
         CM_Rot_Mom  => (X | Y => 0.0, Z => 0.1));

      CM_Mom_CM : Cart_Vector_Type renames CM_State.CM_Rot_Mom;

   begin
      Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("1.Tran", CM_State.Translation, (0.0001,  0.000,  0.000));
      Check ("1.Rot", -To_Rot_Vector (CM_State.Rotation), (0.000, -0.000_12,  0.00475));
      Check ("1.tran_mom", CM_State.CM_Tran_Mom, (0.010,  0.000,  0.000));
      Check ("1.rot_mom", CM_Mom_CM, (0.000, -0.00237,  0.090));

      Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("2.Tran", CM_State.Translation, (0.0004,  0.000,  0.000));
      Check ("2.Rot", -To_Rot_Vector (CM_State.Rotation), (0.000, -0.00047,  0.009));
      Check ("2.tran_mom", CM_State.CM_Tran_Mom, (0.020,  0.000,  0.000));
      Check ("2.rot_mom", CM_Mom_CM, (0.000, -0.004496,  0.07977));

      Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("3.Tran", CM_State.Translation, (0.000899,  0.000,  0.000));
      Check ("3.Rot", -To_Rot_Vector (CM_State.Rotation), (0.000, -0.001,  0.01272));
      Check ("3.tran_mom", CM_State.CM_Tran_Mom, (0.030,  0.000,  0.000));
      Check ("3.rot_mom", CM_Mom_CM, (0.000, -0.00636,  0.06949));

      Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("4.Tran", CM_State.Translation, (0.001599,  0.000,  0.000));
      Check ("4.Rot", -To_Rot_Vector (CM_State.Rotation), (0.000, -0.00173,  0.016));
      Check ("4.tran_mom", CM_State.CM_Tran_Mom, (0.040,  0.000,  0.000));
      Check ("4.rot_mom", CM_Mom_CM, (0.000, -0.008,  0.059135));

      Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("5.Tran", CM_State.Translation, (0.0024998,  0.000,  0.000));
      Check ("5.Rot", -To_Rot_Vector (CM_State.Rotation), (0.000, -0.002597,  0.018636));
      Check ("5.tran_mom", CM_State.CM_Tran_Mom, (0.050,  0.000,  0.000));
      Check ("5.rot_mom", CM_Mom_CM, (0.000, -0.0093176,  0.0487));

      Integrator (CM_State, Inverse_Mass, 0.1);
      Check ("6.Tran", CM_State.Translation, (0.0035996,  0.000,  0.000));
      Check ("6.Rot", -To_Rot_Vector (CM_State.Rotation), (0.000, -0.003585,  0.020809));
      Check ("6.tran_mom", CM_State.CM_Tran_Mom, (0.060,  0.000,  0.000));
      Check ("6.rot_mom", CM_Mom_CM, (0.000108, -0.010404,  0.038207));

   end Child_Rot_Momentum;

   procedure Non_Principal_Axis (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Integrator is new Gen_State_Integrator
        (State_Dep_Wrench    => Zero_Dep_Wrench,
         State_Indep_Wrench  => Zero_Indep_Wrench,
         State_Indep_Rot_Mom => Zero_Momentum);

      --  No wrenches; momentum should be constant in the base frame,
      --  follow Euler's equations in the body frame. With these
      --  relatively large rates, we need to use a smaller step size,
      --  especially since we are using 32 bit floats.

      Time_Step : constant SAL.Math_Float.Real_Type := 0.01;

      Base_Momentum_Init : constant Dual_Cart_Vector_Type := (Tx | Ty | Tz => 0.0, Rx => 0.0, Ry => 1.0, Rz => 10.0);

      Momentum_Mag : constant Dual_Real_Type := Mag (Base_Momentum_Init);

      Mass : constant CM_Mass_Type :=
           (Total          => 50.0,
            Center_Inertia => ((500.0, 200.0, 100.0, 0.0, 0.0, 0.0)));

      Inverse_Mass : constant CM_Inverse_Mass_Type := Inverse (Mass);

      CM_State : State_Type :=
        (Translation => Zero_Cart_Vector,
         Rotation    => Zero_Unit_Quaternion,
         CM_Tran_Mom => Translation (Base_Momentum_Init),
         CM_Rot_Mom  => Rotation (Base_Momentum_Init));

      Base_R_CM : Unit_Quaternion_Type renames CM_State.Rotation;

   begin
      Check ("0.tran_mom_mag", Mag (CM_State.CM_Tran_Mom), Momentum_Mag (Tran));
      Check ("0.rot_mom_mag", Mag (CM_State.CM_Rot_Mom), Momentum_Mag (Rot));

      Integrator (CM_State, Inverse_Mass, Time_Step);
      Check ("1.Tran", CM_State.Translation, (0.0,  0.0,  0.0));
      Check ("1.Rot", CM_State.Rotation, To_Unit_Quaternion (0.00000,   0.00002,   0.00050,   1.00000));
      Check ("1.tran_mom_mag", Mag (CM_State.CM_Tran_Mom), Momentum_Mag (Tran));
      Check ("1.rot_mom_mag", Mag (CM_State.CM_Rot_Mom), Momentum_Mag (Rot));
      Check
        ("1.Base_rot_mom", Times_Inverse (CM_State.CM_Rot_Mom, Base_R_CM), Rotation (Base_Momentum_Init));
      Check ("1.tran_mom", CM_State.CM_Tran_Mom, (0.0, 0.0, 0.0));
      Check ("1.rot_mom", CM_State.CM_Rot_Mom, (0.000_5, 1.0,  10.0));

      for I in 2 .. 1001 loop
         Integrator (CM_State, Inverse_Mass, Time_Step);
      end loop;

      Check ("2.Tran", CM_State.Translation, (0.0,  0.0,  0.0));
      Check ("2.Rot", CM_State.Rotation, To_Unit_Quaternion (0.00272,   0.02282,   0.47990,   0.87703));
      Check ("2.tran_mom_mag", Mag (CM_State.CM_Tran_Mom), Momentum_Mag (Tran));
      Check ("2.rot_mom_mag", Mag (CM_State.CM_Rot_Mom), Momentum_Mag (Rot));
      Check
        ("2.Base_rot_mom", Times_Inverse (CM_State.CM_Rot_Mom, Base_R_CM), Rotation (Base_Momentum_Init));
      Check ("2.tran_mom", CM_State.CM_Tran_Mom, (0.0, 0.0, 0.0));
      Check ("2.rot_mom", CM_State.CM_Rot_Mom, (4.67823E-01,  8.06118E-01,  1.00066E+01));

   end Non_Principal_Axis;

   procedure Non_Principal_Axis_Tran (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Integrator is new Gen_State_Integrator
        (State_Dep_Wrench    => Zero_Dep_Wrench,
         State_Indep_Wrench  => Zero_Indep_Wrench,
         State_Indep_Rot_Mom => Zero_Momentum);

      --  No wrenches, some translation momentum. Momentum should be
      --  constant in the base frame, follow Euler's equations in the
      --  body frame. With these relatively large rates, we need to
      --  use a smaller step size, especially since we are using 32
      --  bit floats.

      Time_Step : constant SAL.Math_Float.Real_Type := 0.01;

      Base_Momentum_Init : constant Dual_Cart_Vector_Type :=
        (Tx => 1.0, Ty | Tz => 0.0, Rx => 0.0, Ry => 1.0, Rz => 10.0);

      Momentum_Mag : constant Dual_Real_Type := Mag (Base_Momentum_Init);

      Mass : constant CM_Mass_Type :=
           (Total          => 50.0,
            Center_Inertia => ((500.0, 200.0, 100.0, 0.0, 0.0, 0.0)));

      Inverse_Mass : constant CM_Inverse_Mass_Type := Inverse (Mass);

      CM_State : State_Type :=
        (Translation => Zero_Cart_Vector,
         Rotation    => Zero_Unit_Quaternion,
         CM_Tran_Mom => Translation (Base_Momentum_Init),
         CM_Rot_Mom  => Rotation (Base_Momentum_Init));
      --  CM_State.Rotation is passive Base_Q_CM
      --  CM_State.CM_Rot_Mom is in CM frame; cm_mom_cm
      --  cm_mom_Base = cm_mom_cm * Inverse (Base_Q_CM)
      --  Translation is all in base frame

      Base_R_CM : Unit_Quaternion_Type renames CM_State.Rotation;

   begin
      Check ("0.tran_mom_mag", Mag (CM_State.CM_Tran_Mom), Momentum_Mag (Tran));
      Check ("0.rot_mom_mag", Mag (CM_State.CM_Rot_Mom), Momentum_Mag (Rot));

      Integrator (CM_State, Inverse_Mass, Time_Step);
      Check ("1.Tran", CM_State.Translation, (2.00000E-04,  0.0, 0.0));
      Check ("1.Rot", CM_State.Rotation, To_Unit_Quaternion (0.00000,   0.00002,   0.00050,   1.00000));
      Check ("1.tran_mom_mag", Mag (CM_State.CM_Tran_Mom), Momentum_Mag (Tran));
      Check ("1.rot_mom_mag", Mag (CM_State.CM_Rot_Mom), Momentum_Mag (Rot));
      Check
        ("1.Base_rot_mom", Times_Inverse (CM_State.CM_Rot_Mom, Base_R_CM), Rotation (Base_Momentum_Init));
      Check ("1.tran_mom", CM_State.CM_Tran_Mom, (1.0, 0.0, 0.0));
      Check ("1.rot_mom", CM_State.CM_Rot_Mom, (0.000_5, 1.0,  10.0));

      for I in 2 .. 1001 loop
         Integrator (CM_State, Inverse_Mass, Time_Step);
      end loop;

      Check ("2.Tran", CM_State.Translation, (2.00202E-01,  0.0,  0.0));
      Check ("2.Rot", CM_State.Rotation, To_Unit_Quaternion (0.00272,   0.02282,   0.47990,   0.87703));
      Check ("2.tran_mom_mag", Mag (CM_State.CM_Tran_Mom), Momentum_Mag (Tran));
      Check ("2.rot_mom_mag", Mag (CM_State.CM_Rot_Mom), Momentum_Mag (Rot));
      Check
        ("2.Base_rot_mom", Times_Inverse (CM_State.CM_Rot_Mom, Base_R_CM), Rotation (Base_Momentum_Init));
      Check ("2.tran_mom", CM_State.CM_Tran_Mom, (1.0, 0.0, 0.0));
      Check ("2.rot_mom", CM_State.CM_Rot_Mom, (4.67823E-01,  8.06118E-01,  1.00066E+01));

   end Non_Principal_Axis_Tran;

   ----------
   --  Public routines

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Zero_Wrench'Access, "Zero_Wrench");
      Register_Routine (T, Constant_Wrench'Access, "Constant_Wrench");
      Register_Routine (T, Child_Rot_Momentum'Access, "Child_Rot_Momentum");
      Register_Routine (T, Non_Principal_Axis'Access, "Non_Principal_Axis");
      Register_Routine (T, Non_Principal_Axis_Tran'Access, "Non_Principal_Axis_Tran");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_DOF_6_Integrator_Utils_Wertz");
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

end Test_Math_Float_DOF_6_Integrator_Utils_Wertz;
