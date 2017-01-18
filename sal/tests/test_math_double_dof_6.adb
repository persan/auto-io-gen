--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2005, 2007 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with AUnit.Test_Cases.Registration;
with SAL.Math_Double.AUnit;
with SAL.Math_Double.DOF_3;
with SAL.Math_Double.DOF_6.AUnit;
with SAL.Math_Double.DOF_6.DC_Array_DCV_Inverse;
with SAL.Math_Double.DOF_6.Integrator_Utils.AUnit;
with SAL.Math_Double.DOF_6.Integrator_Utils.Left;
with SAL.Math_Double.DOF_6.Integrator_Utils.Wertz;
with SAL.Math_Double.DOF_6.Left;
with SAL.Math_Double.DOF_6.Wertz;
package body Test_Math_Double_DOF_6 is

   procedure Test_DOF_6 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double.DOF_6;
      use SAL.Math_Double.DOF_6.AUnit;
   begin
      Check ("Zero_Pose", Zero_Pose, Zero_Pose);
   end Test_DOF_6;

   procedure Test_DOF_6_DC_Array_DCV_Inverse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double.DOF_6;
      use SAL.Math_Double.DOF_6.AUnit;
      --  Just make sure this compiles

      A : constant DC_Array_DCV_Type :=
        ((1.0, 2.0, 3.0, 0.0, 0.0, 0.0),
         (5.0, 6.0, 7.0, 0.0, 0.0, 0.0),
         (9.0, -1.0, -2.0, 0.0, 0.0, 0.0),
         (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
         (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
         (0.0, 0.0, 0.0, 0.0, 0.0, 1.0));
   begin
      Check ("1", DC_Array_DCV_Inverse (DC_Array_DCV_Inverse (A)), A);
   end Test_DOF_6_DC_Array_DCV_Inverse;

   procedure Test_DOF_6_Left (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double.DOF_3;
      use SAL.Math_Double.DOF_6;
      use SAL.Math_Double.DOF_6.AUnit;
      use SAL.Math_Double.DOF_6.Left;
      A_Dual_Cart_Vector : constant Dual_Cart_Vector_Type := (1.0, 2.0, 3.0, 1.570796, 0.0, 0.0);
   begin
      Check ("To_Pose", To_Pose (A_Dual_Cart_Vector), ((1.0, 2.0, 3.0), To_Unit_Quaternion (0.707, 0.0, 0.0, 0.707)));
   end Test_DOF_6_Left;

   procedure Test_DOF_6_Wertz (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double.DOF_3;
      use SAL.Math_Double.DOF_6;
      use SAL.Math_Double.DOF_6.AUnit;
      use SAL.Math_Double.DOF_6.Wertz;
      A_Dual_Cart_Vector : constant Dual_Cart_Vector_Type := (1.0, 2.0, 3.0, 1.570796, 0.0, 0.0);
   begin
      Check ("To_Pose", To_Pose (A_Dual_Cart_Vector), ((1.0, 2.0, 3.0), To_Unit_Quaternion (-0.707, 0.0, 0.0, 0.707)));
   end Test_DOF_6_Wertz;

   procedure Test_DOF_6_Integrator_Utils (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double.DOF_3;
      use SAL.Math_Double.DOF_6.Integrator_Utils;
      use SAL.Math_Double.DOF_6.Integrator_Utils.AUnit;
      State : constant State_Type := (Zero_Cart_Vector, Zero_Unit_Quaternion, (others => 0.0), (others => 0.0));
   begin
      Check ("state", State, State);
   end Test_DOF_6_Integrator_Utils;

   procedure Test_DOF_6_Integrator_Utils_Left (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double.DOF_3;
      use SAL.Math_Double.DOF_6.Integrator_Utils;
      use SAL.Math_Double.DOF_6.Integrator_Utils.AUnit;
      use SAL.Math_Double.DOF_6.Integrator_Utils.Left;
      State : constant State_Type := (Zero_Cart_Vector, Zero_Unit_Quaternion, (others => 0.0), (others => 0.0));

      Derivative : constant State_Dot_Type := (others => (others => 0.0));
   begin
      Check ("state + derivative", State + Derivative, State + Derivative);
   end Test_DOF_6_Integrator_Utils_Left;

   procedure Test_DOF_6_Integrator_Utils_Wertz (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double.DOF_3;
      use SAL.Math_Double.DOF_6.Integrator_Utils;
      use SAL.Math_Double.DOF_6.Integrator_Utils.AUnit;
      use SAL.Math_Double.DOF_6.Integrator_Utils.Wertz;
      State : constant State_Type := (Zero_Cart_Vector, Zero_Unit_Quaternion, (others => 0.0), (others => 0.0));

      Derivative : constant State_Dot_Type := (others => (others => 0.0));
   begin
      Check ("derivative + state", Derivative + State, Derivative + State);
   end Test_DOF_6_Integrator_Utils_Wertz;

   ----------
   --  Public routines

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_DOF_6'Access, "Test_DOF_6");
      Register_Routine (T, Test_DOF_6_DC_Array_DCV_Inverse'Access, "Test_DOF_6_DC_Array_DCV_Inverse");
      Register_Routine (T, Test_DOF_6_Left'Access, "Test_DOF_6_Left");
      Register_Routine (T, Test_DOF_6_Wertz'Access, "Test_DOF_6_Wertz");
      Register_Routine (T, Test_DOF_6_Integrator_Utils'Access, "Test_DOF_6_Integrator_Utils");
      Register_Routine (T, Test_DOF_6_Integrator_Utils_Left'Access, "Test_DOF_6_Integrator_Utils_Left");
      Register_Routine (T, Test_DOF_6_Integrator_Utils_Wertz'Access, "Test_DOF_6_Integrator_Utils_Wertz");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Double_DOF_6");
   end Name;

   procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Double.AUnit.Default_Tolerance := 1.0e-4;
   end Set_Up_Case;

   procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Double.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Double_DOF_6;
