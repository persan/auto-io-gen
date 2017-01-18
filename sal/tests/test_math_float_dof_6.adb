--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2001 - 2003, 2005, 2007 Stephen Leake.  All Rights Reserved.
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


with SAL.AUnit;                  use SAL.AUnit;
with SAL.Math_Float.AUnit;       use SAL.Math_Float.AUnit;
with SAL.Math_Float.DOF_3.AUnit; use SAL.Math_Float.DOF_3.AUnit;
with SAL.Math_Float.DOF_6.AUnit; use SAL.Math_Float.DOF_6.AUnit;
with SAL.Math_Float.DOF_6.DC_Array_DCV_Inverse;
package body Test_Math_Float_DOF_6 is
   use SAL.Math_Float;
   use SAL.Math_Float.DOF_3;
   use SAL.Math_Float.DOF_6;

   ----------
   --  Test routines

   procedure Test_Dual_Real (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Dual_Real_Ops;
   begin
      Check ("(1.0, 1.0) <= (1.0, 1.0): ", Dual_Real_Type'(1.0, 1.0) <= Dual_Real_Type'(1.0, 1.0), True);
      Check ("(1.0, 1.0) <= (1.1, 1.1): ", Dual_Real_Type'(1.0, 1.0) <= Dual_Real_Type'(1.1, 1.1), True);
      Check ("(1.1, 1.1) <= (1.0, 1.0): ", Dual_Real_Type'(1.1, 1.1) <= Dual_Real_Type'(1.0, 1.0), False);
      Check ("(1.1, 1.0) <= (1.0, 1.1): ", Dual_Real_Type'(1.1, 1.0) <= Dual_Real_Type'(1.0, 1.1), False);
      Check ("(1.0, 1.1) <= (1.1, 1.0): ", Dual_Real_Type'(1.0, 1.1) <= Dual_Real_Type'(1.1, 1.0), False);

      Check ("2.0 * (3.0, 4.0) : ", 2.0 * Dual_Real_Type'(3.0, 4.0), (6.00000,  8.00000));
      Check ("(3.0, 4.0) * 2.0 : ", 2.0 * Dual_Real_Type'(3.0, 4.0), (6.00000,  8.00000));
   end Test_Dual_Real;

   procedure Test_Dual_Cart_Vector (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      A_Dual_Cart_Vector : constant Dual_Cart_Vector_Type := (1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
      A_Dual_Real        : constant Dual_Real_Type       := (7.0, 8.0);
   begin
      Check ("Translation : ", Translation (A_Dual_Cart_Vector), (1.00000,  2.00000,  3.00000));
      Check ("Rotation    : ", Rotation (A_Dual_Cart_Vector), (4.00000,  5.00000,  6.00000));
      Check
        ("Trans & Rot : ",
         Dual_Cart_Vector_Type'
           (Cart_Vector_Type'(1.0, 2.0, 3.0) &
              Cart_Vector_Type'(4.0, 5.0, 6.0)),
         (1.00000,  2.00000,  3.00000,  4.00000,  5.00000,  6.00000));

      Check ("Mag         : ", Mag (A_Dual_Cart_Vector), (3.74166,  8.77496));

      Check
        ("Dual_Real * DCV : ",
         A_Dual_Real * A_Dual_Cart_Vector,
         (7.00000, 14.00000, 21.00000, 32.00000, 40.00000, 48.00000));

      Check
        ("DCV * Dual_Real : ",
         A_Dual_Cart_Vector * A_Dual_Real,
         (7.00000, 14.00000, 21.00000, 32.00000, 40.00000, 48.00000));

      Check
        ("DCV / Dual_Real : ",
         A_Dual_Cart_Vector / A_Dual_Real,
        (0.14286,  0.28571,  0.42857,  0.50000,  0.62500,  0.75000));

   end Test_Dual_Cart_Vector;

   --  dc_array_dcv ops are all generic instantiations, tested in test_math_float_dof_3

   procedure Test_Rate_Wrench_Transform (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use DC_Array_DCV_Ops;

      B_Tr_A : constant Rate_Transform_Type := Unchecked_Rate_Transform
        (((1.00000,  0.00000,  0.00000),
          (0.00000,  0.99500,  0.09983),
          (0.00000, -0.09983,  0.99500)),
         ((0.00000,  0.00000,  0.00000),
          (0.00000, -0.09983,  0.99500),
          (0.00000, -0.99500, -0.09983)));

      B_Tw_A : constant Wrench_Transform_Type := Unchecked_Wrench_Transform
        (((1.00000,  0.00000,  0.00000),
          (0.00000,  0.99500,  0.09983),
          (0.00000, -0.09983,  0.99500)),
         ((0.00000,  0.00000,  0.00000),
          (0.00000, -0.09983,  0.99500),
          (0.00000, -0.99500, -0.09983)));

      Disp : constant Cart_Vector_Type := (1.0, 0.0, 0.0);
   begin
      Check
        ("To_Wrench_Transform (B_Tr_A)",
         To_Wrench_Transform (B_Tr_A),
         B_Tw_A);

      Check
        ("To_Rate_Transform (B_Tw_A)",
         To_Rate_Transform (B_Tw_A),
         B_Tr_A);

      Check
        ("1 Transform_Rate (vector, DCV)",
         Transform_Rate (Disp, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
         (1.00000,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000));

      Check
        ("2 Transform_Rate (Vector, DCV)",
         Transform_Rate (Disp, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
         (0.00000,   1.00000,   0.00000,   0.00000,   0.00000,   0.00000));

      Check
        ("3 Transform_Rate (Vector, DCV)",
         Transform_Rate (Disp, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
         (0.00000,   0.00000,   1.00000,   0.00000,  0.00000,   0.00000));

      Check
        ("4 Transform_Rate (Vector, DCV)",
         Transform_Rate (Disp, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
         (0.00000,   0.00000,   0.00000,   1.00000,   0.00000,   0.00000));

      Check
        ("5 Transform_Rate (Vector, DCV)",
         Transform_Rate (Disp, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
         (0.00000,   0.00000,  -1.00000,   0.00000,   1.00000,   0.00000));

      Check
        ("6 Transform_Rate (Vector, DCV)",
         Transform_Rate (Disp, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)),
         (0.00000,   1.00000,   0.00000,   0.00000,   0.00000,   1.00000));

      Check
        ("1 Transform_Wrench (vector, DCV)",
         Transform_Wrench (Disp, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
         (1.00000,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000));

      Check
        ("2 Transform_Wrench (Vector, DCV)",
         Transform_Wrench (Disp, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
         (0.00000,   1.00000,   0.00000,   0.00000,   0.00000,  -1.00000));

      Check
        ("3 Transform_Wrench (Vector, DCV)",
         Transform_Wrench (Disp, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
         (0.00000,   0.00000,   1.00000,   0.00000,   1.00000,   0.00000));

      Check
        ("4 Transform_Wrench (Vector, DCV)",
         Transform_Wrench (Disp, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
         (0.00000,   0.00000,   0.00000,   1.00000,   0.00000,   0.00000));

      Check
        ("5 Transform_Wrench (Vector, DCV)",
         Transform_Wrench (Disp, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
         (0.00000,   0.00000,   0.00000,   0.00000,   1.00000,   0.00000));

      Check
        ("6 Transform_Wrench (Vector, DCV)",
         Transform_Wrench (Disp, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)),
         (0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000));

      Check
        ("1 Transform_Force (vector, Vector)",
         Transform_Force (Disp, (1.0, 0.0, 0.0)),
         (1.00000,   0.00000,   0.00000,   0.00000,   0.00000,  0.00000));

      Check
        ("2 Transform_Force (Vector, Vector)",
         Transform_Force (Disp, (0.0, 1.0, 0.0)),
         (0.00000,   1.00000,   0.00000,   0.00000,   0.00000, -1.00000));

      Check
        ("3 Transform_Force (Vector, Vector)",
         Transform_Force (Disp, (0.0, 0.0, 1.0)),
         (0.00000,   0.00000,   1.00000,   0.00000,   1.00000,   0.00000));

   end Test_Rate_Wrench_Transform;

   procedure Test_Dual_Mag_Axis (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      A_Dual_Mag_Axis : constant Dual_Mag_Axis_Type := ((4.0, X_Unit), (2.0, Y_Unit));
      A_Dual_Float    : constant Dual_Real_Type    := (5.0, 6.0);
      A_Float         : constant Real_Type         := 5.0;

   begin
      Check
        ("Dual_Cart_Vector : ",
         To_Dual_Cart_Vector (A_Dual_Mag_Axis),
         (4.00000,  0.00000,  0.00000,  0.00000,  2.00000,  0.00000));

      Check
        (" and back        : ",
         To_Dual_Mag_Axis (To_Dual_Cart_Vector (A_Dual_Mag_Axis)),
         A_Dual_Mag_Axis);

      Check ("Mag              : ", Mag (A_Dual_Mag_Axis), (4.00000,  2.00000));

      Check
        ("- Dual_Mag_Axis  : ",
         -A_Dual_Mag_Axis,
         ((-4.00000, To_Unit_Vector (1.00000E+00,  0.00000E+00,  0.00000E+00)),
          (-2.00000, To_Unit_Vector (0.00000E+00,  1.00000E+00,  0.00000E+00))));

      Check
        ("Dual_Float * Dual_Mag_Axis : ",
         A_Dual_Float * A_Dual_Mag_Axis,
         ((20.00000, To_Unit_Vector (1.00000E+00,  0.00000E+00,  0.00000E+00)),
          (12.00000, To_Unit_Vector (0.00000E+00,  1.00000E+00,  0.00000E+00))));

      Check
        ("Dual_Mag_Axis * Dual_Float : ",
         A_Dual_Mag_Axis * A_Dual_Float,
         ((20.00000, To_Unit_Vector (1.00000E+00,  0.00000E+00,  0.00000E+00)),
          (12.00000, To_Unit_Vector (0.00000E+00,  1.00000E+00,  0.00000E+00))));

      Check
        ("Dual_Mag_Axis / Dual_Float : ",
         A_Dual_Mag_Axis / A_Dual_Float,
         ((0.80000, To_Unit_Vector (1.00000E+00,  0.00000E+00,  0.00000E+00)),
          (0.33333, To_Unit_Vector (0.00000E+00,  1.00000E+00,  0.00000E+00))));

      Check
        ("Float * Dual_Mag_Axis : ",
         A_Float * A_Dual_Mag_Axis,
         ((20.00000, To_Unit_Vector (1.00000E+00,  0.00000E+00,  0.00000E+00)),
          (10.00000, To_Unit_Vector (0.00000E+00,  1.00000E+00,  0.00000E+00))));

      Check
        ("Dual_Mag_Axis * Float : ",
         A_Dual_Mag_Axis * A_Float,
         ((20.00000, To_Unit_Vector (1.00000E+00,  0.00000E+00,  0.00000E+00)),
          (10.00000, To_Unit_Vector (0.00000E+00,  1.00000E+00,  0.00000E+00))));

      Check
        ("Dual_Mag_Axis / Float : ",
         A_Dual_Mag_Axis / A_Float,
         ((0.80000, To_Unit_Vector (1.00000E+00,  0.00000E+00,  0.00000E+00)),
          (0.40000, To_Unit_Vector (0.00000E+00,  1.00000E+00,  0.00000E+00))));
   end Test_Dual_Mag_Axis;

   procedure Test_Mass_Type (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      A_Mass : constant Mass_Type := To_Mass
         (Total          => 2.0,
          Center         => (0.0, 3.0, 4.0),
          Center_Inertia => (1.0, 2.0, 3.0, 4.0, 5.0, 6.0));

      procedure Test_Times
        (Label             : in String;
         Mass              : in Mass_Type;
         Velocity          : in Dual_Cart_Vector_Type;
         Expected_Momentum : in Dual_Cart_Vector_Type)
      is
         Momentum : constant Dual_Cart_Vector_Type := Mass * Velocity;
      begin
         Check (Label & " Momentum : ", Momentum, Expected_Momentum);
         Check (Label & " Velocity : ", Inverse_Times (Mass, Momentum), Velocity);
      end Test_Times;

   begin
      Check      ("Total          : ", Total (A_Mass), 2.00000);
      Check      ("Center         : ", Center (A_Mass), (0.00000,  3.00000,  4.00000));
      Check
        ("Center_Inertia : ",
         Center_Inertia (A_Mass),
         (1.00000,  2.00000,  3.00000,  4.00000,  5.00000,  6.00000));

      Check
        ("Inertia        : ",
         Inertia (A_Mass),
        (51.00000, 34.00000, 21.00000,  4.00000,  5.00000, -18.00000));

      Test_Times
        ("1",
         Mass              => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
         Velocity          => (1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
         Expected_Momentum => (2.00000,  2.00000,  2.00000,  0.00000,  0.00000,  0.00000));

      Test_Times
        ("2",
         Mass              => To_Mass (2.5, (0.0, 0.0, 0.0), (3.0, 4.0, 5.0, 0.0, 0.0, 0.0)),
         Velocity          => (1.0, 0.0, 0.0, 0.0, 0.0, 2.0),
         Expected_Momentum => (2.50000,  0.00000,  0.00000,  0.00000,  0.00000, 10.00000));

      Test_Times
        ("3",
         Mass              => To_Mass (2.5, (0.0, 1.5, 0.0), (3.0, 4.0, 5.0, 0.0, 0.0, 0.0)),
         Velocity          => (1.0, 0.0, 0.0, 0.0, 0.0, 2.0),
         Expected_Momentum => (-5.00000,  0.00000,  0.00000,  0.00000,  0.00000, 21.25000));

   end Test_Mass_Type;

   procedure Test_CM_Mass_Type (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test_Times
        (Label             : in String;
         Mass              : in CM_Mass_Type;
         Velocity          : in Dual_Cart_Vector_Type;
         Expected_Momentum : in Dual_Cart_Vector_Type)
      is
         Momentum : constant Dual_Cart_Vector_Type := Mass * Velocity;
      begin
         Check (Label & " Momentum : ", Momentum, Expected_Momentum);
         Check (Label & " Velocity : ", Inverse_Times (Mass, Momentum), Velocity);
      end Test_Times;

   begin
      Test_Times
        ("1",
         Mass              => (2.0, (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
         Velocity          => (1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
         Expected_Momentum => (2.00000,  2.00000,  2.00000,  0.00000,  0.00000,  0.00000));

      Test_Times
        ("2",
         Mass              => (2.5, (3.0, 4.0, 5.0, 0.0, 0.0, 0.0)),
         Velocity          => (1.0, 0.0, 0.0, 0.0, 0.0, 2.0),
         Expected_Momentum => (2.50000,  0.00000,  0.00000,  0.00000,  0.00000, 10.00000));

   end Test_CM_Mass_Type;

   procedure Test_DOF_6_DC_Array_DCV_Inverse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Dual_Real'Access, "Test_Dual_Real");
      Register_Routine (T, Test_Dual_Cart_Vector'Access, "Test_Dual_Cart_Vector");
      Register_Routine (T, Test_Rate_Wrench_Transform'Access, "Test_Rate_Wrench_Transform");
      Register_Routine (T, Test_Dual_Mag_Axis'Access, "Test_Dual_Mag_Axis");
      Register_Routine (T, Test_Mass_Type'Access, "Test_Mass_Type");
      Register_Routine (T, Test_CM_Mass_Type'Access, "Test_CM_Mass_Type");
      Register_Routine (T, Test_DOF_6_DC_Array_DCV_Inverse'Access, "Test_DOF_6_DC_Array_DCV_Inverse");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_DOF_6");
   end Name;

   overriding procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 1.0e-4;
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Float_DOF_6;
