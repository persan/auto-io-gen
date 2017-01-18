--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003 - 2006, 2009 Stephen Leake.  All Rights Reserved.
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
with AUnit.Test_Cases.Registration;
with SAL.Math_Float.AUnit;       use SAL.Math_Float.AUnit;
with SAL.Math_Float.DOF_3.AUnit; use SAL.Math_Float.DOF_3.AUnit;
with SAL.Math_Float.Scalar;      use SAL.Math_Float.Scalar;
package body Test_Math_Float_DOF_3 is

   use SAL.Math_Float, SAL.Math_Float.DOF_3;

   ----------
   --  Test cases

   procedure Unit_Vector (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Ac : constant Cart_Vector_Type     := (0.26726, 0.53452, 0.80178);
      Bc : constant Cart_Vector_Type     := (0.80178, 0.53452, 0.26726);
      Au : constant Unit_Vector_Type     := +((0.26726, 0.53452, 0.80178));
      Bu : constant Unit_Vector_Type     := +((0.80178, 0.53452, 0.26726));

      Raised_Exception : Boolean;

   begin
      declare
         procedure Ignore (Item : in Unit_Vector_Type)
         is
            pragma Unreferenced (Item);
         begin
            null;
         end Ignore;
      begin
         Ignore (To_Unit_Vector (0.0, 0.0, 0.0));
         Raised_Exception := False;
      exception
      when SAL.Non_Normalizable_Unit_Vector =>
         Raised_Exception := True;
      end;
      AUnit.Assertions.Assert (Raised_Exception, "did not raise NON_NORMALIZABLE_UNIT_QUATERNION");

      --  non-exception To_Unit_Vector tested implicitly in other tests.

      Check ("negate", -Au, -0.26726,  -0.53452,  -0.80178);

      Check ("scalar times", 5.0 * Au, (1.33631,   2.67261,   4.00892));
      Check ("times scalar", Au * 5.0, (1.33631,   2.67261,   4.00892));

            Check ("div scalar", Au / 5.0, (0.05345,   0.10690,   0.16036));

      Check ("Unit Dot Unit 1", Au * Au, 1.0);
      Check ("Unit Dot Cart 1", Au * Ac, 1.0);
      Check ("Cart Dot Unit 1", Ac * Au, 1.0);
      Check ("Unit Cross Unit 1", Cross (Au, Au), (0.00000,   0.00000,   0.00000));
      Check ("Cart Cross Unit 1", Cross (Ac, Au), (0.00000,   0.00000,   0.00000));
      Check ("Unit Cross Cart 1", Cross (Au, Ac), (0.00000,   0.00000,   0.00000));

      Check ("Unit Dot Unit 2", Au * Bu, 0.71429);
      Check ("Unit Dot Cart 2", Au * Bc, 0.71429);
      Check ("Cart Dot Unit 2", Ac * Bu, 0.71429);
      Check ("Unit Cross Unit 2", Cross (Au, Bu), (-0.28571,   0.57143,  -0.28571));
      Check ("Cart Cross Unit 2", Cross (Ac, Bu), (-0.28571,   0.57143,  -0.28571));
      Check ("Unit Cross Cart 2", Cross (Au, Bc), (-0.28571,   0.57143,  -0.28571));

   end Unit_Vector;

   procedure Mag_Axis (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      S    : constant Real_Type     := 5.0; --  Scalar
      MA_1 : constant Mag_Axis_Type := (2.0, +((0.26726,   0.53452,   0.80178)));

   begin
      Check ("       - Mag_Axis",    -MA_1, -2.00000, 0.26726,   0.53452,   0.80178);
      Check ("Scalar * Mag_Axis", S * MA_1, 10.00000, 0.26726,   0.53452,   0.80178);
      Check ("Mag_Axis * Scalar", MA_1 * S, 10.00000, 0.26726,   0.53452,   0.80178);
      Check ("Mag_Axis / Scalar", MA_1 / S, 0.40000,  0.26726,   0.53452,   0.80178);

      Check ("To_Mag_Axis 1",
             To_Mag_Axis (+(1.0, 0.0, 0.0), +(1.0, 0.0, 0.0)), 0.00000, 1.00000,   0.00000,   0.00000);
      Check ("To_Mag_Axis 2",
             To_Mag_Axis (+(0.0, 1.0, 0.0), +(1.0, 0.0, 0.0)), 1.57080, 0.00000,   0.00000,  -1.00000);
      Check ("To_Mag_Axis 3",
             To_Mag_Axis (+(0.0, 0.0, 1.0), +(0.0, 0.0, -1.0)), 3.14159, 1.00000,   0.00000,  0.00000);
   end Mag_Axis;

   procedure Quaternion_Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Q_Z : Unit_Quaternion_Type renames Zero_Unit_Quaternion;
      Q_6 : constant Unit_Quaternion_Type := To_Unit_Quaternion (2.0, 3.0, 4.0, 1.0);
      Q_7 : constant Unit_Quaternion_Type := To_Unit_Quaternion (3.0, 2.0, 1.0, 4.0);
      Q_8 : constant Unit_Quaternion_Type := To_Unit_Quaternion (1.0, 0.0, 0.0, 0.0);
      Q_9 : constant Unit_Quaternion_Type := To_Unit_Quaternion (2.0, 0.0, 0.0, 1.0);
   begin
      declare
         Item : Unit_Quaternion_Type;
         pragma Unreferenced (Item);
      begin
         Item := To_Unit_Quaternion (0.0, 0.0, 0.0, 0.0);
         AUnit.Assertions.Assert (False, "did not raise NON_NORMALIZABLE_UNIT_QUATERNION");
      exception
      when SAL.Non_Normalizable_Unit_Quaternion =>
         null;
      end;

      --  non-exception To_Unit_Quaternion tested implicitly in other tests

      Check ("Unchecked_Unit_Quaternion", Unchecked_Unit_Quaternion (0.0, 1.0, 2.0, 3.0), 0.0, 1.0, 2.0, 3.0);

      Check ("Mag Quat 6", Mag (Q_6), 2.77438);
      Check ("Inverse Quat 6", Inverse (Q_6), -0.36515, -0.54772, -0.73030, 0.18257);

      --  Note that negating all elements of Q gives the same rotation.
      Check ("Q * Q                1",                Q_Z  *         Q_Z,  0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Inverse Q * Q        1", Inverse       (Q_Z) *         Q_Z,  0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Inverse_Times (Q, Q) 1", Inverse_Times (Q_Z,           Q_Z), 0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Q * Inverse (Q)      1",                Q_Z * Inverse (Q_Z), 0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Times_Inverse (Q, Q) 1", Times_Inverse (Q_Z,           Q_Z), 0.00000,   0.00000,   0.00000,   1.00000);

      Check ("Q * Q                2",                Q_6  *         Q_7,  0.20000,   0.80000,   0.40000,  -0.40000);
      Check ("Inverse Q * Q        2", Inverse       (Q_6) *         Q_7,  0.00000,  -0.66667,  -0.33333,   0.66667);
      Check ("Inverse_Times (Q, Q) 2", Inverse_Times (Q_6,           Q_7), 0.00000,  -0.66667,  -0.33333,   0.66667);
      Check ("Q * Inverse (Q)      2",                Q_6 * Inverse (Q_7), 0.33333,   0.00000,   0.66667,   0.66667);
      Check ("Times_Inverse (Q, Q) 2", Times_Inverse (Q_6,           Q_7), 0.33333,   0.00000,   0.66667,   0.66667);

      Check ("Q * Q                3",                Q_8  *         Q_8,  0.00000,   0.00000,   0.00000,  -1.00000);
      Check ("Inverse Q * Q        3", Inverse       (Q_8) *         Q_8,  0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Inverse_Times (Q, Q) 3", Inverse_Times (Q_8,           Q_8), 0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Q * Inverse (Q)      3",                Q_8 * Inverse (Q_8), 0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Times_Inverse (Q, Q) 3", Times_Inverse (Q_8,           Q_8), 0.00000,   0.00000,   0.00000,   1.00000);

      Check ("Q * Q                4",                Q_9  *         Q_9,  0.80000,   0.00000,   0.00000,  -0.60000);
      Check ("Inverse Q * Q        4", Inverse       (Q_9) *         Q_9,  0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Inverse_Times (Q, Q) 4", Inverse_Times (Q_9,           Q_9), 0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Q * Inverse (Q)      4",                Q_9 * Inverse (Q_9), 0.00000,   0.00000,   0.00000,   1.00000);
      Check ("Times_Inverse (Q, Q) 4", Times_Inverse (Q_9,           Q_9), 0.00000,   0.00000,   0.00000,   1.00000);

   end Quaternion_Nominal;

   procedure Cart_Vector (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      C_Z : Cart_Vector_Type renames Zero_Cart_Vector;
      C_1 : constant Cart_Vector_Type := (1.0, 2.0, 3.0);
      C_2 : constant Cart_Vector_Type := (3.0, 2.0, 1.0);
      C_3 : constant Cart_Vector_Type := (2.0, 3.0, 4.0);

      MA_Z : constant Mag_Axis_Type := (0.0, X_Unit);
      MA_1 : constant Mag_Axis_Type := (3.74166, +(0.26726,   0.53452,   0.80178));

   begin
      Check ("Mag (1.0, 2.0, 3.0)", Mag (C_1), 3.74166);

      Check ("Cart_To_Mag_Axis 1", To_Mag_Axis (C_1), 3.74166, 0.26726,   0.53452,   0.80178);
      Check ("Mag_Axis_To_Cart 1", To_Cart_Vector (MA_1), (1.00000,   2.00000,   3.00000));

      Check ("Cart_To_Mag_Axis 2", To_Mag_Axis (C_Z), 0.0, 1.0,   0.0,   0.0);
      Check ("Mag_Axis_To_Cart 2", To_Cart_Vector (MA_Z), (0.00000,   0.00000,   0.00000));

      Check ("Cross 1", Cross (C_1, C_1), (0.00000,   0.00000,   0.00000));
      Check ("Cross 2", Cross (C_1, C_2), (-4.00000,   8.00000,  -4.00000));

      Check ("Angle Axis * C   X", Rotate             (0.1,  X,   C_3), (2.00000,   2.58568,   4.27952));
      Check ("To_Trig Axis * C X", Rotate (Sin_Cos    (0.1), X,   C_3), (2.00000,   2.58568,   4.27952));

      Check ("Angle Axis * C   Y", Rotate             (0.1,  Y,   C_3), (2.38934,   3.00000,   3.78035));
      Check ("To_Trig Axis * C Y", Rotate (Sin_Cos    (0.1), Y,   C_3), (2.38934,   3.00000,   3.78035));

      Check ("Angle Axis * C   Z", Rotate             (0.1,  Z,   C_3), (1.69051,   3.18468,   4.00000));
      Check ("To_Trig Axis * C Z", Rotate (Sin_Cos    (0.1), Z,   C_3), (1.69051,   3.18468,   4.00000));
   end Cart_Vector;

   procedure Celestial (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Cel_1 : constant Celestial_Coordinate_Type := (0.0, 0.0);
      Cel_2 : constant Celestial_Coordinate_Type := (0.1, 0.0);
      Cel_3 : constant Celestial_Coordinate_Type := (0.0, 0.1);
      Cel_4 : constant Celestial_Coordinate_Type := (0.1, 0.1);
      Cel_5 : constant Celestial_Coordinate_Type := (1.0, Pi / 2.0);
      Cel_6 : constant Celestial_Coordinate_Type := (1.0, -Pi / 2.0);
      Cel_7 : constant Celestial_Coordinate_Type := (5.0, 3.0);

      U_1 : constant Unit_Vector_Type := To_Unit_Vector (Cel_1);
      U_2 : constant Unit_Vector_Type := To_Unit_Vector (Cel_2);
      U_3 : constant Unit_Vector_Type := To_Unit_Vector (Cel_3);
      U_4 : constant Unit_Vector_Type := To_Unit_Vector (Cel_4);
      U_5 : constant Unit_Vector_Type := To_Unit_Vector (Cel_5);
      U_6 : constant Unit_Vector_Type := To_Unit_Vector (Cel_6);
      U_7 : constant Unit_Vector_Type := To_Unit_Vector (Cel_7);

   begin
      Check ("Celestial_To_Vect 1", To_Unit_Vector (Cel_1),  1.00000, 0.00000, 0.00000);
      Check ("Vect_To_Celestial 1", To_Celestial   (U_1), 0.00000, 0.00000);
      Check ("Celestial_To_Vect 2", To_Unit_Vector (Cel_2), 0.99500,   0.09983,  -0.00000);
      Check ("Vect_To_Celestial 2", To_Celestial   (U_2), 0.10000,  -0.00000);
      Check ("Celestial_To_Vect 3", To_Unit_Vector (Cel_3), 0.99500,   0.00000,   0.09983);
      Check ("Vect_To_Celestial 3", To_Celestial   (U_3),  0.00000,   0.10000);
      Check ("Celestial_To_Vect 4", To_Unit_Vector (Cel_4),  0.99003,   0.09933,   0.09983);
      Check ("Vect_To_Celestial 4", To_Celestial   (U_4),  0.10000,   0.10000);
      Check ("Celestial_To_Vect 5", To_Unit_Vector (Cel_5),  0.00000,   0.00000,   1.00000);
      Check ("Vect_To_Celestial 5", To_Celestial   (U_5),  0.00000,   1.57080);
      Check ("Celestial_To_Vect 6", To_Unit_Vector (Cel_6), -0.00000,  -0.00000,  -1.00000);
      Check ("Vect_To_Celestial 6", To_Celestial   (U_6),  4.14159,  -1.57080);
      Check ("Celestial_To_Vect 7", To_Unit_Vector (Cel_7), -0.28082,   0.94933,   0.14112);
      Check ("Vect_To_Celestial 7", To_Celestial   (U_7),  1.85841,   0.14159);
   end Celestial;

   procedure CACV_Inverse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      CACV_1 : constant Cart_Array_Cart_Vector_Type :=
        ((1.0, 0.0, 0.0),
         (0.0, 1.0, 0.0),
         (0.0, 0.0, 1.0));

      CACV_2 : constant Cart_Array_Cart_Vector_Type :=
        ((1.0, 2.0, 0.0),
         (2.0, 1.0, 0.0),
         (0.0, 0.0, 1.0));

      CACV_3 : constant Cart_Array_Cart_Vector_Type :=
            ((1.0, 2.0, 3.0),
             (2.0, 1.0, 0.0),
             (0.0, 3.0, 1.0));
   begin
      Check ("Identity", Inverse (CACV_1),
             ((1.0, 0.0, 0.0),
              (0.0, 1.0, 0.0),
              (0.0, 0.0, 1.0)));

      Check ("symmetric", Inverse (CACV_2),
               ((-0.33333,   0.66667,  -0.00000),
                (0.66667,  -0.33333,   0.00000),
                (-0.00000,   0.00000,   1.00000)));

      Check ("non-singular", Inverse (CACV_3),
            ((0.06667,   0.46667,  -0.20000),
             (-0.13333,   0.06667,   0.40000),
             (0.40000,  -0.20000,  -0.20000)));

   end CACV_Inverse;

   procedure Rot_Matrix_Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Check
        ("Unchecked_Rot_Matrix",
         Unchecked_Rot_Matrix
           (((1.0, 2.0, 3.0),
             (4.0, 5.0, 6.0),
             (7.0, 8.0, 9.0))),
         (((1.0, 2.0, 3.0),
           (4.0, 5.0, 6.0),
           (7.0, 8.0, 9.0))));

      declare
         Item : Rot_Matrix_Type;
         pragma Unreferenced (Item);
      begin
         Item :=
           To_Rot_Matrix
           (Cart_Array_Cart_Vector_Type'
              ((0.0, 0.0, 0.0),
               (0.0, 0.0, 0.0),
               (0.0, 0.0, 0.0)));
         AUnit.Assertions.Assert (False, "did not raise NON_NORMALIZABLE_ROT_MATRIX");
      exception
      when SAL.Non_Normalizable_Rot_Matrix =>
         null;
      end;
      --  IMPROVEME: test To_Rot_Matrix (M), M * M
   end Rot_Matrix_Nominal;

   procedure Inertia (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use CACV_Ops;
      Left : constant Inertia_Type := (1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   begin
      Check ("CACV (Left)", To_CACV (Left),
             ((1.00000,   4.00000,   5.00000),
              (4.00000,   2.00000,   6.00000),
              (5.00000,   6.00000,   3.00000)));

      Check ("Left * (1.0, 2.0, 3.0)", Left * (1.0, 2.0, 3.0), (24.00000,  26.00000,  26.00000));

      Check
        ("Parallel_Axis 1",
         Parallel_Axis (2.0, (1.0, 0.0, 0.0), (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
         (1.00000,   4.00000,   5.00000,   4.00000,   5.00000,   6.00000));

      Check
        ("Parallel_Axis 2",
         Parallel_Axis (2.0, (0.0, 1.0, 0.0), (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
         (3.00000,   2.00000,   5.00000,   4.00000,   5.00000,   6.00000));

      Check
        ("Parallel_Axis 1",
         Parallel_Axis (2.0, (0.0, 0.0, 1.0), (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
         (3.00000,   4.00000,   3.00000,   4.00000,   5.00000,   6.00000));

      Check
        ("Parallel_Axis 1",
         Parallel_Axis (2.0, (1.0, 2.0, 3.0), (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
         (27.00000,  22.00000,  13.00000,   0.00000,  -1.00000,  -6.00000));

      Check
        ("Interpolate 1",
         Interpolate
           (X  => 1.2,
            X1 => 1.0,
            X2 => 2.0,
            Y1 => (1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
            Y2 => (2.0, 4.0, 6.0, -4.0, 6.0, -12.0)),
         (1.2, 2.4, 3.6, 2.4, 5.2, 2.4));

   end Inertia;
   ----------
   --  Public routines

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Unit_Vector'Access, "Unit_Vector");
      Register_Routine (T, Mag_Axis'Access, "Mag_Axis");
      Register_Routine (T, Quaternion_Nominal'Access, "Quaternion_Nominal");
      Register_Routine (T, Cart_Vector'Access, "Cart_Vector");
      Register_Routine (T, Celestial'Access, "Celestial");
      Register_Routine (T, CACV_Inverse'Access, "CACV_Inverse");
      Register_Routine (T, Rot_Matrix_Nominal'Access, "Rot_Matrix_Nominal");
      Register_Routine (T, Inertia'Access, "Inertia");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test Math_Float.DOF_3");
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

end Test_Math_Float_DOF_3;
