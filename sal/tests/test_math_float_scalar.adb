--  Abstract:
--
--  see spec
--
--  Copyright (C) 2001 - 2003, 2007, 2009 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Numerics;
with Interfaces;
with SAL.AUnit;
with SAL.Math_Float.AUnit;  use SAL.Math_Float.AUnit;
with SAL.Math_Float.Scalar.AUnit;
with SAL.Math_Float.Scalar; use SAL.Math_Float.Scalar;
package body Test_Math_Float_Scalar is

   use SAL.Math_Float; -- for Float_Type

   ----------
   --  Test procedures

   --  must use package-level variable so optimizer doesn't optimize away exception
   Test_Mod_Junk : Real_Type;
   pragma Unreferenced (Test_Mod_Junk);

   procedure Test_Mod (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Check ("Mod (10.0, 5.0)", "Mod" (10.0, 5.0), 0.00000);
      Check ("Mod (11.0, 5.0)", "Mod" (11.0, 5.0), 1.00000);
      Check ("Mod (14.0, 5.0)", "Mod" (14.0, 5.0), 4.00000);
      Check ("Mod (10.0, -5.0)", "Mod" (10.0, -5.0), 0.00000);
      Check ("Mod (11.0, -5.0)", "Mod" (11.0, -5.0), -4.00000);
      Check ("Mod (14.0, -5.0)", "Mod" (14.0, -5.0), -1.00000);
      Check ("Mod (-10.0, 5.0)", "Mod" (-10.0, 5.0),  0.00000);
      Check ("Mod (-11.0, 5.0)", "Mod" (-11.0, 5.0),  4.00000);
      Check ("Mod (-14.0, 5.0)", "Mod" (-14.0, 5.0),  1.00000);
      Check ("Mod (-10.0, -5.0)", "Mod" (-10.0, -5.0),  0.00000);
      Check ("Mod (-11.0, -5.0)", "Mod" (-11.0, -5.0), -1.00000);
      Check ("Mod (-14.0, -5.0)", "Mod" (-14.0, -5.0), -4.00000);

      begin
         Test_Mod_Junk := "Mod" (10.0, 0.0);
         AUnit.Assertions.Assert (False, "Mod (10.0, 0.0); no Constraint_Error");

      exception
      when Constraint_Error =>
         null;
      end;

      Check ("Mod (0.0, 2.0)", "Mod" (0.0, 2.0), 0.0);
   end Test_Mod;

   procedure Test_Dead_Band (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Check ("Dead_Band (10.0, 2.0)", Dead_Band (10.0, 2.0),  8.00000);
      Check ("Dead_Band (1.0, 2.0)", Dead_Band (1.0, 2.0),  0.00000);
      Check ("Dead_Band (-10.0, 2.0)", Dead_Band (-10.0, 2.0), -8.00000);
      Check ("Dead_Band (-1.0, 2.0)", Dead_Band (-1.0, 2.0),  0.00000);
   end Test_Dead_Band;

   procedure Test_Detent (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Check ("Detent (-1.0, 2.0, 4.0", Detent (-1.0, 2.0, 4.0),  0.00000);
      Check ("Detent (1.0, 2.0, 4.0", Detent (1.0, 2.0, 4.0),  0.00000);
      Check ("Detent (3.0, 2.0, 4.0", Detent (3.0, 2.0, 4.0),  0.50000);
      Check ("Detent (-3.0, 2.0, 4.0", Detent (-3.0, 2.0, 4.0), -0.50000);
      Check ("Detent (5.0, 2.0, 4.0", Detent (5.0, 2.0, 4.0),  1.00000);
      Check ("Detent (-5.0, 2.0, 4.0", Detent (-5.0, 2.0, 4.0), -1.00000);
   end Test_Detent;

   Test_Limit_Junk : Limit_Type;
   pragma Unreferenced (Test_Limit_Junk);

   procedure Test_Limit (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.Math_Float.Scalar.AUnit;

      A_Limit : constant Limit_Type := To_Limit (-1.0, 1.0);
   begin
      Check ("To_Limit (-1.0, 1.0", A_Limit, -1.0, 1.0);
      begin
         Test_Limit_Junk := To_Limit (1.0, -1.0);
         AUnit.Assertions.Assert (False, "Invalid_Limit NOT raised");
      exception
      when SAL.Invalid_Limit =>
         null;
      end;
      Check ("Low  ((-1.0, 1.0))", Low (A_Limit), -1.0);
      Check ("High ((-1.0, 1.0))", High (A_Limit), 1.0);
      Check ("(-1.0, 1.0) and (-0.5, 0.5)", To_Limit (-1.0, 1.0) and To_Limit (-0.5, 0.5), -0.50000,  0.50000);
      Check ("(-1.0, 1.0) and (-0.5, 1.5)", To_Limit (-1.0, 1.0) and To_Limit (-0.5, 1.5), -0.50000,  1.00000);
      Check ("(-1.0, 1.0) and (-1.5, 0.5)", To_Limit (-1.0, 1.0) and To_Limit (-1.5, 0.5), -1.00000,  0.50000);
      begin
         Test_Limit_Junk := To_Limit (-1.0, 1.0) and To_Limit (1.5, 2.5);
         AUnit.Assertions.Assert (False, "Invalid_Limit NOT raised");
      exception
      when SAL.Invalid_Limit =>
         null;
      end;
      begin
         Test_Limit_Junk := To_Limit (-1.0, 1.0) and To_Limit (-2.5, -1.5);
         AUnit.Assertions.Assert (False, "Invalid_Limit NOT raised");
      exception
      when SAL.Invalid_Limit =>
         null;
      end;
   end Test_Limit;

   procedure Test_Compute_Limit (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Float.Scalar.AUnit;

      procedure Test_One
        (Message      : in String;
         Input_Scale  : in Real_Type;
         Input_Offset : in Real_Type;
         Default      : in Scale_Limit_Type;
         Expected     : in Limit_Type)
      is begin
         Check (Message, Compute_Limit (Input_Scale, Input_Offset, Default), Expected);
      end Test_One;

   begin

      SAL.Math_Float.AUnit.Default_Tolerance := 1.0e-5;

      Test_One
        ("1",
         Input_Scale  => 1.0,
         Input_Offset => 0.0,
         Default      => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected     => To_Limit (High => 10.0, Low => -10.0));

      Test_One
        ("2",
         Input_Scale  => 2.0,
         Input_Offset => 0.0,
         Default      => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected     => To_Limit (High => 5.0, Low => -5.0));

      Test_One
        ("3",
         Input_Scale  => -2.0,
         Input_Offset => 0.0,
         Default      => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected     => To_Limit (High => 5.0, Low => -5.0));

      Test_One
        ("4",
         Input_Scale  => -2.0,
         Input_Offset => 1.0,
         Default      => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected     => To_Limit (High => 5.5, Low => -4.5));

      Test_One
        ("5",
         Input_Scale  => 1.0,
         Input_Offset => 0.0,
         Default      => (Scale => 2.0, Offset => 1.0, Limit => To_Limit (High => 4.5, Low => -5.5)),
         Expected     => To_Limit (High => 10.0, Low => -10.0));

      Test_One
        ("6",
         Input_Scale  => 1.0,
         Input_Offset => 0.0,
         Default      => (Scale => -2.0, Offset => -1.0, Limit => To_Limit (High => 4.5, Low => -5.5)),
         Expected     => To_Limit (High => 10.0, Low => -10.0));

      Test_One
        ("7",
         Input_Scale  => -1.0,
         Input_Offset => 1.0,
         Default      => (Scale => -2.0, Offset => -1.0, Limit => To_Limit (High => 4.5, Low => -5.5)),
         Expected     => To_Limit (High => 11.0, Low => -9.0));

      Test_One
        ("8",
         Input_Scale  => -1.0,
         Input_Offset => 1.0,
         Default      => (Scale => 2.0, Offset => 1.0, Limit => To_Limit (High => 4.5, Low => -5.5)),
         Expected     => To_Limit (High => 11.0, Low => -9.0));

   end Test_Compute_Limit;

   procedure Test_Clip (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Test
        (Item             : in Real_Type;
         Limit            : in Limit_Type;
         Expected_Item    : in Real_Type;
         Expected_Clipped : in Boolean)
      is
         Temp_Item : Real_Type := Item;
         Clipped   : Boolean;
         use SAL.AUnit;
      begin
         Clip (Temp_Item, Limit, Clipped);
         Check (Real_Type'Image (Item) & ".Item", Temp_Item, Expected_Item);
         Check (Real_Type'Image (Item) & ".Clipped", Clipped, Expected_Clipped);
      end Test;
   begin
      Test (-2.0, To_Limit (-1.0, 1.0), -1.00000, True);
      Test (-1.0, To_Limit (-1.0, 1.0), -1.00000, False);
      Test (0.0, To_Limit (-1.0, 1.0),  0.00000, False);
      Test (1.0, To_Limit (-1.0, 1.0),  1.00000, False);
      Test (2.0, To_Limit (-1.0, 1.0),  1.00000, True);
   end Test_Clip;

   procedure Test_Less_Equal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.AUnit;

   begin
      Check ("-3.0 <= (-2.0, 2.0)", -3.0 <= To_Limit (-2.0, 2.0), False);
      Check ("-2.0 <= (-2.0, 2.0)", 2.0 <= To_Limit (-2.0, 2.0), True);
      Check (" 0.0 <= (-2.0, 2.0)", 0.0 <= To_Limit (-2.0, 2.0), True);
      Check (" 2.0 <= (-2.0, 2.0)", 2.0 <= To_Limit (-2.0, 2.0), True);
      Check (" 3.0 <= (-2.0, 2.0)", 3.0 <= To_Limit (-2.0, 2.0), False);
   end Test_Less_Equal;

   procedure Test_Scale_Limit (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Interfaces;
      use SAL.AUnit;
      use SAL.Math_Float.Scalar.AUnit;

      procedure Test
        (Label            : in String;
         Param            : in Scale_Limit_Type;
         Item             : in Real_Type;
         Expected_Item    : in Real_Type;
         Expected_Result  : in Real_Type;
         Expected_Clipped : in Boolean)
      is
         Computed_Item : Real_Type := Item;
         Result        : Real_Type;
         Clipped       : Boolean;
      begin
         Scale_Limit (Computed_Item, Param, Result, Clipped);
         Check (Label & ".Item", Computed_Item, Expected_Item);
         Check (Label & ".Result", Result, Expected_Result);
         Check (Label & ".Clipped", Clipped, Expected_Clipped);
      end Test;

      procedure Test
        (Label            : in String;
         Scale_Limit      : in Scale_Limit_Type;
         Safe_Result      : in Limit_Type;
         Expected_Low     : in Real_Type;
         Expected_High    : in Real_Type;
         Expected_Clipped : in Boolean)
      is
         Temp    : Scale_Limit_Type := Scale_Limit;
         Clipped : Boolean;
      begin
         Clip_Scale_Limit (Temp, Safe_Result, Clipped);
         Check (Label & ".Limit", Temp.Limit, Expected_Low, Expected_High);
         Check (Label & ".Clipped", Clipped, Expected_Clipped);
      end Test;

      Scale_Limit_1 : constant Scale_Limit_Type :=
        (Scale  => 2.0,
         Offset => 0.5,
         Limit  => To_Limit (Low => 1.0, High => 3.0));
      --  Result_Limit = (2.5, 6.5)

      Scale_Limit_2 : constant Scale_Limit_Type :=
        (Scale  => -2.0,
         Offset => 0.5,
         Limit  => To_Limit (Low => -3.0, High => -1.0));
      --  Result_Limit = (2.5, 6.5)

      Temp_Integer_16  : Integer_16;
      pragma Unreferenced (Temp_Integer_16);
      Temp_Unsigned_16 : Unsigned_16;
      pragma Unreferenced (Temp_Unsigned_16);
   begin
      Test ("Scale_Limit 1 low", Scale_Limit_1, 0.0, 1.0, 2.5, True);
      Test ("Scale_Limit 1 ok", Scale_Limit_1, 1.1, 1.1, 2.7, False);
      Test ("Scale_Limit 1 High", Scale_Limit_1, 3.1, 3.0, 6.5, True);

      Test ("Scale_Limit 2 low", Scale_Limit_2, -4.0, -3.0, 6.5, True);
      Test ("Scale_Limit 2 ok", Scale_Limit_2, -2.0, -2.0, 4.5, False);
      Test ("Scale_Limit 2 High", Scale_Limit_2, -0.1, -1.0, 2.5, True);

      Check ("Compute_Limit 1", Compute_Limit (2.0, 1.0, Scale_Limit_1), 0.75, 2.75);
      Check ("Compute_Limit 2", Compute_Limit (-2.0, 1.0, Scale_Limit_2), -2.75, -0.75);

      Test ("clip_scale_limit 1 low", Scale_Limit_1, To_Limit (3.0, 6.5), 1.25, 3.0, True);
      Test ("clip_scale_limit 1 ok", Scale_Limit_1, To_Limit (2.5, 6.5), 1.0, 3.0, False);
      Test ("clip_scale_limit 1 high", Scale_Limit_1, To_Limit (2.5, 6.0), 1.0, 2.75, True);

      Test ("clip_scale_limit 2 low", Scale_Limit_2, To_Limit (3.0, 6.5), -3.0, -1.25, True);
      Test ("clip_scale_limit 2 ok", Scale_Limit_2, To_Limit (2.5, 6.5), -3.0, -1.0, False);
      Test ("clip_scale_limit 2 high", Scale_Limit_2, To_Limit (2.5, 6.0), -2.75, -1.0, True);

      --  Verify no Constraint_Error for these
      Temp_Integer_16  := Integer_16 (Integer_16_First_Real);
      Temp_Integer_16  := Integer_16 (Integer_16_Last_Real);
      Temp_Unsigned_16 := Unsigned_16 (Unsigned_16_First_Real);
      Temp_Unsigned_16 := Unsigned_16 (Unsigned_16_Last_Real);

   end Test_Scale_Limit;

   --  IMPROVEME: Test clip_scale_limit

   Test_Sin_Cos_Junk      : Real_Type;
   pragma Unreferenced (Test_Sin_Cos_Junk);
   Test_Sin_Cos_Junk_Trig : Trig_Pair_Type;
   pragma Unreferenced (Test_Sin_Cos_Junk_Trig);

   procedure Test_Sin_Cos (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.Math_Float.Scalar.AUnit;

      procedure Test_Angle (I : in Integer)
      is
         --  Just check that sin, cos, atan2 are instantiated properly
         Angle : constant Real_Type      := Pi / 8.0 * Real_Type (I);
         Trig  : constant Trig_Pair_Type := Sin_Cos (Angle);
      begin
         Check (Integer'Image (I) & ".Atan2", Atan2 (Trig), Angle);
      end Test_Angle;
   begin
      for I in -7 .. 7 loop
         Test_Angle (I);
      end loop;

      begin
         Test_Sin_Cos_Junk := Atan2 (Unchecked_Trig_Pair (0.0, 0.0));
         AUnit.Assertions.Assert (False, "Atan2 (Unchecked_Trig_Pair (0.0, 0.0))");
      exception
      when Ada.Numerics.Argument_Error =>
         null;
      end;

      Check ("To_Trig_Pair (0.0, 1.0)", To_Trig_Pair (0.0, 1.0), 0.0, 1.0);
      Check ("To_Trig_Pair (4.0, 3.0)", To_Trig_Pair (4.0, 3.0), 0.8, 0.6);
      begin
         Test_Sin_Cos_Junk_Trig := To_Trig_Pair (0.0, 0.0);
         AUnit.Assertions.Assert (False, "To_Trig_Pair (0.0, 0.0) : Non_Normalizable_Trig_Pair not raised");
      exception
      when SAL.Non_Normalizable_Trig_Pair =>
         null;
      end;

   end Test_Sin_Cos;

   procedure Test_Trig_Sum_Diff (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.Math_Float.Scalar.AUnit;

      Small              : constant Real_Type      := First_Order_Trig / 2.0;
      Sin_Cos_Small      : constant Trig_Pair_Type := Sin_Cos (Small);
      Half_Sin_Cos_Small : constant Trig_Pair_Type := Half_Trig (Sin_Cos_Small);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 0.000_001;
      Check ("Small", Small, 1.72633E-04);
      Check ("Half_Trig (Sin_Cos (0.0))", Half_Trig (Sin_Cos (0.0)), 0.0, 1.0);
      Check ("Half_Trig (Sin_Cos (small))", Half_Sin_Cos_Small, 8.63167E-05, 1.00000E+00);
      Check ("Half_Trig (Sin_Cos (PI/2.0))", Atan2 (Half_Trig (Sin_Cos (Pi / 2.0))), 0.785_398);
      Check ("Half_Trig (Sin_Cos (-PI/4.0))", Atan2 (Half_Trig (Sin_Cos (-Pi/4.0))), -0.39270);

      Check ("Sin_Cos ((PI-small))", Atan2 (Sin_Cos (Pi - Small)), 3.14142);
      Check ("Sin_Cos ((PI-small)/2)", Atan2 (Sin_Cos ((Pi - Small) / 2.0)), 1.57071);
      Check ("Half_Trig (Sin_Cos (PI - small))", Atan2 (Half_Trig (Sin_Cos (Pi - Small))), 1.570_80,
             Tolerance => 0.000_01);

      Check ("Double_Trig (Sin_Cos (0.0))", Atan2 (Double_Trig (Sin_Cos (0.0))), 0.00000);
      Check ("Double_Trig (Sin_Cos (-0.2))", Atan2 (Double_Trig (Sin_Cos (-0.2))), -0.40000);
      Check ("Double_Trig (Sin_Cos (Pi/4.0))", Atan2 (Double_Trig (Sin_Cos (Pi / 4.0))), 1.57080,
             Tolerance => 0.000_01);

      Check ("Sin_Cos (0.1) + Sin_Cos (0.2)", Atan2 (Sin_Cos (0.1) + Sin_Cos (0.2)), 0.30000);
      Check ("Sin_Cos (0.1) - Sin_Cos (0.2)", Atan2 (Sin_Cos (0.1) - Sin_Cos (0.2)), -0.10000);
   end Test_Trig_Sum_Diff;

   ----------
   --  Public subprograms

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Mod'Access, "Test_Mod");
      Register_Routine (T, Test_Dead_Band'Access, "Test_Dead_Band");
      Register_Routine (T, Test_Detent'Access, "Test_Detent");
      Register_Routine (T, Test_Limit'Access, "Test_Limit");
      Register_Routine (T, Test_Compute_Limit'Access, "Test_Compute_Limit");
      Register_Routine (T, Test_Clip'Access, "Test_Clip");
      Register_Routine (T, Test_Less_Equal'Access, "Test_Less_Equal");
      Register_Routine (T, Test_Scale_Limit'Access, "Test_Scale_Limit");
      Register_Routine (T, Test_Sin_Cos'Access, "Test_Sin_Cos");
      Register_Routine (T, Test_Trig_Sum_Diff'Access, "Test_Trig_Sum_Diff");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_math_float_scalar");
   end Name;

   procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 0.000_000_1;
   end Set_Up_Case;

   procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Float_Scalar;
