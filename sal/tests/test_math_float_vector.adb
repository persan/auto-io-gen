--  Abstract :
--
--  See spec
--
--  Copyright (C) 2001, 2002, 2003, 2006 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with SAL.AUnit;
with SAL.Gen_Math.Gen_Vector;
with SAL.Math_Float.AUnit;
with SAL.Math_Float.Elementary;
with SAL.Math_Float.Scalar.AUnit;
package body Test_Math_Float_Vector is

   type Index_Type is (A, B, C);

   type Index_Array_Boolean_Type is array (Index_Type) of Boolean;
   type Index_Array_Real_Type is array (Index_Type) of SAL.Math_Float.Real_Type;
   type Index_Array_Limit_Type is array (Index_Type) of SAL.Math_Float.Scalar.Limit_Type;

   package Vector_Math is new SAL.Math_Float.Gen_Vector
      (Elementary               => SAL.Math_Float.Elementary,
       Math_Scalar              => SAL.Math_Float.Scalar,
       Index_Type               => Index_Type,
       Index_Array_Boolean_Type => Index_Array_Boolean_Type,
       Index_Array_Real_Type    => Index_Array_Real_Type,
       Index_Array_Limit_Type   => Index_Array_Limit_Type);
   use Vector_Math;

   procedure Check is new SAL.AUnit.Gen_Check_Discrete (Index_Type);

   procedure Check is new SAL.AUnit.Gen_Check_Array_Tolerance
     (Item_Type         => SAL.Math_Float.Real_Type,
      Index_Type        => Index_Type,
      Array_Type        => Index_Array_Real_Type,
      Tolerance_Type    => SAL.Math_Float.Real_Type,
      Check_Index       => Check,
      Check_Item        => SAL.Math_Float.AUnit.Check,
      Default_Tolerance => SAL.Math_Float.AUnit.Default_Tolerance);

   procedure Check is new SAL.AUnit.Gen_Check_Array_Tolerance
     (Item_Type         => SAL.Math_Float.Scalar.Limit_Type,
      Index_Type        => Index_Type,
      Array_Type        => Index_Array_Limit_Type,
      Tolerance_Type    => SAL.Math_Float.Real_Type,
      Check_Index       => Check,
      Check_Item        => SAL.Math_Float.Scalar.AUnit.Check,
      Default_Tolerance => SAL.Math_Float.AUnit.Default_Tolerance);

   procedure Check is new SAL.AUnit.Gen_Check_Array
     (Item_Type   => Boolean,
      Index_Type  => Index_Type,
      Array_Type  => Index_Array_Boolean_Type,
      Check_Index => Check,
      Check_Item  => SAL.AUnit.Check);

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.AUnit;
      use SAL.Math_Float.AUnit;
      use SAL.Math_Float.Scalar;
   begin
      Check ("Any 1", Any ((True, False, False)), True);
      Check ("Any 2", Any ((False, False, False)), False);
      Check ("Any 3", Any ((True, True, True)), True);

      Check ("- unary", -(1.0, 2.0, 3.0), (-1.0, -2.0, -3.0));
      Check ("abs", abs (-1.0, 2.0, -3.0), (1.0, 2.0, 3.0));

      Check ("v + v", (-1.0, 2.0, -3.0) + (-1.0, 2.0, -3.0), (-2.0, 4.0, -6.0));
      Check ("v - v", (-1.0, 2.0, -3.0) - (-1.0, 2.0, -3.0), (0.0, 0.0, 0.0));
      Check ("v * v", (-1.0, 2.0, -3.0) * (-1.0, 2.0, -3.0), (1.0, 4.0, 9.0));
      Check ("v / v", (-1.0, 2.0, -3.0) / (-1.0, 2.0, -3.0), (1.0, 1.0, 1.0));

      Check ("v + s", (-1.0, 2.0, -3.0) + 3.0, (2.0,   5.0,   0.0));
      Check ("v - s", (-1.0, 2.0, -3.0) - 3.0, (-4.0,  -1.0,  -6.0));
      Check ("v * s", (-1.0, 2.0, -3.0) * 3.0, (-3.0,   6.0,  -9.0));
      Check ("v / s", (-1.0, 2.0, -3.0) / 3.0, (-0.33333,   0.66667,  -1.0));

      Check ("s + v", 3.0 + (-1.0, 2.0, -3.0), (2.0,   5.0,   0.0));
      Check ("s - v", 3.0 - (-1.0, 2.0, -3.0), (4.0,   1.0,   6.0));
      Check ("s * v", 3.0 * (-1.0, 2.0, -3.0), (-3.0,   6.0,  -9.0));
      Check ("s / v", 3.0 / (-1.0, 2.0, -3.0), (-3.0,   1.5,  -1.0));

      Check
        ("Interpolate",
         Interpolate
           (X  => 1.2,
            X1 => 1.0,
            X2 => 2.0,
            Y1 => (0.0, 2.0, 3.0),
            Y2 => (-1.0, 3.0, 6.0)),
         (-0.2, 2.2, 3.6));

      Check ("Dot", (-1.0, 2.0, -3.0) * (-1.0, 2.0, -3.0), 14.0);

      Check ("Mask", Mask ((-1.0,  2.0, -3.0), (True, False, True)), (0.0,   2.0,   0.0));

      Check
        ("dead_band",
         Dead_Band
           (Item =>        (-1.0,  2.0, -3.0),
            Lower_Limit => (2.0,  0.5,  0.5)),
         (0.00000,   1.50000,  -2.50000));

      Check
        ("Detent",
         Detent
           (Item =>        (-1.0,  2.0, -3.0),
            Dead_Band   => (2.0,  0.5,  0.5),
            Upper_Limit => (3.0,  2.5,  2.5)),
         (0.00000,   0.75000,  -1.00000));

      Check
        ("To_Limit",
         To_Limit
           (Low =>   (-2.0, -2.0, -1.0),
            High =>  (1.0, 2.0, 3.0)),
         (To_Limit (-2.00000,  1.00000), To_Limit (-2.00000,  2.00000), To_Limit (-1.00000,  3.00000)));

      Check
        ("and limit",
         Index_Array_Limit_Type'
           (Vector_Math."and"
              (Left  => (To_Limit (-1.0, 1.0), To_Limit (-2.0, 2.0), To_Limit (-3.0, 3.0)),
               Right => (To_Limit (-2.0, 1.0), To_Limit (-2.0, 3.0), To_Limit (-1.0, 1.0)))),
         (To_Limit (-1.00000,  1.00000), To_Limit (-2.00000,  2.00000), To_Limit (-1.00000,  1.00000)));

      declare
         Item    : Index_Array_Real_Type           := (-1.0,  2.0, -3.0);
         Limit   : constant Index_Array_Limit_Type :=
           (To_Limit (-1.0, 1.0), To_Limit (-1.0, 1.0), To_Limit (-1.0, 1.0));
         Clipped : Index_Array_Boolean_Type;
      begin
         Clip (Item, Limit, Clipped);
         Check ("Clip Item", Item, (-1.00000,   1.00000,  -1.00000));
         Check ("Clip Clipped", Clipped, (False, True, True));
      end;

      declare
         Item  : constant Index_Array_Real_Type  := (-1.0,  2.0, -3.0);
         Limit : constant Index_Array_Limit_Type := (To_Limit (-1.0, 1.0), To_Limit (-1.0, 1.0), To_Limit (-1.0, 1.0));
      begin
         Check ("<= limit", Item <= Limit, False);
      end;

      declare
         procedure Test
           (Label           : in String;
            Item            : in Index_Array_Real_Type;
            Limit           : in SAL.Math_Float.Real_Type;
            Expected_Scaled : in Boolean;
            Expected_Item   : in Index_Array_Real_Type)
         is
            Scaled_Item : Index_Array_Real_Type := Item;
            Scaled      : Boolean;
         begin
            Scale_Limit (Scaled_Item, Limit, Scaled);

            Check ("Scale_Limit." & Label & ".Item", Scaled_Item, Expected_Item);
            Check ("Scale_Limit." & Label & ".Item", Scaled, Expected_Scaled);
         end Test;

      begin
         Test
           ("1",
            Item            => (-1.0,  2.0, -3.0),
            Limit           => -1.0,
            Expected_Scaled => True,
            Expected_Item   => (-0.33333,   0.66667,  -1.00000));

         Test
           ("2",
            Item            => (-1.0,  2.0, -3.0),
            Limit           => 4.0,
            Expected_Scaled => False,
            Expected_Item   => (-1.0,  2.0, -3.0));
      end;
   end Nominal;

   ----------
   --  Public routines

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_Vector");
   end Name;

   procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 0.000_1;
   end Set_Up_Case;

   procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Float.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Float_Vector;
