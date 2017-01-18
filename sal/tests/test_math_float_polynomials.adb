--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2008 Stephen Leake.  All Rights Reserved.
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
with SAL.Math_Float.Polynomials; use SAL.Math_Float.Polynomials;
package body Test_Math_Float_Polynomials is

   use SAL.Math_Float;

   Threshold : constant := 10.0e-5;

   procedure Check
     (Message   : in String;
      Computed  : in Real_Type;
      Expected  : in Real_Type)
   is begin
      AUnit.Assertions.Assert
        (abs (Computed - Expected) < Threshold,
         Message &
           " failed; expected " & Real_Type'Image (Expected) &
           " got " & Real_Type'Image (Computed));
   end Check;

   procedure Test_First_Order (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      First_Order : constant Coefficients_Type := (3.0, 4.0);
   begin
      Check ("1", Compute (0.0, First_Order),  3.00);
      Check ("2", Compute (1.0, First_Order),  7.00);
      Check ("3", Compute (2.5, First_Order), 13.00);
   end Test_First_Order;

   procedure Test_Second_Order (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Second_Order : constant Coefficients_Type := (2.0, 3.0, 4.0);
   begin
      Check ("1", Compute (0.0, Second_Order),  2.00);
      Check ("2", Compute (1.0, Second_Order),  9.00);
      Check ("3", Compute (2.5, Second_Order), 34.50);
   end Test_Second_Order;

   procedure Test_Short_Order_Coefficients (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Short_Order_4 : constant Coefficients_Type := (1.0, 2.0, 3.0, 4.0, 5.0);
      Short_Order_2_4 : constant Coefficients_Type (2 .. 4) := (2.0, 3.0, 4.0);
   begin
      Check ("1a", Compute (0.0, Short_Order_4 (2 .. 4)),  0.00);
      Check ("1b", Compute (1.0, Short_Order_4 (2 .. 4)),  12.00);  --  3 +  4 +  5
      Check ("1c", Compute (2.0, Short_Order_4 (2 .. 4)),  124.00); -- 12 + 32 + 80
      Check ("2a", Compute (0.0, Short_Order_2_4),  0.00);
      Check ("2b", Compute (1.0, Short_Order_2_4),  9.00);
      Check ("2c", Compute (2.0, Short_Order_2_4),  96.00);         --  8 + 24 + 64
   end Test_Short_Order_Coefficients;
   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test Math_Float.Polynomial");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_First_Order'Access, "Test_First_Order");
      Register_Routine (T, Test_Second_Order'Access, "Test_Second_Order");
      Register_Routine (T, Test_Short_Order_Coefficients'Access, "Test_Short_Order");
   end Register_Tests;

end Test_Math_Float_Polynomials;
