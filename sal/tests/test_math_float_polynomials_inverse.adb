--  Test of Polynomial Inverse - Including Newton-Raphson technique

with AUnit.Assertions;                   use AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with SAL.Math_Float.Polynomials.Inverse; use SAL.Math_Float.Polynomials.Inverse;
with SAL.Math_Float.Polynomials.AUnit;   use SAL.Math_Float.Polynomials.AUnit;
with SAL.Math_Float.AUnit;               use SAL.Math_Float.AUnit;
package body Test_Math_Float_Polynomials_Inverse is

   use SAL.Math_Float;
   use SAL.Math_Float.Polynomials;

   --  Note: N-R can fail if
   --  (1) there are no real roots or
   --  (2) the guess is not sufficiently close to the desired root and
   --  (3) the surrounding polynomial hides the root. (i.e. derivative of a guess zero)

   --  Need package-level variable to defeat optimizer
   Junk : SAL.Math_Float.Real_Type;

   procedure Test_Derivative (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Check ("1", Derivative ((0 => 3.0, 1 => 4.0)), (0 => 4.0));
      Check ("2", Derivative ((0 => 3.0, 1 => 4.0, 2 => 5.0)), (0 => 4.0, 1 => 10.0));
      Check ("3", Derivative ((0 => 0.0, 1 => 0.0, 2 => 5.0)), (0 => 0.0, 1 => 10.0));
      Check ("4", Derivative ((2 => 5.0)), (1 => 10.0));
   end Test_Derivative;

   procedure Test_First_Order (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      First_Order_1 : constant Coefficients_Type := (3.0, 4.0); -- Positive Slope
      First_Order_2 : constant Coefficients_Type := (2.0, -5.0); -- Negative Slope
      First_Order_3 : constant Coefficients_Type := (3.0, 0.0); -- zero slope, no solution; will throw exception

   begin
      --  Inverse with a real-type yield is the first found real inverse (no guarantees)
      --     Case                 ( Y   Function(X)   guess, precision, iterations),  expected X)
      Check ("1a", Compute_Inverse (0.0, First_Order_1, 1.0, 10.0e-5, 100),  -0.75);
      Check ("1b", Compute_Inverse (7.0, First_Order_1, 1.0, 10.0e-5, 100),   1.0);
      Check ("1c", Compute_Inverse (13.0, First_Order_1, 1.0, 10.0e-5, 100),   2.5);

      Check ("2a", Compute_Inverse (2.0, First_Order_2, 1.0, 10.0e-5, 100),  0.0);
      Check ("2b", Compute_Inverse (-3.0, First_Order_2, 1.0, 10.0e-5, 100),  1.0);
      Check ("2c", Compute_Inverse (12.0, First_Order_2, 1.0, 10.0e-5, 100), -2.0);

      begin
         Junk := Compute_Inverse (0.0, First_Order_3, 0.0, 1.0e-2, 50);
         Assert (False, "Oops, Error NOT raised");
      exception
      when Constraint_Error =>
         null;
      end;
   end Test_First_Order;

   procedure Test_Second_Order (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Second_Order_1 : constant Coefficients_Type := (-16.0, 0.0, 4.0);
      Second_Order_2 : constant Coefficients_Type := (-16.0, 2.0, 1.0);
      Second_Order_3a : constant Coefficients_Type := (0.0, 0.0, 3.0);    -- 3 x^2
      Second_Order_3b : constant Coefficients_Type (2 .. 2) := (2 => 3.0);    -- 3 x^2
   begin
      --  Inverse with a real-type yield is the first found real inverse (no guarantees)
      --     Case                     ( Y   Function(X)   guess, precision, iterations),  expected X)
      Check ("C2.1", Compute_Inverse (0.0, Second_Order_1, 1.0, 10.0e-5, 100),  2.00);
      Check ("C2.2", Compute_Inverse (19.0, Second_Order_2, 1.0, 10.0e-5, 100),  5.00);

      --  no root; iteration limit
      begin
         Junk := Compute_Inverse (-20.0, Second_Order_2, 0.0, 10.0e-5, 100);
         Assert (False, "Oops, Error NOT raised on no root - iteration limit");
      exception
      when SAL.Range_Error =>
         null;
      end;

      --  This argument has no root; it encounters a zero derivative
      begin
         Junk := Compute_Inverse (-20.0, Second_Order_1, 0.0, 10.0e-5, 100);
         Assert (False, "Oops, no error raised on no root - Zero Derivative");
      exception
      when Constraint_Error =>
         null;
      end;

      --  Test coefficient index handling
      Check ("3a", Compute_Inverse (12.0, Second_Order_3a, 1.0, 10.0e-5, 100),  2.0);

      Check ("3b", Compute_Inverse (12.0, Second_Order_3b, 1.0, 10.0e-5, 100),  2.0);

   end Test_Second_Order;

   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test Math_Float.Polynomials_Inverse");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Derivative'Access, "Test_Derivative");
      Register_Routine (T, Test_First_Order'Access, "Test_First_Order");
      Register_Routine (T, Test_Second_Order'Access, "Test_Second_Order");
   end Register_Tests;

end Test_Math_Float_Polynomials_Inverse;

