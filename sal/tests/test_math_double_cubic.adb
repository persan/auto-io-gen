--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2005, 2006 Stephen Leake.  All Rights Reserved.
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
with SAL.Math_Double.Complex_Types;
with SAL.Math_Double.Cubic;
package body Test_Math_Double_Cubic is

   procedure Test_Cubic (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double.AUnit;
      use SAL.Math_Double.Complex_Types;
      use SAL.Math_Double.Cubic;

      Coef : Coefficients_Type := (0.0, 1.0, 2.0, 3.0);
      --  We vary coef (0), and solve for x

      function Compute (Root : in Complex) return Complex
      is begin
         return Coef (0) + Root * (Coef (1) + Root * (Coef (2) + Root * Coef (3)));
      end Compute;

      Root_1 : Complex;
      Root_2 : Complex;
      Root_3 : Complex;

      Result : Complex;
   begin
      --  Show that the roots are solutions to the cubic.

      for I in 0 .. 10 loop
         Coef (0) := SAL.Math_Double.Real_Type (I);

         Solve (Coef, Root_1, Root_2, Root_3);

         Result := Compute (Root_1);
         Check (Integer'Image (I) & ".Re", Result.Re, 0.0);
         Check (Integer'Image (I) & ".Im", Result.Im, 0.0);

         Result := Compute (Root_2);
         Check (Integer'Image (I) & ".Re", Result.Re, 0.0);
         Check (Integer'Image (I) & ".Im", Result.Im, 0.0);

         Result := Compute (Root_3);
         Check (Integer'Image (I) & ".Re", Result.Re, 0.0);
         Check (Integer'Image (I) & ".Im", Result.Im, 0.0);

      end loop;
   end Test_Cubic;

   ----------
   --  Public routines

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Cubic'Access, "Test_Cubic");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Double_Cubic");
   end Name;

   procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Double.AUnit.Default_Tolerance := 0.000_001;
   end Set_Up_Case;

   procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Double.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Double_Cubic;
