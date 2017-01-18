--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
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
with SAL.Math_Float.Linear_Fit;
package body Test_Math_Float_Linear_Fit is

   use SAL.Math_Float;

   Default_M_Threshold : constant Real_Type := 1.0e-6;
   Default_B_Threshold : constant Real_Type := 1.0e-6;

   procedure Check
     (Message     : in String;
      Computed    : in Linear_Fit.Data_Type;
      Expected_M  : in Real_Type;
      Expected_B  : in Real_Type;
      M_Threshold : in SAL.Math_Float.Real_Type := Default_M_Threshold;
      B_Threshold : in SAL.Math_Float.Real_Type := Default_B_Threshold)
   is
      use SAL.Math_Float.AUnit;
      Computed_M : Real_Type;
      Computed_B : Real_Type;
   begin
      Linear_Fit.Fit (Computed, M => Computed_M, B => Computed_B);
      Check (Message & ".M", Computed_M, Expected_M, M_Threshold);
      Check (Message & ".B", Computed_B, Expected_B, B_Threshold);
   end Check;

   procedure Test_Linear_Fit (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Linear_Fit;
      Data : Data_Type;
   begin
      for I in 1 .. 10 loop
         Accumulate (Data, X => Real_Type (I), Y => 2.0 * Real_Type (I));
      end loop;
      Check ("case_one", Data, Expected_M => 2.0, Expected_B => 0.0);

      Reset (Data);

      for I in 1 .. 10 loop
         Accumulate (Data, X => Real_Type (I), Y => -3.5 - 4.0 * Real_Type (I));
      end loop;
      Check ("case_two", Data, Expected_M => -4.0, Expected_B => -3.5);

   end Test_Linear_Fit;

   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test Math_Float.Linear_Fit");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Linear_Fit'Access, "Test_Linear_Fit");
   end Register_Tests;

end Test_Math_Float_Linear_Fit;
