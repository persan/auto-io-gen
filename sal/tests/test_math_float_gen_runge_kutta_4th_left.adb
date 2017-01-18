--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2005 Stephen Leake.  All Rights Reserved.
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

with SAL.Math_Float.Elementary;
with SAL.Gen_Math.Gen_Runge_Kutta_4th_Left;
package body Test_Math_Float_Gen_Runge_Kutta_4th_Left is

   Default_Threshold : constant := 10.0e-5;

   procedure Check
     (Message            : in String;
      Computed, Expected : in SAL.Math_Float.Real_Type;
      Threshold          : in SAL.Math_Float.Real_Type := Default_Threshold)
   is
      use SAL.Math_Float;
   begin
      AUnit.Assertions.Assert
        (abs (Computed - Expected) < Threshold,
         Message &
           " failed; expected " & Real_Type'Image (Expected) &
           " got " & Real_Type'Image (Computed));
   end Check;

   ----------
   --  Public routines

   function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_Gen_Runge_Kutta_4th_Left");
   end Name;

   procedure Integrate_Constant (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.Math_Float;

      function Exact_F (Time : in Real_Type) return Real_Type
      is
         pragma Unreferenced (Time);
      begin
         return 1.0;
      end Exact_F;

      function State_Derivative (State : in Real_Type) return Real_Type
      is
         pragma Unreferenced (State);
      begin
         return 0.0;
      end State_Derivative;

      procedure Constant_Integrator is new SAL.Math_Float.Gen_Runge_Kutta_4th_Left
         (State_Type                 => Real_Type,
          Derivative_Type            => Real_Type,
          Compute_Derivative         => State_Derivative,
          Derivative_Times_Time      => "*",
          Derivative_Plus_Derivative => "+",
          State_Plus_Derivative      => "+");

      Time_Init : constant Real_Type := 0.0;
      Time_Step : constant Real_Type := 0.1;
      State     : Real_Type          := Exact_F (Time_Init);
      Time      : Real_Type;
   begin
      for I in 0 .. 10 loop
         Time := Time_Init + Real_Type (I) * Time_Step;

         Check ("I" & Integer'Image (I), State, Exact_F (Time));

         Constant_Integrator (State, Time_Step);
      end loop;

   end Integrate_Constant;

   procedure Integrate_Linear (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.Math_Float;

      function Exact_F (Time : in Real_Type) return Real_Type
      is begin
         return Time;
      end Exact_F;

      function State_Derivative (State : in Real_Type) return Real_Type
      is
         pragma Unreferenced (State);
      begin
         return 1.0;
      end State_Derivative;

      procedure Linear_Integrator is new SAL.Math_Float.Gen_Runge_Kutta_4th_Left
         (State_Type                 => Real_Type,
          Derivative_Type            => Real_Type,
          Compute_Derivative         => State_Derivative,
          Derivative_Times_Time      => "*",
          Derivative_Plus_Derivative => "+",
          State_Plus_Derivative      => "+");

      Time_Init : constant Real_Type := 0.0;
      Time_Step : constant Real_Type := 0.1;
      State     : Real_Type          := Exact_F (Time_Init);
      Time      : Real_Type;
   begin
      for I in 0 .. 10 loop
         Time := Time_Init + Real_Type (I) * Time_Step;

         Check
           (Message   => "I" & Integer'Image (I),
            Computed  => State,
            Expected  => Exact_F (Time),
            Threshold => Time_Step * 0.1);

         Linear_Integrator (State, Time_Step);
      end loop;

   end Integrate_Linear;

   procedure Integrate_Quadratic (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.Math_Float;

      function Exact_F (Time : in Real_Type) return Real_Type
      is begin
         return Time ** 2;
      end Exact_F;

      function State_Derivative (State : in Real_Type) return Real_Type
      is
         Time : constant Real_Type := SAL.Math_Float.Elementary.Sqrt (State);
      begin
         return 2.0 * Time;
      end State_Derivative;

      procedure Quadratic_Integrator is new SAL.Math_Float.Gen_Runge_Kutta_4th_Left
         (State_Type                 => Real_Type,
          Derivative_Type            => Real_Type,
          Compute_Derivative         => State_Derivative,
          Derivative_Times_Time      => "*",
          Derivative_Plus_Derivative => "+",
          State_Plus_Derivative      => "+");

      --  If start at T_Init = 0.0, Derivative and State are always 0.0
      Time_Init : constant Real_Type := 0.1;
      Time_Step : constant Real_Type := 0.1;
      State     : Real_Type          := Exact_F (Time_Init);
      Time      : Real_Type;
   begin
      for I in 0 .. 10 loop
         Time := Time_Init + Real_Type (I) * Time_Step;

         Check
           (Message   => "I" & Integer'Image (I),
            Computed  => State,
            Expected  => Exact_F (Time),
            Threshold => Time_Step * 0.1);

         Quadratic_Integrator (State, Time_Step);
      end loop;

   end Integrate_Quadratic;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Integrate_Constant'Access, "Integrate Constant");
      Register_Routine (T, Integrate_Linear'Access, "Integrate Linear");
      Register_Routine (T, Integrate_Quadratic'Access, "Integrate Quadratic");
   end Register_Tests;

end Test_Math_Float_Gen_Runge_Kutta_4th_Left;
