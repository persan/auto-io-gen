--  Abstract :
--
--  Test SAL.Gen_Math.Gen_Runge_Kutta_4th
--
--  Copyright (C) 2002, 2003, 2005 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen_Math.Gen_Runge_Kutta_4th_Left;
with SAL.Math_Float.Text_IO; use SAL.Math_Float.Text_IO;
with SAL.Math_Float.Elementary;
procedure Debug_Math_Float_Gen_Runge_Kutta_4th
is
begin

   Put_Line ("Integrate a constant function");
   declare
      use SAL.Math_Float;

      function Exact_F (T : in Real_Type) return Real_Type
      is
         pragma Unreferenced (T);
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

      T_Init    : constant Real_Type := 0.0;
      Time_Step : constant Real_Type := 0.1;
      State     : Real_Type          := Exact_F (T_Init);
      T         : Real_Type;
   begin
      Put_Line (" T    Exact_F(T)  RK_F(T)");
      for I in 0 .. 10 loop
         T := T_Init + Real_Type (I) * Time_Step;

         Put (T, Fore => 2, Aft => 1, Exp => 0);
         Put (" ");
         Put (Exact_F (T), Fore => 5, Aft => 2, Exp => 0);
         Put (" ");
         Put (State, Fore => 5, Aft => 2, Exp => 0);
         New_Line;

         Constant_Integrator (State, Time_Step);
      end loop;
   end;

   ----------
   New_Line;
   Put_Line ("Integrate a linear function");
   declare
      use SAL.Math_Float;

      function Exact_F (T : in Real_Type) return Real_Type
      is begin
         return T;
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

      T_Init    : constant Real_Type := 0.0;
      Time_Step : constant Real_Type := 0.1;
      State     : Real_Type          := Exact_F (T_Init);
      T         : Real_Type;
   begin
      Put_Line (" T    Exact_F(T)  RK_F(T)");
      for I in 0 .. 10 loop
         T := T_Init + Real_Type (I) * Time_Step;

         Put (T, Fore => 2, Aft => 1, Exp => 0);
         Put (" ");
         Put (Exact_F (T), Fore => 5, Aft => 5, Exp => 0);
         Put (" ");
         Put (State, Fore => 5, Aft => 5, Exp => 0);
         New_Line;

         Linear_Integrator (State, Time_Step);
      end loop;
   end;

   ----------
   New_Line;
   Put_Line ("Integrate a quadratic function");
   declare
      use SAL.Math_Float;

      function Exact_F (T : in Real_Type) return Real_Type
      is begin
         return T ** 2;
      end Exact_F;

      function State_Derivative (State : in Real_Type) return Real_Type
      is
         T : constant Real_Type := SAL.Math_Float.Elementary.Sqrt (State);
      begin
         return 2.0 * T;
      end State_Derivative;

      procedure Quadratic_Integrator is new SAL.Math_Float.Gen_Runge_Kutta_4th_Left
         (State_Type                 => Real_Type,
          Derivative_Type            => Real_Type,
          Compute_Derivative         => State_Derivative,
          Derivative_Times_Time      => "*",
          Derivative_Plus_Derivative => "+",
          State_Plus_Derivative      => "+");

      function Exact_Derivative (T : in Real_Type) return Real_Type
      is begin
         return 2.0 * T;
      end Exact_Derivative;

      --  If start at T_Init = 0.0, Derivative and State are always 0.0
      --  If start at T_Init = 0.1, errors are pretty big
      --  expected error is O (time_step * derivative)?
      T_Init    : constant Real_Type := 0.2;
      Time_Step : constant Real_Type := 0.1;
      State     : Real_Type          := Exact_F (T_Init);
      T         : Real_Type;
   begin
      Put_Line (" T      deriv  Exact_F(T)   RK(T)");
      for I in 0 .. 10 loop
         T := T_Init + Real_Type (I) * Time_Step;

         Put (T, Fore => 2, Aft => 1, Exp => 0);
         Put (" ");
         Put (Exact_Derivative (T), Fore => 5, Aft => 2, Exp => 0);
         Put (" ");
         Put (Exact_F (T), Fore => 5, Aft => 5, Exp => 0);
         Put (" ");
         Put (State, Fore => 5, Aft => 5, Exp => 0);
         New_Line;

         Quadratic_Integrator (State, Time_Step);
      end loop;
   end;
end Debug_Math_Float_Gen_Runge_Kutta_4th;
