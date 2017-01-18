--  Abstract :
--
--  Debug for SAL.Gen_Math.Gen_Runge_Kutta_4th, using double precision
--  so round-off error is smaller than integration error (we hope).
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
with SAL.Math_Double.Text_IO; use SAL.Math_Double.Text_IO;
with SAL.Math_Double.Stats.Image;
procedure Debug_Math_Double_Runge_Kutta_4th
is
begin
   Put_Line ("Integrate a 1 DOF wheel position");
   declare
      use SAL.Math_Double;

      Torque       : constant Real_Type := 0.5; --  Newton-meters
      Inertia      : constant Real_Type := 4.0; --  kilogram-meters^2
      Acceleration : constant Real_Type := Torque / Inertia;
      Max_Momentum : constant Real_Type := 100.0;

      type State_Type is record
         Position : Real_Type := 0.0;
         Momentum : Real_Type := 0.0;
      end record;

      Initial_State : constant State_Type := (0.0, 0.0);

      function "-" (Left, Right : in State_Type) return State_Type
      is begin
         return (Left.Position - Right.Position, Left.Momentum - Right.Momentum);
      end "-";

      procedure Put (Item : in State_Type; Small : in Boolean)
      is begin
         if Small then
            Put (Item.Position, Fore => 3, Aft => 5, Exp => 3);
            Put (Item.Momentum, Fore => 3, Aft => 5, Exp => 3);
         else
            Put (Item.Position, Fore => 5, Aft => 2, Exp => 0);
            Put (Item.Momentum, Fore => 5, Aft => 2, Exp => 0);
         end if;
      end Put;

      type State_Stats_Type is record
         Position : SAL.Math_Double.Stats.Stats_Type;
         Momentum : SAL.Math_Double.Stats.Stats_Type;
      end record;

      procedure Accum (Stats : in out State_Stats_Type; Expected, Actual : in State_Type)
      is
         use SAL.Math_Double.Stats;
      begin
         Accumulate (Stats.Position, Expected.Position - Actual.Position);
         Accumulate (Stats.Momentum, Expected.Momentum - Actual.Momentum);
      end Accum;

      procedure Put (Item : in State_Stats_Type)
      is
         use SAL.Math_Double.Stats;
      begin
         Put (Image.Image (Display (Item.Position), Mean_Exp => 3, SD_Exp => 3));
         New_Line;
         Put (Image.Image (Display (Item.Momentum), Mean_Exp => 3, SD_Exp => 3));
         New_Line;
      end Put;

      function Exact_State (T : in Real_Type) return State_Type
      is begin
         --  We are exerting a constant torque, so we get a quadratic
         --  position as a function of time.
         return
           (Position => Initial_State.Position + Initial_State.Momentum / Inertia * T + 0.5 * Acceleration * T ** 2,
            Momentum => Initial_State.Momentum + Torque * T);
      end Exact_State;

      function Derivative (State : in State_Type) return State_Type
      is begin
         return
           (Position => State.Momentum / Inertia,
            Momentum => Torque);
      end Derivative;

      function Derivative_Times_Time
        (Derivative : in State_Type;
         Time_Step  : in Real_Type)
        return State_Type
      is begin
         return
           (Position => Derivative.Position * Time_Step,
            Momentum => Derivative.Momentum * Time_Step);
      end Derivative_Times_Time;

      function State_Plus_State (Left : in State_Type; Right : in State_Type) return State_Type
      is begin
         return (Left.Position + Right.Position, Left.Momentum + Right.Momentum);
      end State_Plus_State;

      procedure Integrator is new SAL.Math_Double.Gen_Runge_Kutta_4th_Left
        (State_Type                 => State_Type,
          Derivative_Type            => State_Type,
          Compute_Derivative         => Derivative,
          Derivative_Times_Time      => Derivative_Times_Time,
          Derivative_Plus_Derivative => State_Plus_State,
          State_Plus_Derivative      => State_Plus_State);

      procedure Run (Time_Step : in Real_Type)
      is
         T_Init       : constant Real_Type := 0.0;
         State        : State_Type         := Exact_State (T_Init);
         T            : Real_Type;
         Steps        : constant Natural   := Natural ((Max_Momentum / Inertia) / Time_Step);
         Report_Steps : constant Natural   := Steps / 10;
         Report_Count : Natural            := 0;
         Total_Error  : State_Stats_Type;
         Start_Error  : State_Stats_Type;
         End_Error    : State_Stats_Type;
         Exact        : State_Type;
      begin
         Put ("Time_Step => "); Put (Time_Step, Fore => 2, Aft => 4, Exp => 0); New_Line;
         Put ("Steps     => " & Natural'Image (Steps)); New_Line;

         Put_Line ("         state              error");
         for I in 0 .. Steps loop
            T     := T_Init + Real_Type (I) * Time_Step;
            Exact := Exact_State (T);

            Accum (Total_Error, Expected => Exact, Actual => State);
            if I < 10 then
               Accum (Start_Error, Expected => Exact, Actual => State);
            end if;

            if Steps - I < 10 then
               Accum (End_Error, Expected => Exact, Actual => State);
            end if;

            if Report_Count = Report_Steps then
               Report_Count := 0;
               Put (State, Small => False);
               Put (Exact_State (T) - State, Small => True);
               New_Line;
            else
               Report_Count := Report_Count + 1;
            end if;

            Integrator (State, Time_Step);
         end loop;
         Put_Line ("Total error => ");
         Put (Total_Error);
         Put_Line ("Start error => ");
         Put (Start_Error);
         Put_Line ("End error => ");
         Put (End_Error);
         New_Line (2);
      end Run;
   begin
      Run (1.0);
      Run (0.1);
      Run (0.01);
      Run (0.001);
   end;
end Debug_Math_Double_Runge_Kutta_4th;
