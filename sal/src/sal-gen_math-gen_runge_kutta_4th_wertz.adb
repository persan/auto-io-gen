--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2005 Stephen Leake.  All Rights Reserved.
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
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

procedure SAL.Gen_Math.Gen_Runge_Kutta_4th_Wertz (State : in out State_Type; Time_Step : in Real_Type)
is
   function "*" (Derivative : in Derivative_Type; Time_Step  : in Real_Type) return Derivative_Type
     renames Derivative_Times_Time;

   function "+" (Left, Right : in Derivative_Type) return Derivative_Type renames Derivative_Plus_Derivative;

   function "+" (Derivative : in Derivative_Type; State : in State_Type) return State_Type
     renames Derivative_Plus_State;

   K1 : constant Derivative_Type := Compute_Derivative (State);
   K2 : constant Derivative_Type := Compute_Derivative (K1 * (Time_Step / 2.0) + State);
   K3 : constant Derivative_Type := Compute_Derivative (K2 * (Time_Step / 2.0) + State);
   K4 : constant Derivative_Type := Compute_Derivative (K3 * Time_Step + State);
begin
   State :=
     (K4 * (Time_Step / 6.0) +
        K3 * (Time_Step / 3.0) +
        K2 * (Time_Step / 3.0) +
        K1 * (Time_Step / 6.0)) +
     State;

end SAL.Gen_Math.Gen_Runge_Kutta_4th_Wertz;
