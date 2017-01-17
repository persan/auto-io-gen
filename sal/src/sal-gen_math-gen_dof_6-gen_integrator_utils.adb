--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2003, 2005, 2006 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils is

   function Derivative
     (State            : in State_Type;
      Inverse_Mass     : in CM_Inverse_Mass_Type;
      CM_Force         : in Math_DOF_3.Cart_Vector_Type;
      CM_Torque        : in Math_DOF_3.Cart_Vector_Type;
      CM_Rot_Mom_Child : in Math_DOF_3.Cart_Vector_Type)
     return State_Dot_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, DCV_Ops;
      Tran_Dot : constant Cart_Vector_Type := Inverse_Mass_Times_Tran (Inverse_Mass, State.CM_Tran_Mom);
      Rot_Dot  : constant Cart_Vector_Type := Inverse_Mass_Times_Rot (Inverse_Mass, State.CM_Rot_Mom);

      Angular_Velocity : constant Math_DOF_3.Cart_Vector_Type := Rot_Dot;
      Angular_Momentum : constant Math_DOF_3.Cart_Vector_Type := State.CM_Rot_Mom + CM_Rot_Mom_Child;
      CM_Torque_Euler  : constant Math_DOF_3.Cart_Vector_Type := Cross (Angular_Velocity, Angular_Momentum);
   begin
      return
        (Tran_Dot        => Tran_Dot,
         Rot_Dot         => Rot_Dot,
         CM_Tran_Mom_Dot => CM_Force,
         CM_Rot_Mom_Dot  => CM_Torque - CM_Torque_Euler);
   end Derivative;

   function Derivative_Plus_Derivative (Left, Right : in State_Dot_Type) return State_Dot_Type
   is
      use Math_DOF_3.Cart_Vector_Ops;
   begin
      return
        (Tran_Dot        => Left.Tran_Dot + Right.Tran_Dot,
         Rot_Dot         => Left.Rot_Dot + Right.Rot_Dot,
         CM_Tran_Mom_Dot => Left.CM_Tran_Mom_Dot + Right.CM_Tran_Mom_Dot,
         CM_Rot_Mom_Dot  => Left.CM_Rot_Mom_Dot + Right.CM_Rot_Mom_Dot);
   end Derivative_Plus_Derivative;

   function Derivative_Times_Time (Derivative : in State_Dot_Type; Delta_Time : in Real_Type) return State_Dot_Type
   is
      use Math_DOF_3.Cart_Vector_Ops;
   begin
      return
        (Tran_Dot        => Delta_Time * Derivative.Tran_Dot,
         Rot_Dot         => Delta_Time * Derivative.Rot_Dot,
         CM_Tran_Mom_Dot => Delta_Time * Derivative.CM_Tran_Mom_Dot,
         CM_Rot_Mom_Dot  => Delta_Time * Derivative.CM_Rot_Mom_Dot);
   end Derivative_Times_Time;

end SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils;
