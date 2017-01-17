--  Abstract :
--
--  See spec
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
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

package body SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils.Gen_Left is

   function State_Plus_Derivative (State : in State_Type; Derivative : in State_Dot_Type) return State_Type
   is
      use DCV_Ops;
      use Math_DOF_6_Left;
      use Math_DOF_3;
      use Math_DOF_3_Left;
      use Math_DOF_3.Cart_Vector_Ops;
   begin
      --  Translation parts are in Base frame, Rotation parts are in
      --  center of mass frame. State rotation and derivative are
      --  active.
      --
      --  Base_Pose_CM_2 = Base_Pose_CM_1 * CM_1_Derivative_CM_2
      return
        (Translation => Derivative.Tran_Dot + State.Translation,
         Rotation    => State.Rotation * To_Unit_Quaternion (Derivative.Rot_Dot),
         CM_Tran_Mom => Derivative.CM_Tran_Mom_Dot + State.CM_Tran_Mom,
         CM_Rot_Mom  => Derivative.CM_Rot_Mom_Dot + State.CM_Rot_Mom);
   end State_Plus_Derivative;

end SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils.Gen_Left;
