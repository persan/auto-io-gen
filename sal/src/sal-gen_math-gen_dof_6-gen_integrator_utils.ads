--  Abstract :
--
--  Generic Kinematic state integrator utilities.
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

pragma License (Modified_GPL);

generic
package SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils is
   pragma Elaborate_Body;

   type State_Type is record
      --  We don't give frames in names, because that depends on left
      --  or Wertz convention.
      --
      --  For Left, Rotation is IMPROVEME:
      --  For Wertz, Rotation is Base_Quat_Obj (GNC convention)

      Translation : Math_DOF_3.Cart_Vector_Type;
      Rotation    : Math_DOF_3.Unit_Quaternion_Type;
      --  Pose of center of mass frame relative to some base frame.
      --  Not a Pose_Type, because typical use may have the two parts
      --  in different frames.

      CM_Tran_Mom : Math_DOF_3.Cart_Vector_Type;
      --  Translation momentum of center of mass in Base frame.

      CM_Rot_Mom : Math_DOF_3.Cart_Vector_Type;
      --  Rotation momentum in center of mass frame (fixed to body).
   end record;

   type State_Dot_Type is record
      Tran_Dot : Math_DOF_3.Cart_Vector_Type;
      --  Translation velocity of center of mass in Base frame.

      Rot_Dot : Math_DOF_3.Cart_Vector_Type;
      --  Rotation velocity about center of mass in object frame.

      CM_Tran_Mom_Dot : Math_DOF_3.Cart_Vector_Type;
      --  Derivative of translation momentum in Base frame.

      CM_Rot_Mom_Dot : Math_DOF_3.Cart_Vector_Type;
      --  Derivative of rotation momentum in object frame.
   end record;

   function Derivative
     (State            : in State_Type;
      Inverse_Mass     : in CM_Inverse_Mass_Type;
      CM_Force         : in Math_DOF_3.Cart_Vector_Type;
      CM_Torque        : in Math_DOF_3.Cart_Vector_Type;
      CM_Rot_Mom_Child : in Math_DOF_3.Cart_Vector_Type)
     return State_Dot_Type;
   --  Return derivative of State, given external Force (at cm, in
   --  base frame) and Torque (about cm, in object frame) on object,
   --  Inverse Mass of object, and extra rotation momentum in attached
   --  objects. Includes Euler torque.

   function Derivative_Plus_Derivative (Left, Right : in State_Dot_Type) return State_Dot_Type;
   function "+" (Left, Right : in State_Dot_Type) return State_Dot_Type
     renames Derivative_Plus_Derivative;

   function Derivative_Times_Time (Derivative : in State_Dot_Type; Delta_Time : in Real_Type) return State_Dot_Type;
   function "*" (Derivative : in State_Dot_Type; Delta_Time : in Real_Type)
     return State_Dot_Type renames Derivative_Times_Time;

end SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils;
