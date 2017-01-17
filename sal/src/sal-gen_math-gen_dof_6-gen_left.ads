--  Abstract:
--
--  DOF_6 operations with left-multiply rotations.
--
--  References:
--
--  [1] Spacecraft Math, Stephen Leake
--
--  Copyright (C) 2001 - 2003, 2005 - 2008 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with SAL.Gen_Math.Gen_DOF_3.Gen_Left;
generic
   with package Math_DOF_3_Left is new Math_DOF_3.Gen_Left;
package SAL.Gen_Math.Gen_DOF_6.Gen_Left is
   pragma Pure;

   ----------
   --  Dual_Cart operations

   function "*"
     (Left  : in Math_DOF_3.Unit_Quaternion_Type;
      Right : in Dual_Cart_Vector_Type)
      return Dual_Cart_Vector_Type;
   --  Rotate Right. [1] 2.3.1-8, eqn:quat_times_vect applied to
   --  translation and rotation parts.

   function Inverse_Times
      (Left  : in Math_DOF_3.Unit_Quaternion_Type;
       Right : in Dual_Cart_Vector_Type)
      return Dual_Cart_Vector_Type;
   --  Same as Inverse (Left) * Right, but faster.

   ----------
   --  Pose operations

   function To_Dual_Cart_Vector (Pose : in Pose_Type) return Dual_Cart_Vector_Type;
   --  [1] 3.1.1.9 eqn:pose_to_dvc

   function To_Pose (Dual_Cart_Vector : in Dual_Cart_Vector_Type) return Pose_Type;
   --  [1] 3.1.1.10 eqn:dvc_to_pose

   function Inverse (Item : in Pose_Type) return Pose_Type;
   --  Return the inverse transform. [1] 3.1.1.7 eqn:transform_inverse_left

   function "*" (Left, Right : in Pose_Type) return Pose_Type;
   --  multiply poses. [1] 3.1.1.6 eqn:transform_mult_left

   function "*" (Left : in Math_DOF_3.Unit_Quaternion_Type; Right : in Pose_Type) return Pose_Type;
   --  Equivalent to (Zero_Cart_Vector, Left) * Right.

   function "*" (Left : in Pose_Type; Right : in Math_DOF_3.Unit_Quaternion_Type) return Pose_Type;
   --  Equivalent to Left * (Zero_Cart_Vector, Right).

   function "*" (Left : in Pose_Type; Right : in Math_DOF_3.Cart_Vector_Type) return Math_DOF_3.Cart_Vector_Type;
   --  Equivalent to Translation (Left * (Right,
   --  Zero_Unit_Quaternion)); transform a vector into a new frame.

   function Inverse_Times (Left, Right : in Pose_Type) return Pose_Type;
   --  Same as Inverse (Left) * Right, but faster. [1] 3.1.1-8 eqn:transform_inverse_mult_left

   function "-" (Left, Right : in Pose_Type) return Dual_Cart_Vector_Type;
   --  Equivalent to To_Dual_Cart_Vector (Inverse (Right) * Left); the
   --  difference between two poses. This is useful mainly when the
   --  poses are close together, so the difference is small.

   function "+" (Left : in Pose_Type; Right : in Dual_Cart_Vector_Type) return Pose_Type;
   --  Equivalent to Left * To_Pose (Right).

   function "+" (Left : in Pose_Type; Right : in Math_DOF_3.Cart_Vector_Type) return Pose_Type;
   --  Equivalent to Left * (Right, Zero_Unit_Quaternion).

   function "+" (Left : in Math_DOF_3.Cart_Vector_Type; Right : in Pose_Type) return Pose_Type;
   --  Equivalent to (Left, Zero_Unit_Quaternion) * Right.

   function "-" (Left : in Pose_Type; Right : in Dual_Cart_Vector_Type) return Pose_Type;
   --  Equivalent to Left * Inverse (To_Pose (Right)).

   function "+" (Left : in Dual_Cart_Vector_Type; Right : in Pose_Type) return Pose_Type;
   --  Equivalent to To_Pose (Left) * Right.

   ----------
   --  Dual_Mag_Axis operations

   function To_Dual_Mag_Axis (Pose : in Pose_Type) return Dual_Mag_Axis_Type;
   function To_Pose (Dual_Mag_Axis : in Dual_Mag_Axis_Type) return Pose_Type;

   ----------
   --  Wrench and rate transforms

   function To_Rate_Transform (Item : in Pose_Type) return Rate_Transform_Type;
   --  [1] 3.2.1-3 eqn:rate_transform_matrix

   function To_Wrench_Transform (Item : in Pose_Type) return Wrench_Transform_Type;

   function To_Rate_Transform
      (Translation : in Math_DOF_3.Cart_Vector_Type;
       Rotation    : in Math_DOF_3.Rot_Matrix_Type)
      return Rate_Transform_Type;
   --  [1] 3.2.1-3 eqn:rate_transform_matrix

   function To_Wrench_Transform
      (Translation : in Math_DOF_3.Cart_Vector_Type;
       Rotation    : in Math_DOF_3.Rot_Matrix_Type)
      return Wrench_Transform_Type;

   function To_DC_Array_DCV (Item : in Rate_Transform_Type) return DC_Array_DCV_Type;
   function To_DC_Array_DCV (Item : in Wrench_Transform_Type) return DC_Array_DCV_Type;
   --  For element access or multiplying by random DC_Array_DCV_Type.

   function "*" (Left, Right : in Rate_Transform_Type) return Rate_Transform_Type;
   function "*" (Left : in Rate_Transform_Type; Right : in DC_Array_DCV_Type) return DC_Array_DCV_Type;
   function "*" (Left : in DC_Array_DCV_Type; Right : in Rate_Transform_Type) return DC_Array_DCV_Type;
   --  [1] 3.2.1-4 eqn:mult_rate_transform

   function "*" (Left, Right : in Wrench_Transform_Type) return Wrench_Transform_Type;
   function "*" (Left : in Wrench_Transform_Type; Right : in DC_Array_DCV_Type) return DC_Array_DCV_Type;
   function "*" (Left : in DC_Array_DCV_Type; Right : in Wrench_Transform_Type) return DC_Array_DCV_Type;

   function "*" (Left : in Rate_Transform_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   --  [1] 3.2.1-2 Same as To_DC_Array_DCV (Left) * Right, but faster.

   function Transform_Rate (Xform : in Pose_Type; Rate : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   --  Same as To_Rate_Transform (Xform) * Rate, but faster. This
   --  should be used when Xform changes often, so it is not worth
   --  converting to a Rate_Transform_Type.

   function Transform_Rate
      (Rotation : in Math_DOF_3.Unit_Quaternion_Type;
       Rate     : in Dual_Cart_Vector_Type)
      return Dual_Cart_Vector_Type
      renames Inverse_Times;
   --  Same as Transform_Rate ((Zero_Cart_Vector, Rotation), Rate),
   --  but much faster.

   function "*" (Left : in Wrench_Transform_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   --  [1] 3.3.1-2

   function Transform_Wrench (Xform : in Pose_Type; Wrench : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   --  Same as To_Wrench_Transform (Xform) * Wrench, but faster. This
   --  should be used when Xform changes often, so it is not worth
   --  converting to a Wrench_Transform_Type.

   function Transform_Wrench
      (Rotation : in Math_DOF_3.Unit_Quaternion_Type;
       Wrench   : in Dual_Cart_Vector_Type)
      return Dual_Cart_Vector_Type
      renames Inverse_Times;
   --  Same as Transform_Wrench ((Zero_Cart_Vector, Rotation), Wrench),
   --  but much faster.

   ----------
   --  Mass properties

   function "*" (Current_T_New : in Pose_Type; Mass : in Mass_Type) return Mass_Type;
   --  Change frame of Mass. [1] eqn 3.4.1-4 eqn:mass_change_frame_left

   function Add
      (Left         : in Mass_Type;
       Right        : in Mass_Type;
       Left_T_Right : in Pose_Type)
      return Mass_Type;
   --  Attach object Right to Left, at pose Left_T_Right. Right must
   --  be in Left_T_Right frame, result is in Left frame.

   function Subtract
      (Left         : in Mass_Type;
       Right        : in Mass_Type;
       Left_T_Right : in Pose_Type)
      return Mass_Type;
   --  Detach object Right from Left, at pose Left_T_Right. Right must
   --  be in Left_T_Right frame, result is in Left frame.

end SAL.Gen_Math.Gen_DOF_6.Gen_Left;
