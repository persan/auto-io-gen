--  Abstract:
--
--  DOF_6 operations with right-multiply quaternions.
--
--  References:
--
--  [1] Spacecraft Math, Stephen Leake
--
--  Copyright (C) 2005 - 2009 Stephen Leake.  All Rights Reserved.
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

with SAL.Gen_Math.Gen_DOF_3.Gen_Wertz;
generic
   with package Math_DOF_3_Wertz is new Math_DOF_3.Gen_Wertz;
package SAL.Gen_Math.Gen_DOF_6.Gen_Wertz is
   pragma Pure;

   ----------
   --  Dual_Cart operations

   function "*"
     (Left  : in Dual_Cart_Vector_Type;
      Right : in Math_DOF_3.Unit_Quaternion_Type)
     return Dual_Cart_Vector_Type;
   --  Rotate Left. [1] 2.3.1-20, eqn:quat_times_vect_right applied to
   --  translation and rotation parts.

   function Times_Inverse
     (Left  : in Dual_Cart_Vector_Type;
      Right : in Math_DOF_3.Unit_Quaternion_Type)
     return Dual_Cart_Vector_Type;
   --  Same as Left * Inverse (Right), but faster.

   ----------
   --  Pose operations

   function To_Dual_Cart_Vector (Pose : in Pose_Type) return Dual_Cart_Vector_Type;
   --  [1] 3.1.1.9 eqn:pose_to_dvc

   function To_Pose (Dual_Cart_Vector : in Dual_Cart_Vector_Type) return Pose_Type;
   --  [1] 3.1.1.10 eqn:dvc_to_pose

   function Inverse (Item : in Pose_Type) return Pose_Type;
   --  Return the inverse transform. [1] 3.1.1.13 eqn:transform_inverse_right

   function "*" (Left, Right : in Pose_Type) return Pose_Type;
   --  multiply poses. [1] 3.1.1.12 eqn:transform_mult_right

   function "*" (Left : in Math_DOF_3.Unit_Quaternion_Type; Right : in Pose_Type) return Pose_Type;
   --  Equivalent to (Zero_Cart_Vector, Left) * Right.

   function "*" (Left : in Pose_Type; Right : in Math_DOF_3.Unit_Quaternion_Type) return Pose_Type;
   --  Equivalent to Left * (Zero_Cart_Vector, Right).

   function "*" (Left : in Math_DOF_3.Cart_Vector_Type; Right : in Pose_Type) return Math_DOF_3.Cart_Vector_Type;
   --  Equivalent to ((Left, Zero_Unit_Quaternion) *
   --  Right).Translation

   function Times_Inverse (Left, Right : in Pose_Type) return Pose_Type;
   --  Same as Left * Inverse (Right), but faster.

   --  function "-" (Left, Right : in Pose_Type) return Dual_Cart_Vector_Type;
   --  We don't define this function and related functions in this
   --  package; it is even less intuitive than for the Gen_Left
   --  package.

   ----------
   --  Dual_Mag_Axis operations

   function To_Dual_Mag_Axis (Pose : in Pose_Type) return Dual_Mag_Axis_Type;
   function To_Pose (Dual_Mag_Axis : in Dual_Mag_Axis_Type) return Pose_Type;

   ----------
   --  Left-multiply wrench and rate transforms

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
     (Rate     : in Dual_Cart_Vector_Type;
      Rotation : in Math_DOF_3.Unit_Quaternion_Type)
     return Dual_Cart_Vector_Type
     renames Times_Inverse;
   --  Same as Transform_Rate (Xform => (Zero_Cart_Vector, Rotation), Rate => Rate),
   --  but much faster.

   function "*" (Left : in Wrench_Transform_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   --  [1] 3.3.1-2

   function Transform_Wrench (Xform : in Pose_Type; Wrench : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   --  Same as To_Wrench_Transform (Xform) * Wrench, but faster. This
   --  should be used when Xform changes often, so it is not worth
   --  converting to a Wrench_Transform_Type.

   function Transform_Wrench
     (Wrench   : in Dual_Cart_Vector_Type;
      Rotation : in Math_DOF_3.Unit_Quaternion_Type)
     return Dual_Cart_Vector_Type
     renames Times_Inverse;
   --  Same as Transform_Wrench ((Zero_Cart_Vector, Rotation), Wrench),
   --  but much faster.

   ----------
   --  Mass properties

   function "*" (Mass : in Mass_Type; Current_Pose_New : in Pose_Type) return Mass_Type;
   --  Change frame of Mass. [1] eqn 3.4.1-8 eqn:mass_change_frame_right

   function Add
     (Base            : in Mass_Type;
      Child           : in Mass_Type;
      Child_Pose_Base : in Pose_Type)
     return Mass_Type;
   function Add
     (Base            : in Mass_Type;
      Child           : in Mass_Type;
      Child_Tran_Base : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type;
   function Add
     (Base            : in Mass_Type;
      Child           : in CM_Mass_Type;
      Child_Tran_Base : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type;
   --  Attach object Child to Base, at pose Child_Pose_Base. Versions
   --  with Child_Tran_Base are the same, but assume zero quaterion.

   function Subtract
     (Base            : in Mass_Type;
      Child           : in Mass_Type;
      Child_Pose_Base : in Pose_Type)
     return Mass_Type;
   function Subtract
     (Base            : in Mass_Type;
      Child           : in Mass_Type;
      Child_Tran_Base : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type;
   function Subtract
     (Base            : in Mass_Type;
      Child           : in CM_Mass_Type;
      Child_Tran_Base : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type;
   --  Detach object Child from Base, at pose Child_Pose_Base. Versions
   --  with Child_Tran_Base are the same, but assume zero quaterion.

end SAL.Gen_Math.Gen_DOF_6.Gen_Wertz;
