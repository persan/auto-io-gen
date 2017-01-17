--  Abstract:
--
--  Math types and operations for right-multiply rotation quaternions
--  and left-multiply rotation matrix operations, matching [2].
--
--  References:
--
--  [1] Spacecraft Math, Stephen Leake
--
--  [2] James R. Wertz, ed., Spacecraft Attitude Determination and
--      Control, 1978.
--
--  Design:
--
--  All rotation units are radians, all translation units are up to
--  the user (meters are recommended).
--
--  Copyright (C) 2005 - 2008 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

generic
package SAL.Gen_Math.Gen_DOF_3.Gen_Wertz is
   pragma Pure;

   ----------
   --  Quaternions

   function Mag_Axis_To_Unit_Quaternion (Mag_Axis : in Mag_Axis_Type) return Unit_Quaternion_Type;
   function To_Unit_Quaternion (Mag_Axis : in Mag_Axis_Type) return Unit_Quaternion_Type
     renames Mag_Axis_To_Unit_Quaternion;
   --  Active 1_2_Quat_A = to_unit_quaternion (active 1_2_Mag_Axis_A).
   --  [1] 2.3.1-18, eqn:angle_axis_to_quat_active_right

   function Unit_Quaternion_To_Mag_Axis (Quaternion : in Unit_Quaternion_Type) return Mag_Axis_Type;
   function To_Mag_Axis (Quaternion : in Unit_Quaternion_Type) return Mag_Axis_Type
     renames Unit_Quaternion_To_Mag_Axis;
   --  Active 1_2_Mag_Axis_A = To_Mag_Axis (active 1_2_Quat_A).
   --  [1] 2.3.1-25, eqn:quat_to_angle_axis_right
   --
   --  Result.Mag will be in range 0.0 .. PI.

   function Rot_Vector_To_Unit_Quaternion (Rot_Vector : in Cart_Vector_Type) return Unit_Quaternion_Type;
   function To_Unit_Quaternion (Rot_Vector : in Cart_Vector_Type) return Unit_Quaternion_Type
      renames Rot_Vector_To_Unit_Quaternion;
   --  [1] 2.5.1-3, eqn:rotvect_to_quat
   --  Equivalent to To_Unit_Quaternion (To_Mag_Axis (Rot_Vector)).

   function Unit_Quaternion_To_Rot_Vector (Quaternion : in Unit_Quaternion_Type) return Cart_Vector_Type;
   function To_Rot_Vector (Quaternion : in Unit_Quaternion_Type) return Cart_Vector_Type
      renames Unit_Quaternion_To_Rot_Vector;
   --  [1] 2.5.1-2, eqn:quat_to_rotvect
   --  Equivalent to To_Cart_Vector (To_Mag_Axis (Quaternion));

   function Light_Vector_Rotation
     (Target_Unit_Inertial : in Unit_Vector_Type;
      Sc_Vel_Inertial      : in Cart_Vector_Type)
     return Unit_Quaternion_Type;
   --  [1] 4.1.1-4, eqn:light_vector_aberration_simple
   --
   --  Return the active unit quaternion Inertial_Ab_Quat_Inertial
   --  that gives the velocity aberration rotation.

   function To_Unit_Quaternion (Angle : in Real_Type; Axis : in Cart_Axis_Type) return Unit_Quaternion_Type;
   --  Equivalent to To_Unit_Quaternion (Mag_Axis_Type'(Angle, To_Unit_Vector (Axis)))

   function X_Axis (Quat : in Unit_Quaternion_Type) return Unit_Vector_Type;
   --  Equivalent to To_Rot_Matrix (Quat)(X).

   function Y_Axis (Quat : in Unit_Quaternion_Type) return Unit_Vector_Type;
   --  Equivalent to To_Rot_Matrix (Quat)(Y).

   function Z_Axis (Quat : in Unit_Quaternion_Type) return Unit_Vector_Type;
   --  Equivalent to To_Rot_Matrix (Quat)(Z).

   function "*" (Left : in Cart_Vector_Type; Right : in Unit_Quaternion_Type) return Cart_Vector_Type;
   function "*" (Left : in Unit_Vector_Type; Right : in Unit_Quaternion_Type) return Unit_Vector_Type;
   --  [1] 2.3.1-20, eqn:quat_times_vect_right

   function Times_Inverse (Left : in Cart_Vector_Type; Right : in Unit_Quaternion_Type) return Cart_Vector_Type;
   function Times_Inverse (Left : in Unit_Vector_Type; Right : in Unit_Quaternion_Type) return Unit_Vector_Type;
   --  Same as Left * Inverse (Right) but faster.

   function To_Unit_Quaternion (N_1, N_2 : in Unit_Vector_Type) return Unit_Quaternion_Type;
   --  Return an active quaternion that rotates N_1 to N_2.
   --  [1] 2.3.1-30, eqn:units_to_quat_right.

   function To_ZYX_Euler (Quaternion : in Unit_Quaternion_Type) return ZYX_Euler_Type;
   --  Convert a unit quaternion to ZYX Euler angles. When at the
   --  singularity, Theta_Z is set to 0.0.
   --
   --  IMPROVEME: document in [1]

   function To_Unit_Quaternion (Euler : in ZYX_Euler_Type) return Unit_Quaternion_Type;
   --  Convert ZYX Euler angles to a unit quaternion.
   --
   --  IMPROVEME: document in [1]

   ----------
   --  Rotation matrices

   function Unit_Quaternion_To_Rot_Matrix (Quaternion : in Unit_Quaternion_Type) return Rot_Matrix_Type;
   function To_Rot_Matrix (Quaternion : in Unit_Quaternion_Type) return Rot_Matrix_Type
     renames Unit_Quaternion_To_Rot_Matrix;
   --  [1] 2.4.1-4 eqn:quat_to_rot_mat, modified for right-multiply quaternion
   --  Left-multiply A_Rot_Mat_B = To_Rot_Matrix (right-multiply B_Quat_A)
   --
   --  Both are either active or passive

   function Rot_Matrix_To_Unit_Quaternion (Rot_Matrix : in Rot_Matrix_Type) return Unit_Quaternion_Type;
   function To_Unit_Quaternion (Rot_Matrix : in Rot_Matrix_Type) return Unit_Quaternion_Type
      renames Rot_Matrix_To_Unit_Quaternion;
   --  [1] 2.4.1-5 eqn:quat_to_rot_mat, modified for right-multiply quaternion
   --  right-multiply B_Quat_A = To_Unit_Quaternion (left-multiply A_Rot_Mat_B)
   --
   --  Both are either active or passive

   function Mag_Axis_To_Rot_Matrix (Mag_Axis : in Mag_Axis_Type) return Rot_Matrix_Type;
   function To_Rot_Matrix (Mag_Axis : in Mag_Axis_Type) return Rot_Matrix_Type renames Mag_Axis_To_Rot_Matrix;
   --  [1] 2.4.1-2 eqn:angle_axis_to_rot_mat
   --  Active left-multiply A_Rot_Matrix_1_2 = To_Rot_Matrix (active 1_2_Rot_A)

   function Rot_Matrix_To_Mag_Axis (Rot_Matrix : in Rot_Matrix_Type) return Mag_Axis_Type;
   function To_Mag_Axis (Rot_Matrix : in Rot_Matrix_Type) return Mag_Axis_Type renames Rot_Matrix_To_Mag_Axis;
   --  [1] 2.4.1-3 eqn:rot_mat_to_angle_axis
   --  active 1_2_Rot_A = To_Mag_Axis (active left-multiply A_Rot_Matrix_1_2)
   --
   --  Result.Mag will be in range 0.0 .. PI.

   function Rot_Matrix_Times_Cart_Vector
      (Left  : in Rot_Matrix_Type;
       Right : in Cart_Vector_Type)
      return Cart_Vector_Type;
   function "*" (Left : in Rot_Matrix_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type
     renames Rot_Matrix_Times_Cart_Vector;
   --  [1] 2.4.1-6 eqn:rot_mat_times_vect

   pragma Inline (Rot_Matrix_Times_Cart_Vector);

   function Inverse_Times (Left : in Rot_Matrix_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type;
   --  Same as Inverse (Left) * Right but faster.
   pragma Inline (Inverse_Times);

   ----------
   --  Inertia

   function Inertia_Times_Unit_Quat  (Left : in Inertia_Type;    Right : in Unit_Quaternion_Type) return Inertia_Type;
   function Rot_Matrix_Times_Inertia (Left : in Rot_Matrix_Type; Right : in Inertia_Type) return Inertia_Type;
   function "*" (Left : in Inertia_Type;    Right : in Unit_Quaternion_Type) return Inertia_Type
     renames Inertia_Times_Unit_Quat;
   function "*" (Left : in Rot_Matrix_Type; Right : in Inertia_Type) return Inertia_Type
     renames Rot_Matrix_Times_Inertia;
   --  Change the frame of an inertia; R^-1 I R. [1] eqn 3.4.1-7 eqn:inertia_change_frame_right

end SAL.Gen_Math.Gen_DOF_3.Gen_Wertz;
