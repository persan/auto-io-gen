--  Abstract:
--
--  Math types and operations for left-multiply rotation operations.
--
--  References:
--
--  [1] Spacecraft Math, Stephen Leake
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
package SAL.Gen_Math.Gen_DOF_3.Gen_Left is
   pragma Pure;

   ----------
   --  Quaternions

   function Mag_Axis_To_Unit_Quaternion (Mag_Axis : in Mag_Axis_Type) return Unit_Quaternion_Type;
   function To_Unit_Quaternion (Mag_Axis : in Mag_Axis_Type) return Unit_Quaternion_Type
     renames Mag_Axis_To_Unit_Quaternion;
   --  Active A_Quat_1_2 = to_unit_quaternion (active A_Mag_Axis_1_2).
   --  [1] 2.3.1-3, eqn:angle_axis_to_quat_active

   function Unit_Quaternion_To_Mag_Axis (Quaternion : in Unit_Quaternion_Type) return Mag_Axis_Type;
   function To_Mag_Axis (Quaternion : in Unit_Quaternion_Type) return Mag_Axis_Type
     renames Unit_Quaternion_To_Mag_Axis;
   --  Active A_Rot_Vect_1_2 = to_rot_vector (active A_Quat_1_2).
   --  [1] 2.3.1-7, eqn:quat_to_angle_axis_active
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

   function To_Unit_Quaternion (Angle : in Real_Type; Axis : in Cart_Axis_Type)
      return Unit_Quaternion_Type;
   --  Equivalent to To_Unit_Quaterion (Mag_Axis_Type'(Angle, To_Unit_Vector (Axis)))

   function X_Axis (Quat : in Unit_Quaternion_Type) return Unit_Vector_Type;
   --  Equivalent to To_Rot_Matrix (Quat)(X).

   function Y_Axis (Quat : in Unit_Quaternion_Type) return Unit_Vector_Type;
   --  Equivalent to To_Rot_Matrix (Quat)(Y).

   function Z_Axis (Quat : in Unit_Quaternion_Type) return Unit_Vector_Type;
   --  Equivalent to To_Rot_Matrix (Quat)(Z).

   function "*" (Left : in Unit_Quaternion_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type;
   function "*" (Left : in Unit_Quaternion_Type; Right : in Unit_Vector_Type) return Unit_Vector_Type;
   --  [1] 2.3.1-8, eqn:quat_times_vect

   function Inverse_Times (Left : in Unit_Quaternion_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type;
   function Inverse_Times (Left : in Unit_Quaternion_Type; Right : in Unit_Vector_Type) return Unit_Vector_Type;
   --  Same as Inverse (Left) * Right but faster.

   function To_Unit_Quaternion (N_1, N_2 : in Unit_Vector_Type) return Unit_Quaternion_Type;
   --  Return an active quaternion that rotates N_1 to N_2.
   --  [1] 2.3.1-18, eqn:units_to_quat_left.

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
   --  [1] 2.4.1-4 eqn:quat_to_rot_mat

   function Rot_Matrix_To_Unit_Quaternion (Rot_Matrix : in Rot_Matrix_Type) return Unit_Quaternion_Type;
   function To_Unit_Quaternion (Rot_Matrix : in Rot_Matrix_Type) return Unit_Quaternion_Type
      renames Rot_Matrix_To_Unit_Quaternion;
   --  [1] 2.4.1-5 eqn:quat_to_rot_mat

   function Mag_Axis_To_Rot_Matrix (Mag_Axis : in Mag_Axis_Type) return Rot_Matrix_Type;
   function To_Rot_Matrix (Mag_Axis : in Mag_Axis_Type) return Rot_Matrix_Type renames Mag_Axis_To_Rot_Matrix;
   --  [1] 2.4.1-2 eqn:angle_axis_to_rot_mat

   function Rot_Matrix_To_Mag_Axis (Rot_Matrix : in Rot_Matrix_Type) return Mag_Axis_Type;
   function To_Mag_Axis (Rot_Matrix : in Rot_Matrix_Type) return Mag_Axis_Type renames Rot_Matrix_To_Mag_Axis;
   --  [1] 2.4.1-3 eqn:rot_mat_to_angle_axis
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

   function Unit_Quat_Times_Inertia  (Left : in Unit_Quaternion_Type; Right : in Inertia_Type) return Inertia_Type;
   function Rot_Matrix_Times_Inertia (Left : in Rot_Matrix_Type;      Right : in Inertia_Type) return Inertia_Type;
   function "*" (Left : in Unit_Quaternion_Type; Right : in Inertia_Type) return Inertia_Type
     renames Unit_Quat_Times_Inertia;
   function "*" (Left : in Rot_Matrix_Type;      Right : in Inertia_Type) return Inertia_Type
     renames Rot_Matrix_Times_Inertia;
   --  Change the frame of an inertia; R^ I R-1. [1] eqn 3.4.1-2 eqn:inertia_change_frame_left

end SAL.Gen_Math.Gen_DOF_3.Gen_Left;
