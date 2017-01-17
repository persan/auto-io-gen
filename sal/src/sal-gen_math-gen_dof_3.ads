--  Abstract:
--
--  Math types and operations for 3 Cartesian degrees of freedom. One
--  degree of freedom types and operations are in Gen_Math.Gen_Scalar,
--  and 6 Cartesian degrees of freedom types and operations are in
--  Gen_Math.Gen_DOF_6.
--
--  See SAL.Gen_Math.Gen_DOF_3.Left for left-multiplying rotations,
--  .Wertz for right-multiplying rotations.
--
--  References:
--
--  [1] Spacecraft Math, Stephen Leake
--  [Jackson] Classical Electrodynamics, J. D. Jackson
--
--  Design:
--
--  By the standard naming convention, Cart_Vector_Type should be
--  named Cart_Array_Real_Type. We use Cart_Vector_Type in recognition
--  of the overwhelming influence of Cartesian geometry.
--
--  All rotation units are radians, all translation units are up to
--  the user (meters are recommended).
--
--  ZYX_Euler_Type is supported only so it can be converted to and from
--  Unit_Quaternion_Type. ZYX_Euler_Type may be useful for display
--  during debugging, or for interfacing to other systems that haven't
--  learned about quaternions yet.
--
--  Cross function is not "*" because "*" (Cart_Vector_Type,
--  Cart_Vector_Type) return Cart_Vector_Type is element by element
--  which is used more often. The other Cross functions are not "*"
--  for consistency.
--
--  Copyright (C) 2001 - 2008 Stephen Leake.  All Rights Reserved.
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

with Ada.Numerics.Generic_Elementary_Functions;
with SAL.Gen_Math.Gen_Scalar;
with SAL.Gen_Math.Gen_Vector;
with SAL.Gen_Math.Gen_Square_Array;
generic
   --  Auto_Io_Gen : ignore
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
   with package Math_Scalar is new SAL.Gen_Math.Gen_Scalar (Elementary);
package SAL.Gen_Math.Gen_DOF_3 is
   pragma Pure;

   type Cart_Axis_Type is (X, Y, Z);

   type Cart_Array_Boolean_Type is array (Cart_Axis_Type) of Boolean;
   type Cart_Vector_Type is array (Cart_Axis_Type) of Real_Type;
   type Cart_Array_Limit_Type is array (Cart_Axis_Type) of Math_Scalar.Limit_Type;

   Zero_Cart_Vector : constant Cart_Vector_Type := (0.0, 0.0, 0.0);

   package Cart_Vector_Ops is new Gen_Math.Gen_Vector
      (Elementary               => Elementary,
       Math_Scalar              => Math_Scalar,
       Index_Type               => Cart_Axis_Type,
       Index_Array_Boolean_Type => Cart_Array_Boolean_Type,
       Index_Array_Real_Type    => Cart_Vector_Type,
       Index_Array_Limit_Type   => Cart_Array_Limit_Type);

   function Mag (Item : in Cart_Vector_Type) return Real_Type;
   --  The Euclidean magnitude of a vector

   function Cross (Left, Right : in Cart_Vector_Type) return Cart_Vector_Type;

   Speed_Of_Light : constant Real_Type := 299_792_456.2; --  [Jackson], page 3
   --  Needed for Lorentz_Transform, and for Light_Vector_Rotation in Left and Wertz children.

   function Rotate
      (Angle  : in Real_Type;
       Axis   : in Cart_Axis_Type;
       Vector : in Cart_Vector_Type)
       return Cart_Vector_Type;
   function Rotate
      (Sin_Cos : in Math_Scalar.Trig_Pair_Type;
       Axis    : in Cart_Axis_Type;
       Vector  : in Cart_Vector_Type)
       return Cart_Vector_Type;
   --  Active rotation of a vector; equivalent to [1] 2.2.1-5,
   --  eqn:angle_axis_active, but faster. Note that there are no
   --  left/right alternatives for this, since it is not defined as a
   --  multiplication operator.

   function Lorentz_Transform
     (Vel_Obj   : in Cart_Vector_Type;
      Vel_Frame : in Cart_Vector_Type)
     return Cart_Vector_Type;
   --  [1] 2.6.1-2 eqn:lorentz_transform

   ----------
   --  unit vectors

   type Unit_Vector_Type is private;
   --  Private to enforce magnitude = 1.0.

   X_Unit : constant Unit_Vector_Type;
   Y_Unit : constant Unit_Vector_Type;
   Z_Unit : constant Unit_Vector_Type;

   function X (Item : in Unit_Vector_Type) return Real_Type;
   function Y (Item : in Unit_Vector_Type) return Real_Type;
   function Z (Item : in Unit_Vector_Type) return Real_Type;
   pragma Inline (X, Y, Z);

   function To_Unit_Vector (X, Y, Z : in Real_Type) return Unit_Vector_Type;
   pragma Inline (To_Unit_Vector);
   function To_Unit_Vector (Item : in Cart_Vector_Type) return Unit_Vector_Type;
   function "+" (Item : in Cart_Vector_Type) return Unit_Vector_Type renames To_Unit_Vector;
   --  Normalize Item to magnitude 1.0
   --
   --  Raises Non_Normalizable_Unit_Vector if Mag (Item) = 0.0.

   function To_Cart_Vector (Item : in Unit_Vector_Type) return Cart_Vector_Type;
   pragma Inline (To_Cart_Vector);

   function Normalize (Item : in Unit_Vector_Type) return Unit_Vector_Type;
   --  Normalize Item to magnitude 1.0. Useful when Item is derived
   --  from user input, or to eliminate round-off.
   pragma Inline (Normalize);

   function Unchecked_Unit_Vector (Item : in Cart_Vector_Type) return Unit_Vector_Type;
   function Unchecked_Unit_Vector (X, Y, Z : in Real_Type) return Unit_Vector_Type;
   --  Convert Item to a unit vector, with no normalization. This is
   --  suitable when algorithm guarantees normalization.
   pragma Inline (Unchecked_Unit_Vector);

   function "-" (Item : in Unit_Vector_Type) return Unit_Vector_Type;

   function "-" (Left, Right : in Unit_Vector_Type) return Cart_Vector_Type;
   function "+" (Left, Right : in Unit_Vector_Type) return Cart_Vector_Type;

   function "*" (Left : in Unit_Vector_Type; Right : in Real_Type) return Cart_Vector_Type;
   function "*" (Left : in Real_Type; Right : in Unit_Vector_Type) return Cart_Vector_Type;

   function "/" (Left : in Unit_Vector_Type; Right : in Real_Type) return Cart_Vector_Type;

   function Dot (Left : in Unit_Vector_Type; Right : in Unit_Vector_Type) return Real_Type;
   function Dot (Left : in Unit_Vector_Type; Right : in Cart_Vector_Type) return Real_Type;
   function Dot (Left : in Cart_Vector_Type; Right : in Unit_Vector_Type) return Real_Type;
   function "*" (Left : in Unit_Vector_Type; Right : in Unit_Vector_Type) return Real_Type renames Dot;
   function "*" (Left : in Unit_Vector_Type; Right : in Cart_Vector_Type) return Real_Type renames Dot;
   function "*" (Left : in Cart_Vector_Type; Right : in Unit_Vector_Type) return Real_Type renames Dot;
   --  Dot product

   function Cross (Left : in Unit_Vector_Type; Right : in Unit_Vector_Type) return Cart_Vector_Type;
   function Cross (Left : in Unit_Vector_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type;
   function Cross (Left : in Cart_Vector_Type; Right : in Unit_Vector_Type) return Cart_Vector_Type;
   --  Cross product

   function Rotate
      (Angle  : in Real_Type;
       Axis   : in Cart_Axis_Type;
       Vector : in Unit_Vector_Type)
       return Unit_Vector_Type;
   function Rotate
      (Sin_Cos : in Math_Scalar.Trig_Pair_Type;
       Axis    : in Cart_Axis_Type;
       Vector  : in Unit_Vector_Type)
       return Unit_Vector_Type;
   --  Active rotation of a vector; equivalent to [1] 2.2.1-5,
   --  eqn:angle_axis_active, but faster. Note that there are no
   --  left/right alternatives for this, since it is not defined as a
   --  multiplication operator.

   function Light_Vector_Transform
     (Unit_Sc_Star : in Unit_Vector_Type;
      Vel_Sci      : in Cart_Vector_Type)
     return Unit_Vector_Type;
   --  [1] 4.1.1-1 eqn:light_vector_deaberration
   --
   --  Only valid when Mag (Vel_Sci) << Speed_Of_Light.

   ----------
   --  magnitude and axis

   type Mag_Axis_Type is record
      Mag  : Real_Type;
      Axis : Unit_Vector_Type;
   end record;
   --  Suitable for magnitude and axis of any Cartesian vector,
   --  translation or rotation.

   function "-" (Item : in Mag_Axis_Type) return Mag_Axis_Type;

   function "*" (Left : in Real_Type; Right : in Mag_Axis_Type) return Mag_Axis_Type;
   function "*" (Left : in Mag_Axis_Type; Right : in Real_Type) return Mag_Axis_Type;

   function "/" (Left : in Mag_Axis_Type; Right : in Real_Type) return Mag_Axis_Type;

   function To_Cart_Vector (Mag_Axis : in Mag_Axis_Type) return Cart_Vector_Type;
   function To_Mag_Axis (Cart_Vector : in Cart_Vector_Type) return Mag_Axis_Type;

   function To_Mag_Axis (Left, Right : in Unit_Vector_Type) return Mag_Axis_Type;
   --  Return active rotation that rotates Left to Right.
   --  [1] 2.2.1-7, eqn:units_to_angle_axis.
   --
   --  The result magnitude is always positive.

   function Rotate
     (Mag_Axis : in Mag_Axis_Type;
      Vector   : in Cart_Vector_Type)
       return Cart_Vector_Type;
   --  Active rotation of a vector; [1] 2.2.1-5,
   --  eqn:angle_axis_active. Note that there are no left/right
   --  alternatives for this, since it is not defined as a
   --  multiplication operator.

   ----------
   --  Euler angles

   type ZYX_Euler_Type is
   record
      Theta_Z : Real_Type;
      Theta_Y : Real_Type;
      Theta_X : Real_Type;
   end record;
   --  ZYX Euler angles in the ranges :
   --
   --  - Pi   < Theta_Z <= + Pi
   --  - Pi/2 < Theta_Y <= + Pi/2
   --  - Pi   < Theta_X <= + Pi
   --
   --  The singularity is at Theta_Y = +- Pi/2.

   type Celestial_Coordinate_Type is
   record
      --  See [2], section 2.2.2, fig 2-3.
      --  Note that there is a singularity at Declination = +-Pi/2
      Right_Ascension : Real_Type; --    0.0 ..  2 * Pi
      Declination     : Real_Type; --  -Pi/2 .. +Pi/2
   end record;

   function To_Celestial (N : in Unit_Vector_Type) return Celestial_Coordinate_Type;
   --  Convert a unit vector to celestial coordinate angles. When at
   --  the singularity, Right_Ascension is set to 0.0.

   function To_Unit_Vector (Celestial : in Celestial_Coordinate_Type) return Unit_Vector_Type;
   --  Convert celestial coordinate angles to a unit vector.

   ----------
   --  Quaternions

   type Quaternion_Index_Type is (X, Y, Z, S);
   type Quaternion_Type is array (Quaternion_Index_Type) of Real_Type;
   --  For un-normalized user input.

   type Unit_Quaternion_Type is private;

   Zero_Unit_Quaternion : constant Unit_Quaternion_Type;

   function X (Item : in Unit_Quaternion_Type) return Real_Type;
   function Y (Item : in Unit_Quaternion_Type) return Real_Type;
   function Z (Item : in Unit_Quaternion_Type) return Real_Type;
   function S (Item : in Unit_Quaternion_Type) return Real_Type;
   pragma Inline (X, Y, Z, S);

   function To_Unit_Quaternion (Item : in Quaternion_Type) return Unit_Quaternion_Type;
   function To_Unit_Quaternion (X, Y, Z, S : in Real_Type) return Unit_Quaternion_Type;
   --  Return a unit quaternion given its elements. They are assumed
   --  to be unnormalized.
   --
   --  Raises Non_Normalizable_Unit_Quaternion if magnitude of
   --  elements is 0.0.

   function Unchecked_Unit_Quaternion (X, Y, Z, S : in Real_Type) return Unit_Quaternion_Type;
   --  Return a unit quaternion given its elements. They are assumed
   --  to be properly normalized; this is suitable for use when the
   --  algorithm guarantees normalization.
   pragma Inline (Unchecked_Unit_Quaternion);

   function Mag (Item : in Unit_Quaternion_Type) return Real_Type;
   --  Result will be in range -PI .. PI.

   function Unit_Quaternion_Inverse (Item : in Unit_Quaternion_Type) return Unit_Quaternion_Type;
   pragma Inline (Unit_Quaternion_Inverse);
   function Inverse (Item : in Unit_Quaternion_Type) return Unit_Quaternion_Type renames Unit_Quaternion_Inverse;
   --  Return the inverse rotation.

   function "*" (Left, Right : in Unit_Quaternion_Type) return Unit_Quaternion_Type;
   --  Add rotations.

   function Inverse_Times (Left, Right : in Unit_Quaternion_Type) return Unit_Quaternion_Type;
   --  Equivalent to Inverse (Left) * Right, but faster.

   function Times_Inverse (Left, Right : in Unit_Quaternion_Type) return Unit_Quaternion_Type;
   --  Equivalent to Left * Inverse (Right), but faster.

   ----------
   --  General matrices for random purposes.

   type Cart_Array_Cart_Vector_Type is array (Cart_Axis_Type) of Cart_Vector_Type;
   --  Should NOT be used for inertias or rotation matrices; use
   --  Inertia_Type or Rot_Matrix_Type.
   --
   --  Abbreviation : CACV

   package CACV_Ops is new Gen_Math.Gen_Square_Array
      (Index_Type => Cart_Axis_Type,
       Row_Type   => Cart_Vector_Type,
       Array_Type => Cart_Array_Cart_Vector_Type);

   function Inverse (Item : in Cart_Array_Cart_Vector_Type) return Cart_Array_Cart_Vector_Type;
   --  Compute inverse using determinant.
   --
   --  Raises Constraint_Error if Item is singular.

   function Determinant (Item : in Cart_Array_Cart_Vector_Type) return Real_Type;

   ----------
   --  Rotation matrices

   type Rot_Matrix_Type is private;
   --  3 by 3 orthonormal matrices, with determinant +1. Also known as
   --  direction cosine matrices, or orientation matrices.

   Zero_Matrix : constant Rot_Matrix_Type;
   --  Zero rotation; also known as the identity matrix.

   function To_Cart_Array_Cart_Vector (Item : in Rot_Matrix_Type) return Cart_Array_Cart_Vector_Type;
   pragma Inline (To_Cart_Array_Cart_Vector);
   function To_CACV (Item : in Rot_Matrix_Type) return Cart_Array_Cart_Vector_Type renames To_Cart_Array_Cart_Vector;
   --  For private element access, and use with general matrix
   --  algorithms.

   function To_Rot_Matrix (Item : in Cart_Array_Cart_Vector_Type) return Rot_Matrix_Type;
   --  Normalize Item.
   --
   --  Raises Non_Normalizable_Rot_Matrix if matrix cannot be
   --  normalized.

   function Unchecked_Rot_Matrix (Item : in Cart_Array_Cart_Vector_Type) return Rot_Matrix_Type;
   --  Return a rotation matrix given its elements. They are assumed
   --  to be properly normalized. This is suitable for use when the
   --  algorithm guarantees normalization.
   pragma Inline (Unchecked_Rot_Matrix);

   function Mag (Item : in Rot_Matrix_Type) return Real_Type;
   --  Result will be in range -PI .. PI.

   function Inverse (Item : in Rot_Matrix_Type) return Rot_Matrix_Type;
   --  Return the inverse rotation.

   function Rot_Matrix_Times_Rot_Matrix (Left, Right : in Rot_Matrix_Type) return Rot_Matrix_Type;
   function "*" (Left, Right : in Rot_Matrix_Type) return Rot_Matrix_Type renames Rot_Matrix_Times_Rot_Matrix;
   --  Add rotations.
   pragma Inline (Rot_Matrix_Times_Rot_Matrix);

   function Inverse_Times (Left, Right : in Rot_Matrix_Type) return Rot_Matrix_Type;
   --  Equivalent to Inverse (Left) * Right, but faster.
   pragma Inline (Inverse_Times);

   function Times_Inverse (Left, Right : in Rot_Matrix_Type) return Rot_Matrix_Type;
   --  Equivalent to Left * Inverse (Right), but faster.
   pragma Inline (Times_Inverse);

   function Rot_Matrix_Times_CACV
      (Left  : in Rot_Matrix_Type;
       Right : in Cart_Array_Cart_Vector_Type)
      return Cart_Array_Cart_Vector_Type;
   function "*" (Left : in Rot_Matrix_Type; Right : in Cart_Array_Cart_Vector_Type) return Cart_Array_Cart_Vector_Type
     renames Rot_Matrix_Times_CACV;
   pragma Inline (Rot_Matrix_Times_CACV);

   function CACV_Times_Rot_Matrix
      (Left  : in Cart_Array_Cart_Vector_Type;
       Right : in Rot_Matrix_Type)
      return Cart_Array_Cart_Vector_Type;
   function "*" (Left : in Cart_Array_Cart_Vector_Type; Right : in Rot_Matrix_Type) return Cart_Array_Cart_Vector_Type
     renames CACV_Times_Rot_Matrix;
   pragma Inline (CACV_Times_Rot_Matrix);

   ----------
   --  Inertias. See Gen_Math.Gen_DOF_6.Mass_Type for more complete
   --  operations on masses.

   type Inertia_Index_Type is (Ixx, Iyy, Izz, Ixy, Ixz, Iyz);

   type Inertia_Type is array (Inertia_Index_Type) of Real_Type;
   type Inverse_Inertia_Type is array (Inertia_Index_Type) of Real_Type;

   Zero_Inertia : constant Inertia_Type := (others => 0.0);

   function To_Cart_Array_Cart_Vector (Item : in Inertia_Type) return Cart_Array_Cart_Vector_Type;
   function To_CACV (Item : in Inertia_Type) return Cart_Array_Cart_Vector_Type renames To_Cart_Array_Cart_Vector;
   --   For use with general matrix algorithms.

   function "+" (Left, Right : in Inertia_Type) return Inertia_Type;
   function "-" (Left, Right : in Inertia_Type) return Inertia_Type;
   --  Assumes both inertias are in the same frame.

   function "*" (Left : in Inertia_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type;
   --  Normal matrix times vector; returns torque when Right is an
   --  angular acceleration, or angular momentum when Right is an
   --  angular velocity.

   function Inverse (Item : in Inertia_Type) return Inverse_Inertia_Type;

   function "*" (Left : in Inverse_Inertia_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type;
   --  Normal matrix times vector; returns angular velocity when Right
   --  is an angular momentum.

   function Parallel_Axis
      (Total_Mass     : in Real_Type;
       Center_Of_Mass : in Cart_Vector_Type;
       Inertia        : in Inertia_Type)
       return Inertia_Type;
   --  [1], 3.4.1-3 eqn:parallel_axis. Total_Mass is the object's
   --  total mass, Center_Of_Mass is expressed in the desired frame,
   --  Inertia is in a frame parallel to the desired frame, but
   --  centered at the center of mass. The result is the inertia
   --  matrix at the desired frame.

   function Interpolate
     (X  : in Real_Type;
      X1 : in Real_Type;
      X2 : in Real_Type;
      Y1 : in Inertia_Type;
      Y2 : in Inertia_Type)
     return Inertia_Type;
   --  Given Y1 = f (X1), Y2 = f (X2), return Y = f (X), using linear interpolation.

private

   type Unit_Vector_Type is array (Cart_Axis_Type) of Real_Type;

   X_Unit : constant Unit_Vector_Type := (1.0, 0.0, 0.0);
   Y_Unit : constant Unit_Vector_Type := (0.0, 1.0, 0.0);
   Z_Unit : constant Unit_Vector_Type := (0.0, 0.0, 1.0);

   type Unit_Quaternion_Type is record
      X : Real_Type;
      Y : Real_Type;
      Z : Real_Type;
      S : Real_Type;
   end record;

   Zero_Unit_Quaternion : constant Unit_Quaternion_Type := (0.0, 0.0, 0.0, 1.0);

   type Rot_Matrix_Type is new Cart_Array_Cart_Vector_Type;

   Zero_Matrix : constant Rot_Matrix_Type :=
      ((1.0, 0.0, 0.0),
       (0.0, 1.0, 0.0),
       (0.0, 0.0, 1.0));

end SAL.Gen_Math.Gen_DOF_3;
