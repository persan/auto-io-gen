--  Abstract:
--
--  Math types and operations for 6 Cartesian degrees of freedom. One
--  degree of freedom types and operations are in Gen_Math.Gen_Scalar,
--  and 3 Cartesian degrees of freedom types and operations are in
--  Gen_Math.Gen_DOF_3.
--
--  Operations that depend on a choice of left or right multiply are
--  in child packages.
--
--  References:
--
--  [1] Spacecraft Math, Stephen Leake
--
--  Design :
--
--  All rotation units are radians, all translation units are up to
--  the user.
--
--  By the common style convention, Dual_Cart_Vector_Type should be
--  named Dual_Cart_Array_Real_Type. We use Dual_Cart_Vector_Type out
--  of recognition of the overwhelming influence of Cartesian
--  geometry. 'Dual' comes from the duality of rotation and
--  translation; in group theory jargon, the Dual_Cart_Vectors live in
--  SO3 cross R3.
--
--  Dual_Real_Type is not Dual_Array_Real_Type, because it is only two
--  elements, which is usually the magnitude of a
--  Dual_Cart_Vector_Type. We need most of the operations of
--  Gen_Vector_Math, but the objects of this type are not really used
--  as arrays.
--
--  We provide Transform_Force, but no equivalent
--  Transform_Translation_Rate, because there are common forces that
--  have no rotation component (ie gravity), but rates with no
--  rotation component are rare, and the simplification is not as
--  significant.
--
--  Copyright (C) 2001 - 2003, 2007 Stephen Leake.  All Rights Reserved.
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
with SAL.Gen_Math.Gen_DOF_3;
generic
   --  Auto_Text_IO : ignore
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
   with package Math_Scalar is new SAL.Gen_Math.Gen_Scalar (Elementary);
   with package Math_DOF_3 is new SAL.Gen_Math.Gen_DOF_3 (Elementary, Math_Scalar);
package SAL.Gen_Math.Gen_DOF_6 is
   pragma Pure;

   --  Dual_Real_Type operations

   type Dual_Axis_Type is (Tran, Rot);

   type Dual_Boolean_Type is array (Dual_Axis_Type) of Boolean;
   type Dual_Real_Type    is array (Dual_Axis_Type) of Real_Type;
   type Dual_Limit_Type   is array (Dual_Axis_Type) of Math_Scalar.Limit_Type;

   package Dual_Real_Ops is new Gen_Vector
      (Elementary               => Elementary,
       Math_Scalar              => Math_Scalar,
       Index_Type               => Dual_Axis_Type,
       Index_Array_Boolean_Type => Dual_Boolean_Type,
       Index_Array_Real_Type    => Dual_Real_Type,
       Index_Array_Limit_Type   => Dual_Limit_Type);

   --  Other Dual_Real_Type ops

   function "<=" (Left, Right : in Dual_Real_Type) return Boolean;
   --  True if both components of left are less than or equal to the
   --  corresponding components of Right. This is used instead of "<="
   --  (Dual_Real_Type, Dual_Limit_Type) when Left is a magnitude,
   --  which is inherently positive.

   ----------
   --  Dual_Cart operations

   type Dual_Cart_Axis_Type is (TX, TY, TZ, RX, RY, RZ);
   --  Translation x, y, z; rotation x, y, z

   subtype Tran_Axis_Type is Dual_Cart_Axis_Type range TX .. TZ;
   subtype Rot_Axis_Type  is Dual_Cart_Axis_Type range RX .. RZ;

   type Dual_Cart_Array_Boolean_Type is array (Dual_Cart_Axis_Type) of Boolean;
   type Dual_Cart_Vector_Type        is array (Dual_Cart_Axis_Type) of Real_Type;
   --  Abbreviation DCV.
   type Dual_Cart_Array_Limit_Type   is array (Dual_Cart_Axis_Type) of Math_Scalar.Limit_Type;

   Zero_Dual_Cart_Vector : constant Dual_Cart_Vector_Type := (others => 0.0);

   package DCV_Ops is new Gen_Vector
      (Elementary               => Elementary,
       Math_Scalar              => Math_Scalar,
       Index_Type               => Dual_Cart_Axis_Type,
       Index_Array_Boolean_Type => Dual_Cart_Array_Boolean_Type,
       Index_Array_Real_Type    => Dual_Cart_Vector_Type,
       Index_Array_Limit_Type   => Dual_Cart_Array_Limit_Type);

   --  Other Dual_Cart_Vector_Type ops

   function Translation (Item : in Dual_Cart_Vector_Type) return Math_DOF_3.Cart_Vector_Type;
   --  Return the translation portion of the Dual_Cart_Vector.

   function Rotation (Item : in Dual_Cart_Vector_Type) return Math_DOF_3.Cart_Vector_Type;
   --  Return the rotation portion of the Dual_Cart_Vector.

   function Concat (Translation, Rotation : in Math_DOF_3.Cart_Vector_Type) return Dual_Cart_Vector_Type;
   function "&" (Translation, Rotation : in Math_DOF_3.Cart_Vector_Type) return Dual_Cart_Vector_Type renames Concat;
   --  Return the resulting Dual_Cart_Vector formed with the two
   --  Cart_Vector_Type.

   pragma Inline (Rotation, Translation, "&");

   function Mag (Item : in Dual_Cart_Vector_Type) return Dual_Real_Type;
   --  Return the dual Euclidean magnitude of a dual vector.

   function "*" (Left : in Dual_Real_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   function "*" (Left : in Dual_Cart_Vector_Type; Right : in Dual_Real_Type) return Dual_Cart_Vector_Type;

   function "/" (Left : in Dual_Cart_Vector_Type; Right : in Dual_Real_Type) return Dual_Cart_Vector_Type;

   ----------
   --  Pose operations

   type Pose_Type is record
      Translation : Math_DOF_3.Cart_Vector_Type;
      Rotation    : Math_DOF_3.Unit_Quaternion_Type;
   end record;
   --  A six dimensional displacement; a frame transformation. See [1]
   --  section 3.1.

   Zero_Pose : constant Pose_Type := (Math_DOF_3.Zero_Cart_Vector, Math_DOF_3.Zero_Unit_Quaternion);

   function Mag (Item : in Pose_Type) return Dual_Real_Type;
   --  The magnitude of the rotation and translation parts.

   ----------
   --  General purpose matrices.

   type DC_Array_DCV_Type is array (Dual_Cart_Axis_Type) of Dual_Cart_Vector_Type;
   --  Useful for information relating DCVs to DCVs such as (but not
   --  limited to) Cartesian stiffness and compliance and calibration
   --  matrices for wrench sensors

   package DC_Array_DCV_Ops is new Gen_Square_Array
      (Index_Type => Dual_Cart_Axis_Type,
       Row_Type   => Dual_Cart_Vector_Type,
       Array_Type => DC_Array_DCV_Type);

   ----------
   --  Wrench and rate transforms

   type Rate_Transform_Type is private;
   type Wrench_Transform_Type is private;
   --  Used to transform either a rate or a wrench to another frame.

   Zero_Rate_Transform   : constant Rate_Transform_Type;
   Zero_Wrench_Transform : constant Wrench_Transform_Type;

   function Unchecked_Rate_Transform (Rot, Rot_Cross : Math_DOF_3.Cart_Array_Cart_Vector_Type)
      return Rate_Transform_Type;
   function Unchecked_Wrench_Transform (Rot, Rot_Cross : Math_DOF_3.Cart_Array_Cart_Vector_Type)
      return Wrench_Transform_Type;
   pragma Inline (Unchecked_Rate_Transform, Unchecked_Wrench_Transform);

   function To_Rate_Transform (Right : in Wrench_Transform_Type) return Rate_Transform_Type;
   function To_Wrench_Transform (Right : in Rate_Transform_Type) return Wrench_Transform_Type;
   pragma Inline (To_Wrench_Transform, To_Rate_Transform);

   function Transform_Rate
      (Disp : in Math_DOF_3.Cart_Vector_Type;
       Rate : in Dual_Cart_Vector_Type)
      return Dual_Cart_Vector_Type;
   --  Same as Transform_Rate ((Left, Zero_Unit_Quaternion), Rate),
   --  but faster.

   function Transform_Wrench
      (Disp   : in Math_DOF_3.Cart_Vector_Type;
       Wrench : in Dual_Cart_Vector_Type)
      return Dual_Cart_Vector_Type;
   --  Same as Transform_Wrench ((Disp, Zero_Unit_Quaternion), Wrench),
   --  but faster.

   function Transform_Force
      (Disp  : in Math_DOF_3.Cart_Vector_Type;
       Force : in Math_DOF_3.Cart_Vector_Type)
      return Dual_Cart_Vector_Type;
   --  Same as Transform_Wrench ((Disp, Zero_Unit_Quaternion), (Force,
   --  Zero_Cart_Vector)), but faster.

   ----------
   --  Dual magnitude and axis

   type Dual_Mag_Axis_Type is record
      Translation : Math_DOF_3.Mag_Axis_Type;
      Rotation    : Math_DOF_3.Mag_Axis_Type;
   end record;
   --  Suitable for magnitude and axis representation of six dimensional
   --  velocities, wrenches, differential displacements.

   function To_Dual_Mag_Axis (Dual_Cart_Vector : in Dual_Cart_Vector_Type) return Dual_Mag_Axis_Type;
   function To_Dual_Cart_Vector (Dual_Mag_Axis : in Dual_Mag_Axis_Type) return Dual_Cart_Vector_Type;

   function Mag (Item : in Dual_Mag_Axis_Type) return Dual_Real_Type;
   --  The dual magnitude of the translation and rotation parts.

   function "-" (Item : in Dual_Mag_Axis_Type) return Dual_Mag_Axis_Type;

   function "*" (Left : in Dual_Real_Type; Right : in Dual_Mag_Axis_Type) return Dual_Mag_Axis_Type;
   function "*" (Left : in Dual_Mag_Axis_Type; Right : in Dual_Real_Type) return Dual_Mag_Axis_Type;
   --  Left.Translation * Right.Translation, Left.Rotation * Right.Rotation.

   function "/" (Left : in Dual_Mag_Axis_Type; Right : in Dual_Real_Type) return Dual_Mag_Axis_Type;
   --  Left.Translation / Right.Translation, Left.Rotation / Right.Rotation.

   function "*" (Left : in Real_Type; Right : in Dual_Mag_Axis_Type) return Dual_Mag_Axis_Type;
   function "*" (Left : in Dual_Mag_Axis_Type; Right : in Real_Type) return Dual_Mag_Axis_Type;
   function "/" (Left : in Dual_Mag_Axis_Type; Right : in Real_Type) return Dual_Mag_Axis_Type;

   ----------
   --  Mass properties

   --  Auto_Text_IO : separate - don't put, get redundant Inertia value; compute it on Get
   type Mass_Type is private;
   --  Private to cache inertia.

   Zero_Mass : constant Mass_Type;

   function Total (Item : in Mass_Type) return Real_Type;
   --  Return total mass of Item.

   function Center (Item : in Mass_Type) return Math_DOF_3.Cart_Vector_Type;
   --  Return the center of mass of Item, expressed in the object frame.

   function Center_Inertia (Item : in Mass_Type) return Math_DOF_3.Inertia_Type;
   --  Return the inertia about the center of mass, expressed in the
   --  object frame.

   function Inertia (Item : in Mass_Type) return Math_DOF_3.Inertia_Type;
   --  Return the inertia about the object frame, expressed in the
   --  object frame.

   function To_Mass
      (Total          : in Real_Type;
       Center         : in Math_DOF_3.Cart_Vector_Type;
       Center_Inertia : in Math_DOF_3.Inertia_Type)
      return Mass_Type;
   --  Total is the total mass. Center is the center of mass expressed
   --  in the object frame. Center_Inertia is about the center of
   --  mass, expressed in the object frame. If Total <
   --  Real_Type'small, returns Zero_Mass.

   function "*" (Left : in Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   --  Mass * acceleration => wrench, or Mass * velocity => momentum.
   --  Right and result are in Left frame.

   function Inverse_Times (Left : in Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   --  Inverse Mass * momentum => velocity. Right and result are in
   --  Left frame.

   ----------
   --  Simple Mass properties; mass frame at center of mass

   type CM_Mass_Type is record
      Total          : Real_Type;
      Center_Inertia : Math_DOF_3.Inertia_Type;
   end record;

   type CM_Inverse_Mass_Type is record
      Inverse_Total          : Real_Type;
      Inverse_Center_Inertia : Math_DOF_3.Inverse_Inertia_Type;
   end record;

   function To_CM_Mass (Item : in Mass_Type) return CM_Mass_Type;

   function To_Mass
     (CM_Mass      : in CM_Mass_Type;
      CM_Tran_Body : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type;

   function Inverse (Item : in CM_Mass_Type) return CM_Inverse_Mass_Type;

   function "*" (Left : in CM_Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   function Mass_Times_Tran
     (Left  : in CM_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type;
   function Mass_Times_Rot
     (Left  : in CM_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type;
   --  Mass * acceleration => wrench, or Mass * velocity => momentum.
   --  Right and result are in Left frame.

   function "*" (Left : in CM_Inverse_Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   function Inverse_Mass_Times_Tran
     (Left  : in CM_Inverse_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type;
   function Inverse_Mass_Times_Rot
     (Left  : in CM_Inverse_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type;
   function Inverse_Times (Left : in CM_Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type;
   function Inverse_Mass_Times_Tran
     (Left  : in CM_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type;
   function Inverse_Mass_Times_Rot
     (Left  : in CM_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type;
   --  Inverse Mass * momentum => velocity. Right and result are in
   --  Left frame.

private

   type Rate_Transform_Type is record
      Rot       : Math_DOF_3.Cart_Array_Cart_Vector_Type;
      Rot_Cross : Math_DOF_3.Cart_Array_Cart_Vector_Type;
   end record;
   --  Rot could be Rot_Matrix_Type, but every operation on it would
   --  involve a conversion to Cart_Array_Cart_Vector_Type, so this is
   --  cleaner.

   type Wrench_Transform_Type is record
      Rot       : Math_DOF_3.Cart_Array_Cart_Vector_Type;
      Rot_Cross : Math_DOF_3.Cart_Array_Cart_Vector_Type;
   end record;
   --  Rot could be Rot_Matrix_Type, but every operation on it would
   --  involve a conversion to Cart_Array_Cart_Vector_Type, so this is
   --  cleaner.

   Zero_Rate_Transform   : constant Rate_Transform_Type   := (others => Math_DOF_3.CACV_Ops.Identity);
   Zero_Wrench_Transform : constant Wrench_Transform_Type := (others => Math_DOF_3.CACV_Ops.Identity);

   type Mass_Type is record
      Total          : Real_Type;
      Center         : Math_DOF_3.Cart_Vector_Type;
      Center_Inertia : Math_DOF_3.Inertia_Type;
      Inertia        : Math_DOF_3.Inertia_Type;
   end record;
   --  Defined as for To_Mass and Inertia above. Both Center_Inertia
   --  and Inertia are stored to save compute time; Inertia is a fixed
   --  function of Center_Inertia, Mass and Center.

   Zero_Mass : constant Mass_Type :=
      (0.0, Math_DOF_3.Zero_Cart_Vector, Math_DOF_3.Zero_Inertia, Math_DOF_3.Zero_Inertia);

end SAL.Gen_Math.Gen_DOF_6;
