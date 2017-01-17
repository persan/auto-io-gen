--  Abstract:
--
--  Types and operations for kinematics and dynamics computations for
--  serial manipulators. Currently, for some operations, all joints
--  must be REVOLUTE; this restriction may be removed in a future
--  release.
--
--  Design Decisions:
--
--  Assumes a 6 DOF work space.
--
--  Built on the SAL.Gen_Math_* packages.
--
--  Generic formal type parameters are provided for those types that
--  are typically needed by other parts of a manipulator control
--  system; types that are exclusively used by the kinematics and/or
--  dynamics computations are declared here.
--
--  Slow_* functions are provided for debugging and for cases when the
--  speed of highly optimized code is not absolutely necessary. The
--  bodies are implemented with general algorithms which are easily
--  tested and inspected; they are also fairly fast :-).
--
--  Frame notation: We use the Denavit-Hartenberg formulation of
--  manipulator joint frames and parameters, for generality. T0 is the
--  base of the manipulator. Tx where x is a member of
--  Joint_Index_Type is the frame of joint x. Tlast is the frame of
--  the joint corresponding to Joint_Index_Type'last. Tp is the tool
--  plate frame. Obj is the object frame. T0_T_Ti is the list of poses
--  from T0 to each joint; Ti_T_Obj is the list of poses from each
--  joint to Obj.
--
--  By convention the last joint's D-H parameter D is set to 0.0 if
--  Revolute and Theta is set to 0.0 if Prismatic. This is not
--  enforced by the code. D-H parameters are insufficient to describe
--  an arbitrary offset from the last joint to the tool plate;
--  Tlast_T_Tp is used for this.
--
--  Jacobian_Type, Inverse_Jacobian_Type, ProjectorS, and Inertia_Type
--  should be private to insure the relationship between the joint
--  angles and the values. However, the user will typically want to
--  provide bodies that are optimized for their particular
--  manipulator, which is not possible in Ada 83 without excessive
--  unchecked conversion. Thus we settle for derived types, and wait
--  for Ada 94 with its daughter packages and function access types.
--
--  (Editor's note; this code was written a long time ago :).
--
--  Transpose operators are not provided because: multiply operators
--  automatically provide transposes when needed, and types with
--  identical types for row and column indices are often symetric so
--  no transpose is needed.
--
--  Another way to attempt this package would be to define
--  unconstrained joint_array_* types, and then declare constrained
--  derived types. However, this does not eliminate all run-time
--  checks, and most compilers pass unconstrained base types via the
--  heap, incurring significant run-time overhead.
--
--  Copyright (C) 2002, 2004 Stephen Leake.  All Rights Reserved.
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
--
--  MODIFICATION HISTORY:
--    (kept for sentimental interest)
--
--  Oct. 9, 1991     Dana Miller     Created
--  Dec  19, 1991    Dana Miller
--      changes from code review: Fixed design decisions. renamed functions
--      and procedures to reflect the Mathmatical notation corresponding to
--      them.  Added To_Base_Pose_List
--  Jan 22, 1991    Dana Miller
--      Changed to use Manipulator_Math package
--  24 Jan 1992     Stephe Leake
--      add "*" (VELOCITY_PROPAGATORS, JacobianS). Fix spec of Slow_Inertia
--      to match what body needs.
--  15 June 1992    Stephe Leake
--      Change name to Generic_Manipulator_Math, clean up comments, replace
--      one Slow_Jacobian with Inverse. Add Incremental_To_Joint.
--  24 June 1992    Stephe Leake
--      make all type conversion subprogram names plural. Add
--      Null_Space_Projector. Make visible all operations on DC_Array_JARS,
--      Joint_Array_DCVS, using new Generic_Inverse_Array_Math.
--   8 July 1992    Stephe Leake
--      match changes to Math_6_DOF spec
--   9 Nov 1992     Stephe Leake
--      add operations on JacobianS, Inverse_JacobianS.
--  30 Nov 1993 Stephe Leake
--      convert to _Type naming convention, match changes to with'ed specs.
--  31 August 1996  Stephe Leake
--      replace Generic_Array_Math.Convert_Array with Generic_Convert_Array.
--      delete renames of Joint_DCV_Ops; they confuse gnat.
--  21 May 2002 Stephe Leake
--      change to use SAL.Gen_Math_*, match my current style conventions.

with Ada.Numerics.Generic_Elementary_Functions;
with SAL.Gen_Convert_Array;
with SAL.Gen_Math.Gen_Inverse_Array;
with SAL.Gen_Math.Gen_Scalar;
with SAL.Gen_Math.Gen_Den_Hart;
with SAL.Gen_Math.Gen_DOF_3;
with SAL.Gen_Math.Gen_DOF_6;
with SAL.Gen_Math.Gen_Vector;
with SAL.Gen_Math.Gen_Square_Array;
generic
   --  Auto_Io_Gen : ignore
   with package Elementary    is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
   with package Math_Scalar   is new SAL.Gen_Math.Gen_Scalar (Elementary);
   with package Math_DOF_3    is new SAL.Gen_Math.Gen_DOF_3 (Elementary, Math_Scalar);
   with package Math_DOF_6    is new SAL.Gen_Math.Gen_DOF_6 (Elementary, Math_Scalar, Math_DOF_3);
   --  Auto_Io_Gen : ignore
   with package Math_Den_Hart is new SAL.Gen_Math.Gen_Den_Hart (Elementary, Math_Scalar, Math_DOF_3, Math_DOF_6);

   with function Math_DOF_6_DC_Array_DCV_Inverse
     (Right : in Math_DOF_6.DC_Array_DCV_Type)
     return Math_DOF_6.DC_Array_DCV_Type;

   type Joint_Index_Type          is (<>);
   --  Auto_Io_Gen : ignore
   type Joint_Array_Real_Type     is array (Joint_Index_Type) of Real_Type;
   --  Auto_Io_Gen : ignore
   type Joint_Array_Pose_Type     is array (Joint_Index_Type) of Math_DOF_6.Pose_Type;
   --  Auto_Io_Gen : ignore
   type Joint_Array_Mass_Type     is array (Joint_Index_Type) of Math_DOF_6.Mass_Type;
   pragma Unreferenced (Joint_Array_Mass_Type); -- referenced in child packages
   --  Auto_Io_Gen : ignore
   type Joint_Array_Den_Hart_Type is array (Joint_Index_Type) of Math_Den_Hart.Den_Hart_Type;
   pragma Unreferenced (Joint_Array_Den_Hart_Type); -- referenced in child packages

package SAL.Gen_Math.Gen_Manipulator is
   pragma Pure;

   type Joint_Array_Boolean_Type           is array (Joint_Index_Type) of Boolean;
   type Joint_Array_Limit_Type             is array (Joint_Index_Type) of Math_Scalar.Limit_Type;
   --  Auto_Io_Gen : ignore
   type Joint_Array_JAR_Type               is array (Joint_Index_Type) of Joint_Array_Real_Type;
   type Joint_Array_Wrench_Transform_Type  is array (Joint_Index_Type) of Math_DOF_6.Wrench_Transform_Type;
   type Joint_Array_DCV_Type               is array (Joint_Index_Type) of Math_DOF_6.Dual_Cart_Vector_Type;

   --  Auto_Io_Gen : ignore
   type DC_Array_JAR_Type                  is array (Math_DOF_6.Dual_Cart_Axis_Type) of Joint_Array_Real_Type;

   package Joint_Array_Real_Ops is new SAL.Gen_Math.Gen_Vector
     (Elementary               => Elementary,
      Math_Scalar              => Math_Scalar,
      Index_Type               => Joint_Index_Type,
      Index_Array_Boolean_Type => Joint_Array_Boolean_Type,
      Index_Array_Real_Type    => Joint_Array_Real_Type,
      Index_Array_Limit_Type   => Joint_Array_Limit_Type);

   package Joint_Array_JAR_Ops is new SAL.Gen_Math.Gen_Square_Array
     (Index_Type => Joint_Index_Type,
      Row_Type   => Joint_Array_Real_Type,
      Array_Type => Joint_Array_JAR_Type);

   --  Trig
   type Joint_Array_Trig_Pair_Type is array (Joint_Index_Type) of Math_Scalar.Trig_Pair_Type;

   function To_Sin_Cos is new SAL.Gen_Convert_Array
     (Index_Type              => Joint_Index_Type,
      Input_Element_Type      => Real_Type,
      Index_Array_Input_Type  => Joint_Array_Real_Type,
      Output_Element_Type     => Math_Scalar.Trig_Pair_Type,
      Index_Array_Output_Type => Joint_Array_Trig_Pair_Type,
      Convert_Element         => Math_Scalar.Sin_Cos);
   function Sin_Cos (Radians : in Joint_Array_Real_Type) return Joint_Array_Trig_Pair_Type renames To_Sin_Cos;

   function To_Double_Trig is new SAL.Gen_Convert_Array
     (Index_Type              => Joint_Index_Type,
      Input_Element_Type      => Math_Scalar.Trig_Pair_Type,
      Index_Array_Input_Type  => Joint_Array_Trig_Pair_Type,
      Output_Element_Type     => Math_Scalar.Trig_Pair_Type,
      Index_Array_Output_Type => Joint_Array_Trig_Pair_Type,
      Convert_Element         => Math_Scalar.Double_Trig);
   function Double_Trig (Trig : in Joint_Array_Trig_Pair_Type) return Joint_Array_Trig_Pair_Type
     renames To_Double_Trig;

   function To_Half_Trig is new SAL.Gen_Convert_Array
     (Index_Type              => Joint_Index_Type,
      Input_Element_Type      => Math_Scalar.Trig_Pair_Type,
      Index_Array_Input_Type  => Joint_Array_Trig_Pair_Type,
      Output_Element_Type     => Math_Scalar.Trig_Pair_Type,
      Index_Array_Output_Type => Joint_Array_Trig_Pair_Type,
      Convert_Element         => Math_Scalar.Half_Trig);
   function Half_Trig (Trig : in Joint_Array_Trig_Pair_Type) return Joint_Array_Trig_Pair_Type renames To_Half_Trig;

   function To_Sin is new SAL.Gen_Convert_Array
     (Index_Type              => Joint_Index_Type,
      Input_Element_Type      => Math_Scalar.Trig_Pair_Type,
      Index_Array_Input_Type  => Joint_Array_Trig_Pair_Type,
      Output_Element_Type     => Real_Type,
      Index_Array_Output_Type => Joint_Array_Real_Type,
      Convert_Element         => Math_Scalar.Sin);
   function Sin (Trig : in Joint_Array_Trig_Pair_Type) return Joint_Array_Real_Type renames To_Sin;

   function To_Cos is new SAL.Gen_Convert_Array
     (Index_Type              => Joint_Index_Type,
      Input_Element_Type      => Math_Scalar.Trig_Pair_Type,
      Index_Array_Input_Type  => Joint_Array_Trig_Pair_Type,
      Output_Element_Type     => Real_Type,
      Index_Array_Output_Type => Joint_Array_Real_Type,
      Convert_Element         => Math_Scalar.Cos);
   function Cos (Trig : in Joint_Array_Trig_Pair_Type) return Joint_Array_Real_Type renames To_Cos;

   package Joint_Array_DCV_Ops is new SAL.Gen_Math.Gen_Inverse_Array
     (A_Index_Type      => Math_DOF_6.Dual_Cart_Axis_Type,
      B_Index_Type      => Joint_Index_Type,
      A_Array_Real_Type => Math_DOF_6.Dual_Cart_Vector_Type,
      B_Array_Real_Type => Joint_Array_Real_Type,
      A_Array_BAR_Type  => DC_Array_JAR_Type,
      B_Array_AAR_Type  => Joint_Array_DCV_Type,
      A_Array_AAR_Type  => Math_DOF_6.DC_Array_DCV_Type,
      B_Array_BAR_Type  => Joint_Array_JAR_Type,
      Inverse           => Math_DOF_6_DC_Array_DCV_Inverse);

   --  Jacobians

   --  Auto_Io_Gen : ignore
   type Jacobian_Type is new DC_Array_JAR_Type;
   type Inverse_Jacobian_Type is new Joint_Array_DCV_Type;

   function "*" (Left : in Jacobian_Type; Right : in Joint_Array_Real_Type) return Math_DOF_6.Dual_Cart_Vector_Type;
   function "*" (Left : in Jacobian_Type; Right : in Math_DOF_6.Dual_Cart_Vector_Type) return Joint_Array_Real_Type;
   --  Not derived from DC_Array_JAR_Type, because declared in a nested package.

   function "*"
     (Left  : in Inverse_Jacobian_Type;
      Right : in Math_DOF_6.Dual_Cart_Vector_Type)
     return Joint_Array_Real_Type;
   function "*"
     (Left  : in Inverse_Jacobian_Type;
      Right : in Joint_Array_Real_Type)
     return Math_DOF_6.Dual_Cart_Vector_Type;
   --  Not derived from Joint_Array_DCV_Type, because declared in a nested package.

   Zero_Jacobian         : constant Jacobian_Type         := (others => (others => 0.0));
   Zero_Inverse_Jacobian : constant Inverse_Jacobian_Type := (others => (others => 0.0));
   Zero_Joint_Array_Pose : constant Joint_Array_Pose_Type := (others => Math_DOF_6.Zero_Pose);

   function Inverse (Right : in Jacobian_Type) return Inverse_Jacobian_Type;
   --  Returns the Moore-Penrose psuedo-inverse of Right.
   --
   --  Raises Array_Math_Exceptions.SINGULAR if inverse cannot be
   --  found.

   --  Projectors

   --  Auto_Io_Gen : ignore
   type Projector_Type is new Joint_Array_JAR_Type;

   function "*" (Left : in Projector_Type; Right : in Joint_Array_Real_Type) return Joint_Array_Real_Type;
   --  Not derived from Joint_Array_JAR_Type, because that's a generic
   --  formal parameter.

   function Null_Space_Projector
     (Forward : in Jacobian_Type;
      Inverse : in Inverse_Jacobian_Type)
     return Projector_Type;
   --  If Joint_Array_Real_Type'length <= 6, returns (others =>
   --  (others => 0.0)). Inverse must be a psuedo-inverse of Forward.

   --  Auto_Io_Gen : ignore
   type Inertia_Type is new Joint_Array_JAR_Type;

end SAL.Gen_Math.Gen_Manipulator;
