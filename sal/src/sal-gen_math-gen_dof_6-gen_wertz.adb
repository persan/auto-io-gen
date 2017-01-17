--  Abstract:
--
--  see spec
--
--  Copyright (C) 2001 - 2003, 2005, 2007 - 2008 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_DOF_6.Gen_Wertz is

   --  Private subprograms

   function To_Rot_Cross
      (Inverse_Rot : in Math_DOF_3.Cart_Array_Cart_Vector_Type;
       Tran        : in Math_DOF_3.Cart_Vector_Type)
      return Math_DOF_3.Cart_Array_Cart_Vector_Type
      --  return the Rot_Cross part of a propagator, given the inverse of the rotation part of the pose.
   is
      use Math_DOF_3, Math_Scalar;
   begin
      --  See ../Doc/derive_rate_transform.maxima
      return
         (X =>
             (X => Inverse_Rot (X)(Z) * Tran (Y) - Inverse_Rot (X)(Y) * Tran (Z),
              Y => Inverse_Rot (X)(X) * Tran (Z) - Inverse_Rot (X)(Z) * Tran (X),
              Z => Inverse_Rot (X)(Y) * Tran (X) - Inverse_Rot (X)(X) * Tran (Y)),
          Y =>
             (X => Inverse_Rot (Y)(Z) * Tran (Y) - Inverse_Rot (Y)(Y) * Tran (Z),
              Y => Inverse_Rot (Y)(X) * Tran (Z) - Inverse_Rot (Y)(Z) * Tran (X),
              Z => Inverse_Rot (Y)(Y) * Tran (X) - Inverse_Rot (Y)(X) * Tran (Y)),
          Z =>
             (X => Inverse_Rot (Z)(Z) * Tran (Y) - Inverse_Rot (Z)(Y) * Tran (Z),
              Y => Inverse_Rot (Z)(X) * Tran (Z) - Inverse_Rot (Z)(Z) * Tran (X),
              Z => Inverse_Rot (Z)(Y) * Tran (X) - Inverse_Rot (Z)(X) * Tran (Y))
          );
   end To_Rot_Cross;

   ----------
   --  Public subprograms

   --  Dual_Cart operations

   function "*"
     (Left  : in Dual_Cart_Vector_Type;
      Right : in Math_DOF_3.Unit_Quaternion_Type)
     return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
   begin
      return Translation (Left) * Right & Rotation (Left) * Right;
   end "*";

   function Times_Inverse
     (Left  : in Dual_Cart_Vector_Type;
      Right : in Math_DOF_3.Unit_Quaternion_Type)
     return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
   begin
      return Times_Inverse (Translation (Left), Right) & Times_Inverse (Rotation (Left), Right);
   end Times_Inverse;

   ----------
   --  Pose_Type operations

   function To_Dual_Cart_Vector (Pose : in Pose_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
   begin
      return Pose.Translation & To_Rot_Vector (Pose.Rotation);
   end To_Dual_Cart_Vector;

   function To_Pose (Dual_Cart_Vector : in Dual_Cart_Vector_Type) return Pose_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
   begin
      return (Translation (Dual_Cart_Vector), To_Unit_Quaternion (Rotation (Dual_Cart_Vector)));
   end To_Pose;

   function Inverse (Item : in Pose_Type) return Pose_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, Math_DOF_3_Wertz;
   begin
      return (-Times_Inverse (Item.Translation, Item.Rotation), Inverse (Item.Rotation));
   end Inverse;

   function "*" (Left, Right : in Pose_Type) return Pose_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, Math_DOF_3_Wertz;
   begin
      return (Left.Translation * Right.Rotation + Right.Translation, Left.Rotation * Right.Rotation);
   end "*";

   function "*" (Left : in Math_DOF_3.Unit_Quaternion_Type; Right : in Pose_Type) return Pose_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
   begin
      return (Right.Translation, Left * Right.Rotation);
   end "*";

   function "*" (Left : in Pose_Type; Right : in Math_DOF_3.Unit_Quaternion_Type) return Pose_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
   begin
      return (Left.Translation * Right, Left.Rotation * Right);
   end "*";

   function "*" (Left : in Math_DOF_3.Cart_Vector_Type; Right : in Pose_Type) return Math_DOF_3.Cart_Vector_Type
   is
      use Math_DOF_3.Cart_Vector_Ops, Math_DOF_3_Wertz;
   begin
      return Left * Right.Rotation + Right.Translation;
   end "*";

   function Times_Inverse (Left, Right : in Pose_Type) return Pose_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, Math_DOF_3_Wertz;
   begin
      return
        (Times_Inverse ((Left.Translation - Right.Translation), Right.Rotation),
         Times_Inverse (Left.Rotation, Right.Rotation));
   end Times_Inverse;

   ----------
   --  Dual_Mag_Axis_Type

   function To_Dual_Mag_Axis (Pose : in Pose_Type) return Dual_Mag_Axis_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
   begin
      return (To_Mag_Axis (Pose.Translation), To_Mag_Axis (Pose.Rotation));
   end To_Dual_Mag_Axis;

   function To_Pose (Dual_Mag_Axis : in Dual_Mag_Axis_Type) return Pose_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
   begin
      return (To_Cart_Vector (Dual_Mag_Axis.Translation), To_Unit_Quaternion (Dual_Mag_Axis.Rotation));
   end To_Pose;

   ----------
   --  Wrench and rate transforms

   function To_Rate_Transform (Item : in Pose_Type) return Rate_Transform_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
      Inverse_Rot : constant Cart_Array_Cart_Vector_Type := To_CACV (To_Rot_Matrix (Inverse (Item.Rotation)));
   begin
      return (Inverse_Rot, To_Rot_Cross (Inverse_Rot, Item.Translation));
   end To_Rate_Transform;

   function To_Wrench_Transform (Item : in Pose_Type) return Wrench_Transform_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
      Inverse_Rot : constant Cart_Array_Cart_Vector_Type := To_CACV (To_Rot_Matrix (Inverse (Item.Rotation)));
   begin
      return (Inverse_Rot, To_Rot_Cross (Inverse_Rot, Item.Translation));
   end To_Wrench_Transform;

   function To_Rate_Transform
      (Translation : in Math_DOF_3.Cart_Vector_Type;
       Rotation    : in Math_DOF_3.Rot_Matrix_Type)
      return Rate_Transform_Type
   is
      use Math_DOF_3;
      Inverse_Rot : constant Cart_Array_Cart_Vector_Type := To_CACV (Inverse (Rotation));
   begin
      return (Inverse_Rot, To_Rot_Cross (Inverse_Rot, Translation));
   end To_Rate_Transform;

   function To_Wrench_Transform
      (Translation : in Math_DOF_3.Cart_Vector_Type;
       Rotation    : in Math_DOF_3.Rot_Matrix_Type)
      return Wrench_Transform_Type
   is
      use Math_DOF_3;
      Inverse_Rot : constant Cart_Array_Cart_Vector_Type := To_CACV (Inverse (Rotation));
   begin
      return (Inverse_Rot, To_Rot_Cross (Inverse_Rot, Translation));
   end To_Wrench_Transform;

   function To_DC_Array_DCV (Item : in Rate_Transform_Type) return DC_Array_DCV_Type
   is
      use Math_DOF_3;
   begin
      return
         (TX => Item.Rot (X) & Item.Rot_Cross (X),
          TY => Item.Rot (Y) & Item.Rot_Cross (Y),
          TZ => Item.Rot (Z) & Item.Rot_Cross (Z),
          RX => Zero_Cart_Vector & Item.Rot (X),
          RY => Zero_Cart_Vector & Item.Rot (Y),
          RZ => Zero_Cart_Vector & Item.Rot (Z));
   end To_DC_Array_DCV;

   function To_DC_Array_DCV (Item : in Wrench_Transform_Type) return DC_Array_DCV_Type
   is
      use Math_DOF_3;
   begin
      return
         (TX => Item.Rot (X) & Zero_Cart_Vector,
          TY => Item.Rot (Y) & Zero_Cart_Vector,
          TZ => Item.Rot (Z) & Zero_Cart_Vector,
          RX => Item.Rot_Cross (X) & Item.Rot (X),
          RY => Item.Rot_Cross (Y) & Item.Rot (Y),
          RZ => Item.Rot_Cross (Z) & Item.Rot (Z));
   end To_DC_Array_DCV;

   function "*" (Left, Right : in Rate_Transform_Type) return Rate_Transform_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, Math_DOF_3.CACV_Ops;
   begin
      return (Left.Rot * Right.Rot, Left.Rot * Right.Rot_Cross + Left.Rot_Cross * Right.Rot);
   end "*";

   function "*" (Left : in Rate_Transform_Type; Right : in DC_Array_DCV_Type) return DC_Array_DCV_Type
   is
      use DC_Array_DCV_Ops;
   begin
      return To_DC_Array_DCV (Left) * Right;
   end "*";

   function "*" (Left : in DC_Array_DCV_Type; Right : in Rate_Transform_Type) return DC_Array_DCV_Type
   is
      use DC_Array_DCV_Ops;
   begin
      return Left * To_DC_Array_DCV (Right);
   end "*";

   function "*" (Left, Right : in Wrench_Transform_Type) return Wrench_Transform_Type
   is
      use Math_DOF_3.Cart_Vector_Ops, Math_DOF_3.CACV_Ops;
   begin
      return (Left.Rot * Right.Rot, Left.Rot_Cross * Right.Rot + Left.Rot * Right.Rot_Cross);
   end "*";

   function "*" (Left : in Wrench_Transform_Type; Right : in DC_Array_DCV_Type) return DC_Array_DCV_Type
   is
      use DC_Array_DCV_Ops;
   begin
      return To_DC_Array_DCV (Left) * Right;
   end "*";

   function "*" (Left : in DC_Array_DCV_Type; Right : in Wrench_Transform_Type) return DC_Array_DCV_Type
   is
      use DC_Array_DCV_Ops;
   begin
      return Left * To_DC_Array_DCV (Right);
   end "*";

   function "*" (Left : in Rate_Transform_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3.Cart_Vector_Ops, Math_DOF_3.CACV_Ops;
   begin
      return (Left.Rot * Translation (Right) + Left.Rot_Cross * Rotation (Right)) & Left.Rot * Rotation (Right);
   end "*";

   function Transform_Rate (Xform : in Pose_Type; Rate : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, Math_DOF_3_Wertz;
   begin
      return Times_Inverse (Translation (Rate) - Cross (Xform.Translation, Rotation (Rate)), Xform.Rotation) &
         Times_Inverse (Rotation (Rate), Xform.Rotation);
   end Transform_Rate;

   function "*" (Left : in Wrench_Transform_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, Math_DOF_3.CACV_Ops;
   begin
      return Left.Rot * Translation (Right) & (Left.Rot * Rotation (Right) + Left.Rot_Cross * Translation (Right));
   end "*";

   function Transform_Wrench (Xform : in Pose_Type; Wrench : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, Math_DOF_3_Wertz;
   begin
      return Times_Inverse (Translation (Wrench), Xform.Rotation) &
         Times_Inverse (Rotation (Wrench) - Cross (Xform.Translation, Translation (Wrench)), Xform.Rotation);
   end Transform_Wrench;

   ----------
   --  Mass properties

   function "*" (Mass : in Mass_Type; Current_Pose_New : in Pose_Type) return Mass_Type
   is
      use Math_DOF_3, Math_DOF_3_Wertz;
      Result : Mass_Type;
   begin
      --  Can't use an aggregate, because the last component is a
      --  function of the first three.
      Result.Total          := Mass.Total;
      Result.Center         := Mass.Center * Current_Pose_New;
      Result.Center_Inertia := Mass.Center_Inertia * Current_Pose_New.Rotation;
      Result.Inertia        := Parallel_Axis (Result.Total, Result.Center, Result.Center_Inertia);
      return Result;
   end "*";

   function Add
     (Base            : in Mass_Type;
      Child           : in Mass_Type;
      Child_Pose_Base : in Pose_Type)
     return Mass_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, Math_DOF_3_Wertz;

      Center_Child_Tran_Base : constant Cart_Vector_Type := Child.Center * Child_Pose_Base;
      Result                 : Mass_Type;
   begin
      Result.Total := Base.Total + Child.Total;

      if Result.Total < Real_Type'Small then
         return Zero_Mass;

      else
         Result.Center := ((Base.Center * Base.Total) + (Center_Child_Tran_Base  * Child.Total)) / Result.Total;

         Result.Center_Inertia :=
            Parallel_Axis (Base.Total, Result.Center - Base.Center, Base.Center_Inertia) +
            Parallel_Axis (Child.Total, Result.Center - Center_Child_Tran_Base,
                           Child.Center_Inertia * Child_Pose_Base.Rotation);

         Result.Inertia := Parallel_Axis (Result.Total, Result.Center, Result.Center_Inertia);

         return Result;
      end if;
   end Add;

   function Add
     (Base            : in Mass_Type;
      Child           : in Mass_Type;
      Child_Tran_Base : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;

      Center_Child_Tran_Base : constant Cart_Vector_Type := Child.Center + Child_Tran_Base;
      Result              : Mass_Type;
   begin
      Result.Total := Base.Total + Child.Total;

      if Result.Total < Real_Type'Small then
         return Zero_Mass;

      else
         Result.Center := ((Base.Center * Base.Total) + (Center_Child_Tran_Base * Child.Total)) / Result.Total;
         Result.Center_Inertia :=
            Parallel_Axis (Base.Total, Result.Center - Base.Center, Base.Center_Inertia) +
            Parallel_Axis (Child.Total, Result.Center - Center_Child_Tran_Base, Child.Center_Inertia);

         Result.Inertia := Parallel_Axis (Result.Total, Result.Center, Result.Center_Inertia);

         return Result;
      end if;
   end Add;

   function Add
     (Base            : in Mass_Type;
      Child           : in CM_Mass_Type;
      Child_Tran_Base : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;

      Center_Child_Tran_Base : Cart_Vector_Type renames Child_Tran_Base;
      Result              : Mass_Type;
   begin
      Result.Total := Base.Total + Child.Total;

      if Result.Total < Real_Type'Small then
         return Zero_Mass;

      else
         Result.Center := ((Base.Center * Base.Total) + (Center_Child_Tran_Base * Child.Total)) / Result.Total;
         Result.Center_Inertia :=
            Parallel_Axis (Base.Total, Result.Center - Base.Center, Base.Center_Inertia) +
            Parallel_Axis (Child.Total, Result.Center - Center_Child_Tran_Base, Child.Center_Inertia);

         Result.Inertia := Parallel_Axis (Result.Total, Result.Center, Result.Center_Inertia);

         return Result;
      end if;
   end Add;

   function Subtract
     (Base            : in Mass_Type;
      Child           : in Mass_Type;
      Child_Pose_Base : in Pose_Type)
     return Mass_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops, Math_DOF_3_Wertz;
      Center_Child_Tran_Base : constant Cart_Vector_Type := Child.Center * Child_Pose_Base;
      Result              : Mass_Type;
   begin
      Result.Total := Base.Total - Child.Total;

      if Result.Total < Real_Type'Small then
         return Zero_Mass;

      else
         Result.Center := ((Base.Center * Base.Total) - (Center_Child_Tran_Base * Child.Total)) / Result.Total;
         Result.Center_Inertia :=
            Parallel_Axis (Base.Total, Result.Center - Base.Center, Base.Center_Inertia) -
            Parallel_Axis (Child.Total, Result.Center - Center_Child_Tran_Base,
                           Child.Center_Inertia * Child_Pose_Base.Rotation);
         Result.Inertia := Parallel_Axis (Result.Total, Result.Center, Result.Center_Inertia);

         return Result;
      end if;
   end Subtract;

   function Subtract
     (Base            : in Mass_Type;
      Child           : in Mass_Type;
      Child_Tran_Base : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
      Center_Child_Tran_Base : constant Cart_Vector_Type := Child.Center + Child_Tran_Base;
      Result              : Mass_Type;
   begin
      Result.Total := Base.Total - Child.Total;

      if Result.Total < Real_Type'Small then
         return Zero_Mass;

      else
         Result.Center := ((Base.Center * Base.Total) - (Center_Child_Tran_Base * Child.Total)) / Result.Total;
         Result.Center_Inertia :=
            Parallel_Axis (Base.Total, Result.Center - Base.Center, Base.Center_Inertia) -
            Parallel_Axis (Child.Total, Result.Center - Center_Child_Tran_Base, Child.Center_Inertia);
         Result.Inertia := Parallel_Axis (Result.Total, Result.Center, Result.Center_Inertia);

         return Result;
      end if;
   end Subtract;

   function Subtract
     (Base            : in Mass_Type;
      Child           : in CM_Mass_Type;
      Child_Tran_Base : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
      Center_Child_Tran_Base : Cart_Vector_Type renames Child_Tran_Base;
      Result              : Mass_Type;
   begin
      Result.Total := Base.Total - Child.Total;

      if Result.Total < Real_Type'Small then
         return Zero_Mass;

      else
         Result.Center := ((Base.Center * Base.Total) - (Center_Child_Tran_Base * Child.Total)) / Result.Total;
         Result.Center_Inertia :=
            Parallel_Axis (Base.Total, Result.Center - Base.Center, Base.Center_Inertia) -
            Parallel_Axis (Child.Total, Result.Center - Center_Child_Tran_Base, Child.Center_Inertia);
         Result.Inertia := Parallel_Axis (Result.Total, Result.Center, Result.Center_Inertia);

         return Result;
      end if;
   end Subtract;

end SAL.Gen_Math.Gen_DOF_6.Gen_Wertz;
