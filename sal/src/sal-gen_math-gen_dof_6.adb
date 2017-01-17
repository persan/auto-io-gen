--  Abstract:
--
--  see spec
--
--  Copyright (C) 2001 - 2003, 2005, 2006 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_DOF_6 is

   ----------
   --  Public subprograms

   --  Dual_Float_Type operations

   function "<=" (Left, Right : in Dual_Real_Type) return Boolean
   is begin
      return Left (Tran) <= Right (Tran) and Left (Rot) <= Right (Rot);
   end "<=";

   ----------
   --  Dual_Cart_Vector_Type operations

   function Translation (Item : in Dual_Cart_Vector_Type) return Math_DOF_3.Cart_Vector_Type
   is
   begin
      return (Item (TX), Item (TY), Item (TZ));
   end Translation;

   function Rotation (Item : in Dual_Cart_Vector_Type) return Math_DOF_3.Cart_Vector_Type
   is
   begin
      return (Item (RX), Item (RY), Item (RZ));
   end Rotation;

   function Concat (Translation, Rotation : in Math_DOF_3.Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3;
   begin
      return (TX => Translation (X),
              TY => Translation (Y),
              TZ => Translation (Z),
              RX => Rotation (X),
              RY => Rotation (Y),
              RZ => Rotation (Z));
   end Concat;

   function Mag (Item : in Dual_Cart_Vector_Type) return Dual_Real_Type
   is begin
      return (Elementary.Sqrt (Item (TX) * Item (TX) + Item (TY) * Item (TY) + Item (TZ) * Item (TZ)),
              Elementary.Sqrt (Item (RX) * Item (RX) + Item (RY) * Item (RY) + Item (RZ) * Item (RZ)));
   end Mag;

   function "*" (Left : in Dual_Real_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is begin
      return (Left (Tran) * Right (TX), Left (Tran) * Right (TY), Left (Tran) * Right (TZ),
              Left (Rot) * Right (RX), Left (Rot) * Right (RY), Left (Rot) * Right (RZ));
   end "*";

   function "*" (Left : in Dual_Cart_Vector_Type; Right : in Dual_Real_Type) return Dual_Cart_Vector_Type
   is begin
      return (Left (TX) * Right (Tran), Left (TY) * Right (Tran), Left (TZ) * Right (Tran),
              Left (RX) * Right (Rot), Left (RY) * Right (Rot), Left (RZ) * Right (Rot));
   end "*";

   function "/" (Left : in Dual_Cart_Vector_Type; Right : in Dual_Real_Type) return Dual_Cart_Vector_Type
   is begin
      return (Left (TX) / Right (Tran), Left (TY) / Right (Tran), Left (TZ) / Right (Tran),
              Left (RX) / Right (Rot), Left (RY) / Right (Rot), Left (RZ) / Right (Rot));
   end "/";

   ----------
   --  Pose_Type operations

   function Mag (Item : in Pose_Type) return Dual_Real_Type
   is
      use Math_DOF_3;
   begin
      return (Mag (Item.Translation), Mag (Item.Rotation));
   end Mag;

   ----------
   --  Dual magnitude and axis

   function Mag (Item : in Dual_Mag_Axis_Type) return Dual_Real_Type
   is
   begin
      return (Item.Translation.Mag, Item.Rotation.Mag);
   end Mag;

   function "-" (Item : in Dual_Mag_Axis_Type) return Dual_Mag_Axis_Type
   is begin
      return ((-Item.Translation.Mag, Item.Translation.Axis), (-Item.Rotation.Mag, Item.Rotation.Axis));
   end "-";

   function "*" (Left : in Dual_Real_Type; Right : in Dual_Mag_Axis_Type) return Dual_Mag_Axis_Type
   is
      use Math_DOF_3;
   begin
      return (Left (Tran) * Right.Translation, Left (Rot) * Right.Rotation);
   end "*";

   function "*" (Left : in Dual_Mag_Axis_Type; Right : in Dual_Real_Type) return Dual_Mag_Axis_Type
   is
      use Math_DOF_3;
   begin
      return (Left.Translation * Right (Tran), Left.Rotation * Right (Rot));
   end "*";

   function "/" (Left : in Dual_Mag_Axis_Type; Right : in Dual_Real_Type) return Dual_Mag_Axis_Type
   is
      use Math_DOF_3;
   begin
      return (Left.Translation / Right (Tran), Left.Rotation / Right (Rot));
   end "/";

   function "*" (Left : in Real_Type; Right : in Dual_Mag_Axis_Type) return Dual_Mag_Axis_Type
   is begin
      return ((Left * Right.Translation.Mag, Right.Translation.Axis),
              (Left * Right.Rotation.Mag, Right.Rotation.Axis));
   end "*";

   function "*" (Left : in Dual_Mag_Axis_Type; Right : in Real_Type) return Dual_Mag_Axis_Type
   is begin
      return ((Left.Translation.Mag * Right, Left.Translation.Axis), (Left.Rotation.Mag * Right, Left.Rotation.Axis));
   end "*";

   function "/" (Left : in Dual_Mag_Axis_Type; Right : in Real_Type) return Dual_Mag_Axis_Type
   is begin
      return ((Left.Translation.Mag / Right, Left.Translation.Axis), (Left.Rotation.Mag / Right, Left.Rotation.Axis));
   end "/";

   function To_Dual_Mag_Axis (Dual_Cart_Vector : in Dual_Cart_Vector_Type) return Dual_Mag_Axis_Type
   is
      use Math_DOF_3;
   begin
      return (To_Mag_Axis (Translation (Dual_Cart_Vector)),
              To_Mag_Axis (Rotation (Dual_Cart_Vector)));
   end To_Dual_Mag_Axis;

   function To_Dual_Cart_Vector (Dual_Mag_Axis : in Dual_Mag_Axis_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3;
   begin
      return To_Cart_Vector (Dual_Mag_Axis.Translation) & To_Cart_Vector (Dual_Mag_Axis.Rotation);
   end To_Dual_Cart_Vector;

   ----------
   --  Wrench and rate transforms

   function Unchecked_Rate_Transform (Rot, Rot_Cross : Math_DOF_3.Cart_Array_Cart_Vector_Type)
      return Rate_Transform_Type
   is
   begin
      return (Rot, Rot_Cross);
   end Unchecked_Rate_Transform;

   function Unchecked_Wrench_Transform (Rot, Rot_Cross : Math_DOF_3.Cart_Array_Cart_Vector_Type)
      return Wrench_Transform_Type
   is
   begin
      return (Rot, Rot_Cross);
   end Unchecked_Wrench_Transform;

   function To_Wrench_Transform (Right : in Rate_Transform_Type) return Wrench_Transform_Type
   is begin
      return (Right.Rot, Right.Rot_Cross);
   end To_Wrench_Transform;

   function To_Rate_Transform (Right : in Wrench_Transform_Type) return Rate_Transform_Type
   is begin
      return (Right.Rot, Right.Rot_Cross);
   end To_Rate_Transform;

   function Transform_Wrench
      (Disp   : in Math_DOF_3.Cart_Vector_Type;
       Wrench : in Dual_Cart_Vector_Type)
      return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Translation (Wrench) & (Rotation (Wrench) - Cross (Disp, Translation (Wrench)));
   end Transform_Wrench;

   function Transform_Rate
      (Disp : in Math_DOF_3.Cart_Vector_Type;
       Rate : in Dual_Cart_Vector_Type)
      return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Translation (Rate) - Cross (Disp, Rotation (Rate)) & Rotation (Rate);
   end Transform_Rate;

   function Transform_Force
      (Disp  : in Math_DOF_3.Cart_Vector_Type;
       Force : in Math_DOF_3.Cart_Vector_Type)
      return Dual_Cart_Vector_Type
   is
      use Math_DOF_3;
   begin
      return Force & Cross (Force, Disp);
   end Transform_Force;

   ----------
   --  Mass properties

   function Total (Item : in Mass_Type) return Real_Type
   is
   begin
      return Item.Total;
   end Total;

   function Center (Item : in Mass_Type) return Math_DOF_3.Cart_Vector_Type
   is
   begin
      return Item.Center;
   end Center;

   function Center_Inertia (Item : in Mass_Type) return Math_DOF_3.Inertia_Type
   is
   begin
      return Item.Center_Inertia;
   end Center_Inertia;

   function Inertia (Item : in Mass_Type) return Math_DOF_3.Inertia_Type
   is
   begin
      return Item.Inertia;
   end Inertia;

   function To_Mass
      (Total          : in Real_Type;
       Center         : in Math_DOF_3.Cart_Vector_Type;
       Center_Inertia : in Math_DOF_3.Inertia_Type)
      return Mass_Type
   is begin
      if Total < Real_Type'Small then
         --  Preserve Center for table lookups using DOF_3.Interpolate (center).
         return (0.0, Center, (others => 0.0), (others => 0.0));
      else
         return (Total, Center, Center_Inertia, Math_DOF_3.Parallel_Axis (Total, Center, Center_Inertia));
      end if;
   end To_Mass;

   function "*" (Left : in Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
      --  force = mass * acceleration of cm, torque = I * alpha, at object frame.
      --  momentum = m * v_cm, I * w
      --  v_cm = v + w x r
   begin
      return Left.Total * (Translation (Right) + Cross (Rotation (Right), Left.Center)) &
         Left.Inertia * Rotation (Right);
   end "*";

   function Inverse_Times (Left : in Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
      --  momentum = P, L
      --  velocity = v_cm - w x r, w
      --  w = I^-1 * L
      --  v_cm = P / m
      Rotation_Result : constant Cart_Vector_Type := Inverse (Left.Inertia) * Rotation (Right);
   begin
      return Translation (Right) / Left.Total - Cross (Rotation_Result, Left.Center) & Rotation_Result;
   end Inverse_Times;

   ----------
   --  Simple mass properties

   function To_CM_Mass (Item : in Mass_Type) return CM_Mass_Type
   is begin
      return (Total => Item.Total, Center_Inertia => Item.Center_Inertia);
   end To_CM_Mass;

   function To_Mass
     (CM_Mass      : in CM_Mass_Type;
      CM_Tran_Body : in Math_DOF_3.Cart_Vector_Type)
     return Mass_Type
   is begin
      return To_Mass (CM_Mass.Total, CM_Tran_Body, CM_Mass.Center_Inertia);
   end To_Mass;

   function Inverse (Item : in CM_Mass_Type) return CM_Inverse_Mass_Type
   is begin
      return
        (Inverse_Total => 1.0 / Item.Total,
         Inverse_Center_Inertia => Math_DOF_3.Inverse (Item.Center_Inertia));
   end Inverse;

   function "*" (Left : in CM_Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Left.Total * Translation (Right) & Left.Center_Inertia * Rotation (Right);
   end "*";

   function Mass_Times_Tran
     (Left  : in CM_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Left.Total * Right;
   end Mass_Times_Tran;

   function Mass_Times_Rot
     (Left  : in CM_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Left.Center_Inertia * Right;
   end Mass_Times_Rot;

   function "*" (Left : in CM_Inverse_Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Left.Inverse_Total * Translation (Right) & Left.Inverse_Center_Inertia * Rotation (Right);
   end "*";

   function Inverse_Mass_Times_Tran
     (Left  : in CM_Inverse_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Left.Inverse_Total * Right;
   end Inverse_Mass_Times_Tran;

   function Inverse_Mass_Times_Rot
     (Left  : in CM_Inverse_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Left.Inverse_Center_Inertia * Right;
   end Inverse_Mass_Times_Rot;

   function Inverse_Times (Left : in CM_Mass_Type; Right : in Dual_Cart_Vector_Type) return Dual_Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Translation (Right) / Left.Total & Inverse (Left.Center_Inertia) * Rotation (Right);
   end Inverse_Times;

   function Inverse_Mass_Times_Tran
     (Left  : in CM_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Right / Left.Total;
   end Inverse_Mass_Times_Tran;

   function Inverse_Mass_Times_Rot
     (Left  : in CM_Mass_Type;
      Right : in Math_DOF_3.Cart_Vector_Type)
     return Math_DOF_3.Cart_Vector_Type
   is
      use Math_DOF_3, Math_DOF_3.Cart_Vector_Ops;
   begin
      return Inverse (Left.Center_Inertia) * Right;
   end Inverse_Mass_Times_Rot;

end SAL.Gen_Math.Gen_DOF_6;
