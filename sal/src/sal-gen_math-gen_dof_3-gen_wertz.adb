--  Abstract:
--
--  see spec
--
--  Copyright (C) 2005 - 2007 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_DOF_3.Gen_Wertz is

   ----------
   --  Quaternions

   function Mag_Axis_To_Unit_Quaternion (Mag_Axis : in Mag_Axis_Type) return Unit_Quaternion_Type
   is
      use Math_Scalar;
      Half     : constant Trig_Pair_Type   := Sin_Cos (Mag_Axis.Mag / 2.0);
      Sin_Axis : constant Cart_Vector_Type := (-Sin (Half)) * Mag_Axis.Axis;
   begin
      return (X => Sin_Axis (X), Y => Sin_Axis (Y), Z => Sin_Axis (Z), S => Cos (Half));
   end Mag_Axis_To_Unit_Quaternion;

   function Unit_Quaternion_To_Mag_Axis (Quaternion : in Unit_Quaternion_Type) return Mag_Axis_Type
   is
      use Math_Scalar;
      Sin_Half : constant Real_Type := Mag (Cart_Vector_Type'(Quaternion.X, Quaternion.Y, Quaternion.Z));
      Temp     : Unit_Quaternion_Type;
   begin
      if Sin_Half > 0.0 then
         if Quaternion.S < 0.0 then
            Temp := (X => -Quaternion.X, Y => -Quaternion.Y, Z => -Quaternion.Z, S => -Quaternion.S);
         else
            Temp := Quaternion;
         end if;

         return
            (Mag  => Atan2 (Double_Trig (Unchecked_Trig_Pair (Sin_Half, Temp.S))),
             Axis => -Unit_Vector_Type'(Temp.X / Sin_Half, Temp.Y / Sin_Half, Temp.Z / Sin_Half));
      else
         return (0.0, X_Unit);
      end if;
   end Unit_Quaternion_To_Mag_Axis;

   function Rot_Vector_To_Unit_Quaternion (Rot_Vector : in Cart_Vector_Type) return Unit_Quaternion_Type
   is
      --  Ensure Angle < Pi. Find Angle_Axis. Apply [1] 2.3.1-15.
      use Math_Scalar;
      Angle     : constant Real_Type := Mag (Rot_Vector);
      Half_Trig : Trig_Pair_Type     := Sin_Cos (Angle / 2.0);
      Temp      : Real_Type;
   begin
      if Angle > 0.0 then
         if Cos (Half_Trig) < 0.0 then
            --  Angle > pi
            Half_Trig := (Unchecked_Trig_Pair (-Sin (Half_Trig), -Cos (Half_Trig)));
         end if;

         Temp := -Sin (Half_Trig) / Angle;

         return (X => Temp * Rot_Vector (X),
                 Y => Temp * Rot_Vector (Y),
                 Z => Temp * Rot_Vector (Z),
                 S => Cos (Half_Trig));
      else
         --  Angle = 0.0
         return (X | Y | Z => 0.0, S => 1.0);
      end if;
   end Rot_Vector_To_Unit_Quaternion;

   function Unit_Quaternion_To_Rot_Vector (Quaternion : in Unit_Quaternion_Type) return Cart_Vector_Type
   is
      --  [1] 2.3.1-21, followed by theta * n
      use Math_Scalar, Cart_Vector_Ops;
      Sin_Half : constant Real_Type := Mag (Cart_Vector_Type'(Quaternion.X, Quaternion.Y, Quaternion.Z));
      Temp     : Unit_Quaternion_Type;
      Angle    : Real_Type;
   begin
      if Sin_Half > 0.0 then
         if Quaternion.S < 0.0 then
            Temp := (X => -Quaternion.X, Y => -Quaternion.Y, Z => -Quaternion.Z, S => -Quaternion.S);
         else
            Temp := Quaternion;
         end if;

         Angle := 2.0 * Atan2 (Unchecked_Trig_Pair (Sin_Half, Temp.S));

         return (-Angle / Sin_Half) * Cart_Vector_Type'(Temp.X, Temp.Y, Temp.Z);

      else
         return (0.0, 0.0, 0.0);
      end if;
   end Unit_Quaternion_To_Rot_Vector;

   function Light_Vector_Rotation
     (Target_Unit_Inertial : in Unit_Vector_Type;
      Sc_Vel_Inertial      : in Cart_Vector_Type)
     return Unit_Quaternion_Type
   is
      use Cart_Vector_Ops;
   begin
      return Rot_Vector_To_Unit_Quaternion (Cross (Target_Unit_Inertial, Sc_Vel_Inertial) / Speed_Of_Light);
   end Light_Vector_Rotation;

   function To_Unit_Quaternion (Angle : in Real_Type; Axis : in Cart_Axis_Type) return Unit_Quaternion_Type
   is
      --  Mag_Axis_To_Unit_Quaternion, optimized for zeros.
      use Math_Scalar;
      Half_Trig : constant Trig_Pair_Type := Sin_Cos (-Angle / 2.0);
   begin
      case Axis is
      when X =>
         return (X => Sin (Half_Trig), Y => 0.0, Z => 0.0, S => Cos (Half_Trig));
      when Y =>
         return (X => 0.0, Y => Sin (Half_Trig), Z => 0.0, S => Cos (Half_Trig));
      when Z =>
         return (X => 0.0, Y => 0.0, Z => Sin (Half_Trig), S => Cos (Half_Trig));
      end case;
   end To_Unit_Quaternion;

   function X_Axis (Quat : in Unit_Quaternion_Type) return Unit_Vector_Type
   is
      --  Unit_Quaternion_To_Rot_Matrix, pick X
      Q : constant Unit_Quaternion_Type := (-Quat.X, -Quat.Y, -Quat.Z, Quat.S);
   begin
      return
        (X => 1.0 - 2.0 * (Q.Y * Q.Y + Q.Z * Q.Z),
         Y => 2.0 * (Q.X * Q.Y - Q.S * Q.Z),
         Z => 2.0 * (Q.X * Q.Z + Q.S * Q.Y));
   end X_Axis;

   function Y_Axis (Quat : in Unit_Quaternion_Type) return Unit_Vector_Type
   is
      --  Unit_Quaternion_To_Rot_Matrix, pick Y
      Q : constant Unit_Quaternion_Type := (-Quat.X, -Quat.Y, -Quat.Z, Quat.S);
   begin
      return
        (X => 2.0 * (Q.S * Q.Z + Q.X * Q.Y),
         Y => 1.0 - 2.0 * (Q.Z * Q.Z + Q.X * Q.X),
         Z => 2.0 * (Q.Y * Q.Z - Q.S * Q.X));
   end Y_Axis;

   function Z_Axis (Quat : in Unit_Quaternion_Type) return Unit_Vector_Type
   is
      --  Unit_Quaternion_To_Rot_Matrix, pick Z
      Q : constant Unit_Quaternion_Type := (-Quat.X, -Quat.Y, -Quat.Z, Quat.S);
   begin
      return
        (X => 2.0 * (Q.X * Q.Z - Q.S * Q.Y),
         Y => 2.0 * (Q.Y * Q.Z + Q.S * Q.X),
         Z => 1.0 - 2.0 * (Q.Y * Q.Y + Q.X * Q.X));
   end Z_Axis;

   function "*" (Left : in Cart_Vector_Type; Right : in Unit_Quaternion_Type) return Cart_Vector_Type
   is
      A : constant Cart_Vector_Type :=
         (-Right.Y * Left (Z) + Right.Z * Left (Y),
          -Right.Z * Left (X) + Right.X * Left (Z),
          -Right.X * Left (Y) + Right.Y * Left (X));
   begin
      return
         (Left (X) + 2.0 * (Right.S * A (X) - Right.Y * A (Z) + Right.Z * A (Y)),
          Left (Y) + 2.0 * (Right.S * A (Y) - Right.Z * A (X) + Right.X * A (Z)),
          Left (Z) + 2.0 * (Right.S * A (Z) - Right.X * A (Y) + Right.Y * A (X)));
   end "*";

   function Times_Inverse (Left : in Cart_Vector_Type; Right : in Unit_Quaternion_Type) return Cart_Vector_Type
   is
      --  Same as "*", but with Right.x, y, z negated.
      A : constant Cart_Vector_Type :=
         (Right.Y * Left (Z) - Right.Z * Left (Y),
          Right.Z * Left (X) - Right.X * Left (Z),
          Right.X * Left (Y) - Right.Y * Left (X));
   begin
      return
         (Left (X) + 2.0 * (Right.S * A (X) + Right.Y * A (Z) - Right.Z * A (Y)),
          Left (Y) + 2.0 * (Right.S * A (Y) + Right.Z * A (X) - Right.X * A (Z)),
          Left (Z) + 2.0 * (Right.S * A (Z) + Right.X * A (Y) - Right.Y * A (X)));
   end Times_Inverse;

   function "*" (Left : in Unit_Vector_Type; Right : in Unit_Quaternion_Type) return Unit_Vector_Type
   is
      A : constant Cart_Vector_Type :=
         (-Right.Y * Left (Z) + Right.Z * Left (Y),
          -Right.Z * Left (X) + Right.X * Left (Z),
          -Right.X * Left (Y) + Right.Y * Left (X));
   begin
      return
         (Left (X) + 2.0 * (Right.S * A (X) - Right.Y * A (Z) + Right.Z * A (Y)),
          Left (Y) + 2.0 * (Right.S * A (Y) - Right.Z * A (X) + Right.X * A (Z)),
          Left (Z) + 2.0 * (Right.S * A (Z) - Right.X * A (Y) + Right.Y * A (X)));
   end "*";

   function Times_Inverse (Left : in Unit_Vector_Type; Right : in Unit_Quaternion_Type) return Unit_Vector_Type
   is
      --  Same as "*", but with Right.x, y, z negated.
      A : constant Cart_Vector_Type :=
         (Right.Y * Left (Z) - Right.Z * Left (Y),
          Right.Z * Left (X) - Right.X * Left (Z),
          Right.X * Left (Y) - Right.Y * Left (X));
   begin
      return
         (Left (X) + 2.0 * (Right.S * A (X) + Right.Y * A (Z) - Right.Z * A (Y)),
          Left (Y) + 2.0 * (Right.S * A (Y) + Right.Z * A (X) - Right.X * A (Z)),
          Left (Z) + 2.0 * (Right.S * A (Z) + Right.X * A (Y) - Right.Y * A (X)));
   end Times_Inverse;

   function To_Unit_Quaternion (N_1, N_2 : in Unit_Vector_Type) return Unit_Quaternion_Type
   is
      use Math_Scalar;
      use Cart_Vector_Ops;
      Sn   : constant Cart_Vector_Type := Cross (N_1, N_2);
      S    : constant Real_Type        := Mag (Sn);
      C    : constant Real_Type        := N_1 * N_2;
      Half : constant Trig_Pair_Type   := Half_Trig (Unchecked_Trig_Pair (S, C));
      Temp : Cart_Vector_Type;
   begin
      if S = 0.0 then
         return Zero_Unit_Quaternion;

      else
         Temp := -(Sin (Half) / S) * Sn;
         return (Temp (X), Temp (Y), Temp (Z), Cos (Half));
      end if;

   end To_Unit_Quaternion;

   ----------
   --  ZYX_EULER operations

   function To_ZYX_Euler (Quaternion : in Unit_Quaternion_Type) return ZYX_Euler_Type
   is
      use Math_Scalar;

      Q : Unit_Quaternion_Type;

      Half_Sum_ZX,
      Half_Diff_ZX,
      Theta_Z,
      Theta_Y,
      Theta_X : Real_Type;

      Trig_Half_Sum_ZX,
      Trig_Half_Diff_ZX,
      Half_Trig_Z,
      Half_Trig_X : Trig_Pair_Type;

   begin
      --  Invert the quaternion in this step
      if Quaternion.S < 0.0 then
         Q := (X => Quaternion.X, Y => Quaternion.Y, Z => Quaternion.Z, S => -Quaternion.S);
      else
         Q := (X => -Quaternion.X, Y => -Quaternion.Y, Z => -Quaternion.Z, S => Quaternion.S);
      end if;

      if (Q.S + Q.Y) ** 2 + (Q.X - Q.Z) ** 2 < 3.0 * First_Order_Trig  or
         (Q.S - Q.Y) ** 2 + (Q.X + Q.Z) ** 2 < 3.0 * First_Order_Trig
      then
         --  at the Euler angle singularity
         Theta_Y := Pi / 2.0;
         Theta_Z := 0.0;
         Theta_X := 2.0 * Atan2 (To_Trig_Pair (Q.X, Q.S));
      else
         Trig_Half_Sum_ZX := To_Trig_Pair (Q.Z + Q.X, Q.S - Q.Y);
         Trig_Half_Diff_ZX := To_Trig_Pair (Q.Z - Q.X, Q.S + Q.Y);

         Half_Sum_ZX := Atan2 (Trig_Half_Sum_ZX);
         Half_Diff_ZX := Atan2 (Trig_Half_Diff_ZX);

         Theta_Z := Half_Sum_ZX + Half_Diff_ZX;
         Theta_X := Half_Sum_ZX - Half_Diff_ZX;

         Half_Trig_Z := Half_Trig (Trig_Half_Sum_ZX + Trig_Half_Diff_ZX);
         Half_Trig_X := Half_Trig (Trig_Half_Sum_ZX - Trig_Half_Diff_ZX);

         Theta_Y := 2.0 * Atan2 (To_Trig_Pair
                                 (Cos (Half_Trig_Z) * Cos (Half_Trig_X) * Q.Y
                                  - Sin (Half_Trig_Z) * Cos (Half_Trig_X) * Q.X
                                  - Cos (Half_Trig_Z) * Sin (Half_Trig_X) * Q.Z
                                  + Sin (Half_Trig_Z) * Sin (Half_Trig_X) * Q.S,
                                  Cos (Half_Trig_Z) * Cos (Half_Trig_X) * Q.S
                                  + Sin (Half_Trig_Z) * Cos (Half_Trig_X) * Q.Z
                                  + Cos (Half_Trig_Z) * Sin (Half_Trig_X) * Q.X
                                  + Sin (Half_Trig_Z) * Sin (Half_Trig_X) * Q.Y));
      end if;
      return (Theta_Z, Theta_Y, Theta_X);
   end To_ZYX_Euler;

   function To_Unit_Quaternion (Euler : in ZYX_Euler_Type) return Unit_Quaternion_Type
   is begin
      return
        To_Unit_Quaternion (Euler.Theta_X, X) *
        To_Unit_Quaternion (Euler.Theta_Y, Y) *
        To_Unit_Quaternion (Euler.Theta_Z, Z);
   end To_Unit_Quaternion;

   ----------
   --  Rotation matrices

   function Unit_Quaternion_To_Rot_Matrix (Quaternion : in Unit_Quaternion_Type) return Rot_Matrix_Type
   is
      Q : constant Unit_Quaternion_Type := (-Quaternion.X, -Quaternion.Y, -Quaternion.Z, Quaternion.S);
   begin
      return
         (X =>
             (X => 1.0 - 2.0 * (Q.Z * Q.Z + Q.Y * Q.Y),
              Y => 2.0 * (Q.X * Q.Y - Q.S * Q.Z),
              Z => 2.0 * (Q.X * Q.Z + Q.S * Q.Y)),
          Y =>
             (X => 2.0 * (Q.S * Q.Z + Q.X * Q.Y),
              Y => 1.0 - 2.0 * (Q.Z * Q.Z + Q.X * Q.X),
              Z => 2.0 * (Q.Y * Q.Z - Q.S * Q.X)),
          Z =>
             (X => 2.0 * (Q.X * Q.Z - Q.S * Q.Y),
              Y => 2.0 * (Q.Y * Q.Z + Q.S * Q.X),
              Z => 1.0 - 2.0 * (Q.Y * Q.Y + Q.X * Q.X)));
   end Unit_Quaternion_To_Rot_Matrix;

   function Rot_Matrix_To_Unit_Quaternion (Rot_Matrix : in Rot_Matrix_Type) return Unit_Quaternion_Type
   is
      --  [1], 2.4.1-5, eqn:rot_mat_to_quat, quaternion inverted to right-multiply
      M : Rot_Matrix_Type renames Rot_Matrix;
      A : constant Real_Type :=  M (X)(X) + M (Y)(Y) + M (Z)(Z);
      B : constant Real_Type :=  M (X)(X) - M (Y)(Y) - M (Z)(Z);
      C : constant Real_Type := -M (X)(X) + M (Y)(Y) - M (Z)(Z);
      D : constant Real_Type := -M (X)(X) - M (Y)(Y) + M (Z)(Z);
      E : Real_Type;
   begin

      if A >= Real_Type'Max (B, Real_Type'Max (C, D)) then
         E := 2.0 * Elementary.Sqrt (1.0 + A);
         return
            (X => -(M (Z)(Y) - M (Y)(Z)) / E,
             Y => -(M (X)(Z) - M (Z)(X)) / E,
             Z => -(M (Y)(X) - M (X)(Y)) / E,
             S => E / 4.0);

      elsif B >= Real_Type'Max (A, Real_Type'Max (C, D)) then
         E := 2.0 * Elementary.Sqrt (1.0 + B);
         return
           (X => -E / 4.0,
            Y => -(M (X)(Y) + M (Y)(X)) / E,
            Z => -(M (X)(Z) + M (Z)(X)) / E,
            S => (M (Z)(Y) - M (Y)(Z)) / E);

      elsif C >= Real_Type'Max (A, Real_Type'Max (B, D)) then
         E := 2.0 * Elementary.Sqrt (1.0 + C);
         return
           (X => -(M (X)(Y) + M (Y)(X)) / E,
            Y => -E / 4.0,
            Z => -(M (Y)(Z) + M (Z)(Y)) / E,
            S => (M (X)(Z) - M (Z)(X)) / E);
      else
         E := 2.0 * Elementary.Sqrt (1.0 + D);
         return
           (X => -(M (X)(Z) + M (Z)(X)) / E,
            Y => -(M (Y)(Z) + M (Z)(Y)) / E,
            Z => -E / 4.0,
            S => (M (Y)(X) - M (X)(Y)) / E);
      end if;
   end Rot_Matrix_To_Unit_Quaternion;

   function Mag_Axis_To_Rot_Matrix (Mag_Axis : in Mag_Axis_Type) return Rot_Matrix_Type
   is
      use Math_Scalar;
      Trig : constant Trig_Pair_Type := Sin_Cos (Mag_Axis.Mag);
      Sin  : Real_Type renames Math_Scalar.Sin (Trig);
      Cos  : Real_Type renames Math_Scalar.Cos (Trig);
      Vers : constant Real_Type      := 1.0 - Cos;
      N    : Unit_Vector_Type renames Mag_Axis.Axis;
   begin
      return
         (X =>
             (X => Cos + N (X) * N (X) * Vers,
              Y => -N (Z) * Sin + N (X) * N (Y) * Vers,
              Z =>  N (Y) * Sin + N (X) * N (Z) * Vers),
          Y =>
             (X =>  N (Z) * Sin + N (X) * N (Y) * Vers,
              Y =>  Cos + N (Y) * N (Y) * Vers,
              Z => -N (X) * Sin + N (Y) * N (Z) * Vers),
          Z =>
             (X => -N (Y) * Sin + N (X) * N (Z) * Vers,
              Y =>  N (X) * Sin + N (Y) * N (Z) * Vers,
              Z => Cos + N (Z) * N (Z) * Vers));
   end Mag_Axis_To_Rot_Matrix;

   function Rot_Matrix_To_Mag_Axis (Rot_Matrix : in Rot_Matrix_Type) return Mag_Axis_Type
   is
      use Math_Scalar, Cart_Vector_Ops;
      Temp_Axis : constant Cart_Vector_Type :=
         (Rot_Matrix (Z)(Y) - Rot_Matrix (Y)(Z),
          Rot_Matrix (X)(Z) - Rot_Matrix (Z)(X),
          Rot_Matrix (Y)(X) - Rot_Matrix (X)(Y));
      Cos_Mag : constant Real_Type := (Rot_Matrix (X)(X) + Rot_Matrix (Y)(Y) + Rot_Matrix (Z)(Z) - 1.0) * 0.5;
      Sin_Mag : constant Real_Type := Mag (Temp_Axis) * 0.5;
   begin
      --  Note max (Temp_Axis) = 1.0, so division is only a problem if
      --  Sin_Mag is identically zero.
      if Sin_Mag > 0.0 then
         return
            (Mag  => Atan2 (Unchecked_Trig_Pair (Sin_Mag, Cos_Mag)),
             Axis => Unchecked_Unit_Vector (Temp_Axis / (2.0 * Sin_Mag)));

      else
         --  Magnitude is 0.0; pick arbitrary axis.
         return (0.0, X_Unit);
      end if;
   end Rot_Matrix_To_Mag_Axis;

   function Rot_Matrix_Times_Cart_Vector
      (Left  : in Rot_Matrix_Type;
       Right : in Cart_Vector_Type)
      return Cart_Vector_Type
   is begin
      return CACV_Ops."*" (Cart_Array_Cart_Vector_Type (Left), Right);
   end Rot_Matrix_Times_Cart_Vector;

   function Inverse_Times (Left : in Rot_Matrix_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type
   is begin
      return CACV_Ops.Transpose_Times (Cart_Array_Cart_Vector_Type (Left), Right);
   end Inverse_Times;

   ----------
   --  Inertias

   function Inertia_Times_Unit_Quat (Left : in Inertia_Type; Right : in Unit_Quaternion_Type) return Inertia_Type
   is begin
      return Rot_Matrix_Times_Inertia (To_Rot_Matrix (Right), Left);
   end Inertia_Times_Unit_Quat;

   function Rot_Matrix_Times_Inertia (Left : in Rot_Matrix_Type; Right : in Inertia_Type) return Inertia_Type
   is
      use CACV_Ops;
      Left_Matrix   : constant Cart_Array_Cart_Vector_Type := Cart_Array_Cart_Vector_Type (Left);
      Middle_Matrix : constant Cart_Array_Cart_Vector_Type := To_CACV (Right);
      Temp          : Cart_Array_Cart_Vector_Type;
   begin
      Temp := Times_Transpose (Left_Matrix * Middle_Matrix, Left_Matrix);
      return
         (Ixx => Temp (X) (X),
          Iyy => Temp (Y) (Y),
          Izz => Temp (Z) (Z),
          Ixy => Temp (X) (Y),
          Ixz => Temp (X) (Z),
          Iyz => Temp (Y) (Z));
   end Rot_Matrix_Times_Inertia;

end SAL.Gen_Math.Gen_DOF_3.Gen_Wertz;
