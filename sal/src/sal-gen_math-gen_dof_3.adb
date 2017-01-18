--  Abstract:
--
--  see spec
--
--  Copyright (C) 2001 - 2007, 2009 Stephen Leake.  All Rights Reserved.
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

pragma License (Modified_Gpl);

package body SAL.Gen_Math.Gen_DOF_3 is

   ----------
   --  more Cart_Vector operations

   function Mag (Item : in Cart_Vector_Type) return Real_Type
   is begin
      return Elementary.Sqrt (Item (X) * Item (X) + Item (Y) * Item (Y) + Item (Z) * Item (Z));
   end Mag;

   function Cross (Left, Right : in Cart_Vector_Type) return Cart_Vector_Type
   is begin
      return
         (Left (Y) * Right (Z) - Left (Z) * Right (Y),
          Left (Z) * Right (X) - Left (X) * Right (Z),
          Left (X) * Right (Y) - Left (Y) * Right (X));
   end Cross;

   function Rotate
      (Angle  : in Real_Type;
       Axis   : in Cart_Axis_Type;
       Vector : in Cart_Vector_Type)
      return Cart_Vector_Type
   is
      --  from [1], 2.3.1.3 and 2.3.1.8, optimized. See
      --  DERIVE_MATH_3_DOF.MAC for verification.
      use Math_Scalar;
      Trig_A : constant Trig_Pair_Type := Sin_Cos (Angle);
      Cos_A  : Real_Type renames Cos (Trig_A);
      Sin_A  : Real_Type renames Sin (Trig_A);
   begin
      case Axis is
      when X =>
         return (Vector (X),
                 Cos_A * Vector (Y) - Sin_A * Vector (Z),
                 Sin_A * Vector (Y) + Cos_A * Vector (Z));
      when Y =>
         return (Cos_A * Vector (X) + Sin_A * Vector (Z),
                 Vector (Y),
                 -Sin_A * Vector (X) + Cos_A * Vector (Z));
      when Z =>
         return (Cos_A * Vector (X) - Sin_A * Vector (Y),
                 Sin_A * Vector (X) + Cos_A * Vector (Y),
                 Vector (Z));
      end case;
   end Rotate;

   function Rotate
      (Sin_Cos : in Math_Scalar.Trig_Pair_Type;
       Axis    : in Cart_Axis_Type;
       Vector  : in Cart_Vector_Type)
      return Cart_Vector_Type
   is
      use Math_Scalar;
   begin
      case Axis is
      when X =>
         return (Vector (X),
                 Cos (Sin_Cos) * Vector (Y) - Sin (Sin_Cos) * Vector (Z),
                 Sin (Sin_Cos) * Vector (Y) + Cos (Sin_Cos) * Vector (Z));

      when Y =>
         return (Cos (Sin_Cos) * Vector (X) + Sin (Sin_Cos) * Vector (Z),
                 Vector (Y),
                 -Sin (Sin_Cos) * Vector (X) + Cos (Sin_Cos) * Vector (Z));

      when Z =>
         return (Cos (Sin_Cos) * Vector (X) - Sin (Sin_Cos) * Vector (Y),
                 Sin (Sin_Cos) * Vector (X) + Cos (Sin_Cos) * Vector (Y),
                 Vector (Z));

      end case;
   end Rotate;

   function Lorentz_Transform
     (Vel_Obj   : in Cart_Vector_Type;
      Vel_Frame : in Cart_Vector_Type)
     return Cart_Vector_Type
   is
      use Cart_Vector_Ops;
      Vel_Obj_Par : constant Cart_Vector_Type := Dot (Vel_Obj, Vel_Frame) * Vel_Frame /
        Dot (Vel_Frame, Vel_Frame);
      Vel_Perp    : constant Cart_Vector_Type := Vel_Obj - Vel_Obj_Par;
      Beta        : constant Real_Type        := Mag (Vel_Frame) / Speed_Of_Light;
      Gamma       : constant Real_Type        := Elementary.Sqrt (1.0 - Beta**2);
   begin
      return (Vel_Frame + Vel_Obj_Par + Gamma * Vel_Perp) / (1.0 + Dot (Vel_Obj, Vel_Frame) / Speed_Of_Light**2);
   end Lorentz_Transform;

   ----------
   --  Unit vectors

   function X (Item : in Unit_Vector_Type) return Real_Type
   is begin
      return Item (X);
   end X;

   function Y (Item : in Unit_Vector_Type) return Real_Type
   is begin
      return Item (Y);
   end Y;

   function Z (Item : in Unit_Vector_Type) return Real_Type
   is begin
      return Item (Z);
   end Z;

   function To_Unit_Vector (Item : in Cart_Vector_Type) return Unit_Vector_Type
   is
      Magnitude : constant Real_Type := Mag (Item);
   begin
      --  (x, y, z) / magnitude is guaranteed < 1.0, so we only need
      --  to check for precisely 0.0.
      if Magnitude > 0.0 then
         return (Item (X) / Magnitude, Item (Y) / Magnitude, Item (Z) / Magnitude);
      else
         raise Non_Normalizable_Unit_Vector;
      end if;
   end To_Unit_Vector;

   function To_Unit_Vector (X, Y, Z : in Real_Type) return Unit_Vector_Type
   is begin
      return To_Unit_Vector (Cart_Vector_Type'(X, Y, Z));
   end To_Unit_Vector;

   function To_Cart_Vector (Item : in Unit_Vector_Type) return Cart_Vector_Type
   is begin
      return (Item (X), Item (Y), Item (Z));
   end To_Cart_Vector;

   function Normalize (Item : in Unit_Vector_Type) return Unit_Vector_Type
   is begin
      return To_Unit_Vector (Cart_Vector_Type'(Item (X), Item (Y), Item (Z)));
   end Normalize;

   function Unchecked_Unit_Vector (Item : in Cart_Vector_Type) return Unit_Vector_Type
   is begin
      return (Item (X), Item (Y), Item (Z));
   end Unchecked_Unit_Vector;

   function Unchecked_Unit_Vector (X, Y, Z : in Real_Type) return Unit_Vector_Type
   is begin
      return (X, Y, Z);
   end Unchecked_Unit_Vector;

   function "-" (Item : in Unit_Vector_Type) return Unit_Vector_Type
   is begin
      return (-Item (X), -Item (Y), -Item (Z));
   end "-";

   function "-" (Left, Right : in Unit_Vector_Type) return Cart_Vector_Type
   is begin
      return (Left (X) - Right (X), Left (Y) - Right (Y), Left (Z) - Right (Z));
   end "-";

   function "+" (Left, Right : in Unit_Vector_Type) return Cart_Vector_Type
   is begin
      return (Left (X) + Right (X), Left (Y) + Right (Y), Left (Z) + Right (Z));
   end "+";

   function "*" (Left : in Unit_Vector_Type; Right : in Real_Type) return Cart_Vector_Type
   is begin
      return (Right * Left (X), Right * Left (Y), Right * Left (Z));
   end "*";

   function "*" (Left : in Real_Type; Right : in Unit_Vector_Type) return Cart_Vector_Type
   is begin
      return (Left * Right (X), Left * Right (Y), Left * Right (Z));
   end "*";

   function "/" (Left : in Unit_Vector_Type; Right : in Real_Type) return Cart_Vector_Type
   is begin
      return (Left (X) / Right, Left (Y) / Right, Left (Z) / Right);
   end "/";

   function Dot (Left : in Unit_Vector_Type; Right : in Unit_Vector_Type) return Real_Type
   is begin
      return Left (X) * Right (X) + Left (Y) * Right (Y) + Left (Z) * Right (Z);
   end Dot;

   function Dot (Left : in Unit_Vector_Type; Right : in Cart_Vector_Type) return Real_Type
   is begin
      return Left (X) * Right (X) + Left (Y) * Right (Y) + Left (Z) * Right (Z);
   end Dot;

   function Dot (Left : in Cart_Vector_Type; Right : in Unit_Vector_Type) return Real_Type
   is begin
      return Left (X) * Right (X) + Left (Y) * Right (Y) + Left (Z) * Right (Z);
   end Dot;

   function Cross (Left : in Unit_Vector_Type; Right : in Unit_Vector_Type) return Cart_Vector_Type
   is begin
      return
         (Left (Y) * Right (Z) - Left (Z) * Right (Y),
          Left (Z) * Right (X) - Left (X) * Right (Z),
          Left (X) * Right (Y) - Left (Y) * Right (X));
   end Cross;

   function Cross (Left : in Unit_Vector_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type
   is begin
      return
         (Left (Y) * Right (Z) - Left (Z) * Right (Y),
          Left (Z) * Right (X) - Left (X) * Right (Z),
          Left (X) * Right (Y) - Left (Y) * Right (X));
   end Cross;

   function Cross (Left : in Cart_Vector_Type; Right : in Unit_Vector_Type) return Cart_Vector_Type
   is begin
      return
         (Left (Y) * Right (Z) - Left (Z) * Right (Y),
          Left (Z) * Right (X) - Left (X) * Right (Z),
          Left (X) * Right (Y) - Left (Y) * Right (X));
   end Cross;

   function Rotate
      (Angle  : in Real_Type;
       Axis   : in Cart_Axis_Type;
       Vector : in Unit_Vector_Type)
      return Unit_Vector_Type
   is
      Result : constant Cart_Vector_Type := Rotate (Angle, Axis, To_Cart_Vector (Vector));
   begin
      return (Result (X), Result (Y), Result (Z));
   end Rotate;

   function Rotate
      (Sin_Cos : in Math_Scalar.Trig_Pair_Type;
       Axis    : in Cart_Axis_Type;
       Vector  : in Unit_Vector_Type)
      return Unit_Vector_Type
   is
      Result : constant Cart_Vector_Type := Rotate (Sin_Cos, Axis, To_Cart_Vector (Vector));
   begin
      return (Result (X), Result (Y), Result (Z));
   end Rotate;

   function Light_Vector_Transform
     (Unit_Sc_Star : in Unit_Vector_Type;
      Vel_Sci      : in Cart_Vector_Type)
     return Unit_Vector_Type
   is
      use Cart_Vector_Ops;
      Vel_Sci_C : constant Cart_Vector_Type := Vel_Sci / Speed_Of_Light;
      Result    : constant Cart_Vector_Type := To_Cart_Vector (Unit_Sc_Star) +
        Unit_Sc_Star * Dot (Unit_Sc_Star, Vel_Sci_C) - Vel_Sci_C;
   begin
      return To_Unit_Vector (Result);
   end Light_Vector_Transform;

   ----------
   --  Magnitude and axis

   function "-" (Item : in Mag_Axis_Type) return Mag_Axis_Type
   is begin
      return (-Item.Mag, Item.Axis);
   end "-";

   function "*" (Left : in Real_Type; Right : in Mag_Axis_Type) return Mag_Axis_Type
   is begin
      return (Left * Right.Mag, Right.Axis);
   end "*";

   function "*" (Left : in Mag_Axis_Type; Right : in Real_Type) return Mag_Axis_Type
   is begin
      return (Left.Mag * Right, Left.Axis);
   end "*";

   function "/" (Left : in Mag_Axis_Type; Right : in Real_Type) return Mag_Axis_Type
   is begin
      return (Left.Mag / Right, Left.Axis);
   end "/";

   function To_Cart_Vector (Mag_Axis : in Mag_Axis_Type) return Cart_Vector_Type
   is begin
      return Mag_Axis.Mag * Mag_Axis.Axis;
   end To_Cart_Vector;

   function To_Mag_Axis (Cart_Vector : in Cart_Vector_Type) return Mag_Axis_Type
   is
      Magnitude : constant Real_Type := Mag (Cart_Vector);
   begin
      --  (x, y, z) / magnitude is guaranteed < 1.0, so we only need
      --  to check for precisely 0.0
      if Magnitude > 0.0 then
         return (Magnitude,
                 (Cart_Vector (X) / Magnitude,
                  Cart_Vector (Y) / Magnitude,
                  Cart_Vector (Z) / Magnitude));
      else
         return (0.0, X_Unit);
      end if;
   end To_Mag_Axis;

   function To_Mag_Axis (Left, Right : in Unit_Vector_Type) return Mag_Axis_Type
   is
      use Math_Scalar;
      Temp_Cross     : constant Cart_Vector_Type := Cross (Left, Right);
      Mag_Temp_Cross : constant Real_Type        := Mag (Temp_Cross);
      Temp_Dot       : constant Real_Type        := Left * Right;
      Magnitude      : constant Real_Type        := Atan2 (Unchecked_Trig_Pair (Mag_Temp_Cross, Temp_Dot));
   begin
      --  Singular for magnitude = 0 or pi

      if Mag_Temp_Cross > 0.0 then
         return (Mag  => Magnitude,
                 Axis => To_Unit_Vector (Temp_Cross));
      elsif Magnitude > 0.0 then
         return (Magnitude, X_Unit);
      else
         return (0.0, X_Unit);
      end if;
   end To_Mag_Axis;

   function Rotate
     (Mag_Axis : in Mag_Axis_Type;
      Vector   : in Cart_Vector_Type)
     return Cart_Vector_Type
   is
      use Elementary;
      use Cart_Vector_Ops;
   begin
      return Vector * Cos (Mag_Axis.Mag) -
        Cross (Vector, Mag_Axis.Axis) * Sin (Mag_Axis.Mag) +
        Mag_Axis.Axis * Dot (Vector, Mag_Axis.Axis) * (1.0 - Cos (Mag_Axis.Mag));
   end Rotate;

   function To_Celestial (N : in Unit_Vector_Type) return Celestial_Coordinate_Type
   is
      --  See [2] section E.1. But first check for the singularity,
      --  use atan2, allow for round-off errors making the elements
      --  slightly greater than 1.0, and optimize for r = 1.0.
      use Math_Scalar, Elementary;
      Clipped_Z   : constant Real_Type := Real_Type'Max (-1.0, Real_Type'Min (1.0, N (Z)));
      Declination : constant Real_Type := Pi / 2.0 - Arccos (Clipped_Z);
      RA          : Real_Type;
   begin
      if N (X) = 0.0 and N (Y) = 0.0 then
         return
            (Right_Ascension => 0.0,
             Declination     => Declination);
      else
         RA := Atan2 (Unchecked_Trig_Pair (N (Y), N (X)));
         if RA < 0.0 then
            RA := RA + 2.0 * Pi;
         end if;
         return
            (Right_Ascension => RA,
             Declination     => Declination);
      end if;
   end To_Celestial;

   function To_Unit_Vector (Celestial : in Celestial_Coordinate_Type) return Unit_Vector_Type
   is
      use Math_Scalar;
      Trig_RA    : constant Trig_Pair_Type := Sin_Cos (Celestial.Right_Ascension);
      Trig_Theta : constant Trig_Pair_Type := Sin_Cos (Pi / 2.0 - Celestial.Declination);
   begin
      --  See [2] section E.1, optimize for r = 1.0.
      return
         (X => Sin (Trig_Theta) * Cos (Trig_RA),
          Y => Sin (Trig_Theta) * Sin (Trig_RA),
          Z => Cos (Trig_Theta));
   end To_Unit_Vector;

   ----------
   --  quaternions

   function X (Item : in Unit_Quaternion_Type) return Real_Type
   is begin
      return Item.X;
   end X;

   function Y (Item : in Unit_Quaternion_Type) return Real_Type
   is begin
      return Item.Y;
   end Y;

   function Z (Item : in Unit_Quaternion_Type) return Real_Type
   is begin
      return Item.Z;
   end Z;

   function S (Item : in Unit_Quaternion_Type) return Real_Type
   is begin
      return Item.S;
   end S;

   function To_Unit_Quaternion (Item : in Quaternion_Type) return Unit_Quaternion_Type
   is
      Mag : constant Real_Type := Elementary.Sqrt
        (Item (X) * Item (X) + Item (Y) * Item (Y) + Item (Z) * Item (Z) + Item (S) * Item (S));
   begin
      --  Element / Mag is guaranteed < 1.0, so we only have to worry
      --  about exactly 0.0.
      if Mag > 0.0 then
         return (X => Item (X) / Mag, Y => Item (Y) / Mag, Z => Item (Z) / Mag, S => Item (S) / Mag);
      else
         raise Non_Normalizable_Unit_Quaternion;
      end if;
   end To_Unit_Quaternion;

   function To_Unit_Quaternion (X, Y, Z, S : in Real_Type) return Unit_Quaternion_Type
   is
      Mag : constant Real_Type := Elementary.Sqrt (X * X + Y * Y + Z * Z + S * S);
   begin
      --  Element / Mag is guaranteed < 1.0, so we only have to worry
      --  about exactly 0.0.
      if Mag > 0.0 then
         return (X => X / Mag, Y => Y / Mag, Z => Z / Mag, S => S / Mag);
      else
         raise Non_Normalizable_Unit_Quaternion;
      end if;
   end To_Unit_Quaternion;

   function Unchecked_Unit_Quaternion (X, Y, Z, S : in Real_Type) return Unit_Quaternion_Type
   is begin
      return (X => X, Y => Y, Z => Z, S => S);
   end Unchecked_Unit_Quaternion;

   function Mag (Item : in Unit_Quaternion_Type) return Real_Type
   is
      --  [1], 2.3.1-7 eqn:quat_to_angle_axis_active
      use Math_Scalar;
      Result : Real_Type;
      Two_Pi : constant Real_Type := 2.0 * Pi;
   begin
      Result := 2.0 * Atan2 (Unchecked_Trig_Pair (Mag (Cart_Vector_Type'(Item.X, Item.Y, Item.Z)), Item.S));
      if Result > Pi then
         return Result - Two_Pi;
      elsif Result < -Pi then
         return Result + Two_Pi;
      else
         return Result;
      end if;
   end Mag;

   function Unit_Quaternion_Inverse (Item : in Unit_Quaternion_Type) return Unit_Quaternion_Type
   is begin
      --  [1], 2.3.1.6
      return (X => -Item.X, Y => -Item.Y, Z => -Item.Z, S => Item.S);
   end Unit_Quaternion_Inverse;

   function "*" (Left, Right : in Unit_Quaternion_Type) return Unit_Quaternion_Type
   is begin
      --  [1], 2.3.1.11, eqn:quat_times_quat
      return (X => Left.S * Right.X + Left.X * Right.S + Left.Y * Right.Z - Left.Z * Right.Y,
              Y => Left.S * Right.Y - Left.X * Right.Z + Left.Y * Right.S + Left.Z * Right.X,
              Z => Left.S * Right.Z + Left.X * Right.Y - Left.Y * Right.X + Left.Z * Right.S,
              S => Left.S * Right.S - Left.X * Right.X - Left.Y * Right.Y - Left.Z * Right.Z);
   end "*";

   function Inverse_Times (Left, Right : in Unit_Quaternion_Type) return Unit_Quaternion_Type
   is begin
      --  Same as "*", but with left.x, y, z negated
      return (X => Left.S * Right.X - Left.X * Right.S - Left.Y * Right.Z + Left.Z * Right.Y,
              Y => Left.S * Right.Y + Left.X * Right.Z - Left.Y * Right.S - Left.Z * Right.X,
              Z => Left.S * Right.Z - Left.X * Right.Y + Left.Y * Right.X - Left.Z * Right.S,
              S => Left.S * Right.S + Left.X * Right.X + Left.Y * Right.Y + Left.Z * Right.Z);
   end Inverse_Times;

   function Times_Inverse (Left, Right : in Unit_Quaternion_Type) return Unit_Quaternion_Type
   is begin
      --  Same as "*", but with Right.x, y, z negated
      return (X => -Left.S * Right.X + Left.X * Right.S - Left.Y * Right.Z + Left.Z * Right.Y,
              Y => -Left.S * Right.Y + Left.X * Right.Z + Left.Y * Right.S - Left.Z * Right.X,
              Z => -Left.S * Right.Z - Left.X * Right.Y + Left.Y * Right.X + Left.Z * Right.S,
              S =>  Left.S * Right.S + Left.X * Right.X + Left.Y * Right.Y + Left.Z * Right.Z);
   end Times_Inverse;

   ----------
   --  General matrices for random purposes.

   function Determinant (Item : in Cart_Array_Cart_Vector_Type) return Real_Type
   is
      Det : constant Real_Type :=
         (Item (X)(X) * ((Item (Y)(Y) * Item (Z)(Z)) - (Item (Y)(Z)) * (Item (Z)(Y)))) -
         (Item (X)(Y) * ((Item (Y)(X) * Item (Z)(Z)) - (Item (Y)(Z)) * (Item (Z)(X)))) +
         (Item (X)(Z) * ((Item (Y)(X) * Item (Z)(Y)) - (Item (Y)(Y)) * (Item (Z)(X))));

   begin
      return Det;

   end Determinant;


   function Inverse (Item : in Cart_Array_Cart_Vector_Type) return Cart_Array_Cart_Vector_Type
   is
      Det : constant Real_Type :=
         (Item (X)(X) * ((Item (Y)(Y) * Item (Z)(Z)) - (Item (Y)(Z)) * (Item (Z)(Y)))) -
         (Item (X)(Y) * ((Item (Y)(X) * Item (Z)(Z)) - (Item (Y)(Z)) * (Item (Z)(X)))) +
         (Item (X)(Z) * ((Item (Y)(X) * Item (Z)(Y)) - (Item (Y)(Y)) * (Item (Z)(X))));

   begin
      return
         (X =>
             (X =>  ((Item (Y)(Y) * Item (Z)(Z)) - (Item (Y)(Z) * Item (Z)(Y))) / Det,
              Y => -((Item (X)(Y) * Item (Z)(Z)) - (Item (X)(Z) * Item (Z)(Y))) / Det,
              Z =>  ((Item (X)(Y) * Item (Y)(Z)) - (Item (X)(Z) * Item (Y)(Y))) / Det),

          Y =>
             (X => -((Item (Y)(X) * Item (Z)(Z)) - (Item (Y)(Z) * Item (Z)(X))) / Det,
              Y =>  ((Item (X)(X) * Item (Z)(Z)) - (Item (X)(Z) * Item (Z)(X))) / Det,
              Z => -((Item (X)(X) * Item (Y)(Z)) - (Item (X)(Z) * Item (Y)(X))) / Det),

          Z =>
             (X =>  ((Item (Y)(X) * Item (Z)(Y)) - (Item (Y)(Y) * Item (Z)(X))) / Det,
              Y => -((Item (X)(X) * Item (Z)(Y)) - (Item (X)(Y) * Item (Z)(X))) / Det,
              Z =>  ((Item (X)(X) * Item (Y)(Y)) - (Item (X)(Y) * Item (Y)(X))) / Det));
   end Inverse;

   ----------
   --  Rotation matrices

   function To_Cart_Array_Cart_Vector (Item : in Rot_Matrix_Type) return Cart_Array_Cart_Vector_Type
   is
   begin
      return Cart_Array_Cart_Vector_Type (Item);
   end To_Cart_Array_Cart_Vector;

   function To_Rot_Matrix (Item : in Cart_Array_Cart_Vector_Type) return Rot_Matrix_Type
   is
      --  This algorithm is not very good, because it ignores the
      --  third row. Ni = Normalized vector; Ui = unNormalized vector
      use Cart_Vector_Ops;
      NX : Unit_Vector_Type;
      NY : Unit_Vector_Type;
      NZ : Cart_Vector_Type;
      UY : constant Cart_Vector_Type := (Item (Y)(X), Item (Y)(Y), Item (Y)(Z));
   begin
      NX := To_Unit_Vector (Cart_Vector_Type'(Item (X)(X), Item (X)(Y), Item (X)(Z)));
      NY := To_Unit_Vector (UY - NX * (NX * UY));
      NZ := Cross (NX, NY);
      return
         ((X (NX), Y (NX), Z (NX)),
          (X (NY), Y (NY), Z (NY)),
          NZ);
   exception
   when Non_Normalizable_Unit_Vector =>
      raise Non_Normalizable_Rot_Matrix;
   end To_Rot_Matrix;

   function Unchecked_Rot_Matrix (Item : in Cart_Array_Cart_Vector_Type) return Rot_Matrix_Type
   is
   begin
      return Rot_Matrix_Type (Item);
   end Unchecked_Rot_Matrix;

   function Mag (Item : in Rot_Matrix_Type) return Real_Type
   is
      use Math_Scalar;
      Temp_Axis : constant Cart_Vector_Type :=
         (Item (Z)(Y) - Item (Y)(Z), Item (X)(Z) - Item (Z)(X), Item (Y)(X) - Item (X)(Y));
      Cos_Mag : constant Real_Type := (Item (X)(X) + Item (Y)(Y) + Item (Z)(Z) - 1.0) * 0.5;
      Sin_Mag : constant Real_Type := Mag (Temp_Axis) * 0.5;
   begin
      if Sin_Mag > 0.0 then
         return Atan2 (To_Trig_Pair (Sin_Mag, Cos_Mag));
      elsif Cos_Mag > 0.0 then
         return 0.0;
      else
         return Pi;
      end if;
   end Mag;

   overriding function Inverse (Item : in Rot_Matrix_Type) return Rot_Matrix_Type
   is begin
      return
         ((Item (X)(X), Item (Y)(X), Item (Z)(X)),
          (Item (X)(Y), Item (Y)(Y), Item (Z)(Y)),
          (Item (X)(Z), Item (Y)(Z), Item (Z)(Z)));
   end Inverse;

   function Rot_Matrix_Times_Rot_Matrix (Left, Right : in Rot_Matrix_Type) return Rot_Matrix_Type
   is begin
      return Rot_Matrix_Type (CACV_Ops."*" (Cart_Array_Cart_Vector_Type (Left), Cart_Array_Cart_Vector_Type (Right)));
   end Rot_Matrix_Times_Rot_Matrix;

   function Inverse_Times (Left, Right : in Rot_Matrix_Type) return Rot_Matrix_Type
   is begin
      return Rot_Matrix_Type
         (CACV_Ops.Transpose_Times (Cart_Array_Cart_Vector_Type (Left), Cart_Array_Cart_Vector_Type (Right)));
   end Inverse_Times;

   function Times_Inverse (Left, Right : in Rot_Matrix_Type) return Rot_Matrix_Type
   is begin
      return Rot_Matrix_Type
         (CACV_Ops.Times_Transpose (Cart_Array_Cart_Vector_Type (Left), Cart_Array_Cart_Vector_Type (Right)));
   end Times_Inverse;

   function Rot_Matrix_Times_CACV (Left : in Rot_Matrix_Type; Right : in Cart_Array_Cart_Vector_Type)
      return Cart_Array_Cart_Vector_Type
   is begin
      return CACV_Ops."*" (Cart_Array_Cart_Vector_Type (Left), Right);
   end Rot_Matrix_Times_CACV;

   function CACV_Times_Rot_Matrix
      (Left  : in Cart_Array_Cart_Vector_Type;
       Right : in Rot_Matrix_Type)
      return Cart_Array_Cart_Vector_Type
   is begin
      return CACV_Ops."*" (Left, Cart_Array_Cart_Vector_Type (Right));
   end CACV_Times_Rot_Matrix;

   ----------
   --  Inertias

   function To_Cart_Array_Cart_Vector (Item : in Inertia_Type) return Cart_Array_Cart_Vector_Type
   is begin
      return
         (X => (Item (Ixx), Item (Ixy), Item (Ixz)),
          Y => (Item (Ixy), Item (Iyy), Item (Iyz)),
          Z => (Item (Ixz), Item (Iyz), Item (Izz)));
   end To_Cart_Array_Cart_Vector;

   function "+" (Left, Right : in Inertia_Type) return Inertia_Type
   is begin
      return
        (Ixx => Left (Ixx) + Right (Ixx),
         Iyy => Left (Iyy) + Right (Iyy),
         Izz => Left (Izz) + Right (Izz),
         Ixy => Left (Ixy) + Right (Ixy),
         Ixz => Left (Ixz) + Right (Ixz),
         Iyz => Left (Iyz) + Right (Iyz));
   end "+";

   function "-" (Left, Right : in Inertia_Type) return Inertia_Type
   is begin
      return
        (Ixx => Left (Ixx) - Right (Ixx),
         Iyy => Left (Iyy) - Right (Iyy),
         Izz => Left (Izz) - Right (Izz),
         Ixy => Left (Ixy) - Right (Ixy),
         Ixz => Left (Ixz) - Right (Ixz),
         Iyz => Left (Iyz) - Right (Iyz));
   end "-";

   function "*" (Left : in Inertia_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type
   is begin
      return
         (X => Left (Ixx) * Right (X) + Left (Ixy) * Right (Y) + Left (Ixz) * Right (Z),
          Y => Left (Ixy) * Right (X) + Left (Iyy) * Right (Y) + Left (Iyz) * Right (Z),
          Z => Left (Ixz) * Right (X) + Left (Iyz) * Right (Y) + Left (Izz) * Right (Z));
   end "*";

   function Inverse (Item : in Inertia_Type) return Inverse_Inertia_Type
   is
      --  See ../Maxima/derive_math_dof_3.maxima for derivation
      T_1 : constant Real_Type := Item (Iyy) * Item (Izz) - Item (Iyz) * Item (Iyz);
      T_2 : constant Real_Type := Item (Ixy) * Item (Iyz) - Item (Ixz) * Item (Iyy);
      T_3 : constant Real_Type := Item (Ixz) * Item (Iyz) - Item (Ixy) * Item (Izz);
      T_4 : constant Real_Type := 1.0 / (Item (Ixx) * T_1 + Item (Ixy) * T_3 + Item (Ixz) * T_2);
      T_5 : constant Real_Type := T_3 * T_4;
      T_6 : constant Real_Type := T_2 * T_4;
      T_7 : constant Real_Type := (Item (Ixy) * Item (Ixz) - Item (Ixx) * Item (Iyz)) * T_4;

   begin
      return
        (Ixx => T_1 * T_4,
         Ixy => T_5,
         Ixz => T_6,
         Iyy => (Item (Ixx) * Item (Izz) - Item (Ixz) * Item (Ixz)) * T_4,
         Iyz => T_7,
         Izz => (Item (Ixx) * Item (Iyy) - Item (Ixy) * Item (Ixy)) * T_4);
   end Inverse;

   function "*" (Left : in Inverse_Inertia_Type; Right : in Cart_Vector_Type) return Cart_Vector_Type
   is begin
      return
         (X => Left (Ixx) * Right (X) + Left (Ixy) * Right (Y) + Left (Ixz) * Right (Z),
          Y => Left (Ixy) * Right (X) + Left (Iyy) * Right (Y) + Left (Iyz) * Right (Z),
          Z => Left (Ixz) * Right (X) + Left (Iyz) * Right (Y) + Left (Izz) * Right (Z));
   end "*";

   function Parallel_Axis
      (Total_Mass     : in Real_Type;
       Center_Of_Mass : in Cart_Vector_Type;
       Inertia        : in Inertia_Type)
      return Inertia_Type
   is begin
      --  [1] 3.4.1-3 eqn:parallel_axis
      return
         (Ixx => Inertia (Ixx) + Total_Mass *
                 (Center_Of_Mass (Y) * Center_Of_Mass (Y) + Center_Of_Mass (Z) * Center_Of_Mass (Z)),
          Iyy => Inertia (Iyy) + Total_Mass *
                 (Center_Of_Mass (Z) * Center_Of_Mass (Z) + Center_Of_Mass (X) * Center_Of_Mass (X)),
          Izz => Inertia (Izz) + Total_Mass *
                 (Center_Of_Mass (X) * Center_Of_Mass (X) + Center_Of_Mass (Y) * Center_Of_Mass (Y)),
          Ixy => Inertia (Ixy) - Total_Mass * Center_Of_Mass (X) * Center_Of_Mass (Y),
          Ixz => Inertia (Ixz) - Total_Mass * Center_Of_Mass (X) * Center_Of_Mass (Z),
          Iyz => Inertia (Iyz) - Total_Mass * Center_Of_Mass (Y) * Center_Of_Mass (Z));
   end Parallel_Axis;

   function Interpolate
     (X : in Real_Type;
      X1    : in Real_Type;
      X2    : in Real_Type;
      Y1    : in Inertia_Type;
      Y2    : in Inertia_Type)
     return Inertia_Type
   is
      function "/" (Left : in Inertia_Type; Right : in Real_Type) return Inertia_Type
      is begin
         return
           (Left (Ixx) / Right,
            Left (Iyy) / Right,
            Left (Izz) / Right,
            Left (Ixy) / Right,
            Left (Ixz) / Right,
            Left (Iyz) / Right);
      end "/";

      function "*" (Left : in Inertia_Type; Right : in Real_Type) return Inertia_Type
      is begin
         return
           (Left (Ixx) * Right,
            Left (Iyy) * Right,
            Left (Izz) * Right,
            Left (Ixy) * Right,
            Left (Ixz) * Right,
            Left (Iyz) * Right);
      end "*";

   begin
      return Y1 + ((Y2 - Y1) / (X2 - X1)) * (X - X1);
   end Interpolate;

end SAL.Gen_Math.Gen_DOF_3;
