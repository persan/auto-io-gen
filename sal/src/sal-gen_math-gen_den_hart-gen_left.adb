--  Abstract:
--
--  see spec
--
--  References:
--
--  [1] Derive_Mult.mac
--
--  MODIFICATION HISTORY:
--    (kept for sentimental reasons; see CVS for more recent history)
--
--  Jan. 17, 1991       Dana Miller     Created
--  27 Jan 1992 Stephe Leake
--     Match spec changes
--  15 June 1992        Stephe Leake
--     Match spec changes, use [.DERIVE]MANIPULATOR_MATH.MAC for all
--     bodies.
--   8 July 1992        Stephe Leake
--     match changes to Math_6_DOF spec
--   August 5, 1992     Victoria Buckland
--     After much headache, finally realized to change temporary
--     quaternion object assignments in Partial_Jacobian to reflect their
--     object names.  (ex. Qx_Qy is  2.0*Qx*Qy  NOT  2.0*Qs_Qx; bug due to
--     Stephe!)
--  30 Nov 1993 Stephe Leake
--     match spec changes
--  18 Jul 1994     Victoria Buckland
--     match spec changes
--  21 May 2002     Stephe Leake
--     match spec changes

package body SAL.Gen_Math.Gen_Den_Hart.Gen_Left is

   function To_Pose (Param : in Den_Hart_Type; Position : in Real_Type) return Math_DOF_6.Pose_Type
   is
      use Math_Scalar, Math_DOF_3;
      Half_Trig_Alpha : constant Trig_Pair_Type := Half_Trig (Param.Trig_Alpha);
      Half_Trig_Theta : Trig_Pair_Type;
      D               : Real_Type;
   begin
      case Param.Class is
      when Prismatic =>
         Half_Trig_Theta := Half_Trig (Param.Trig_Theta);
         D               := Position;
      when Revolute =>
         Half_Trig_Theta := Half_Trig (Sin_Cos (Position));
         D               := Param.D;
      end case;

      return
        (Translation => (Param.A, -Sin (Param.Trig_Alpha) * D, Cos (Param.Trig_Alpha) * D),
         Rotation    => Unchecked_Unit_Quaternion
           (S        => Cos (Half_Trig_Alpha) * Cos (Half_Trig_Theta),
            X        => Sin (Half_Trig_Alpha) * Cos (Half_Trig_Theta),
            Y        => -Sin (Half_Trig_Alpha) * Sin (Half_Trig_Theta),
            Z        => Cos (Half_Trig_Alpha) * Sin (Half_Trig_Theta)));
   end To_Pose;

   function To_Inverse_Pose (Param : in Den_Hart_Type; Position : in Real_Type) return Math_DOF_6.Pose_Type
   is
      use Math_Scalar, Math_DOF_3;
      Half_Trig_Alpha : constant Trig_Pair_Type := Half_Trig (Param.Trig_Alpha);
      Half_Trig_Theta : Trig_Pair_Type;
      Trig_Theta      : Trig_Pair_Type;
      D               : Real_Type;
   begin
      case Param.Class is
      when Prismatic =>
         Trig_Theta      := Param.Trig_Theta;
         Half_Trig_Theta := Half_Trig (Trig_Theta);
         D               := Position;
      when Revolute =>
         Trig_Theta      := Sin_Cos (Position);
         Half_Trig_Theta := Half_Trig (Trig_Theta);
         D               := Param.D;
      end case;

      return
        (Translation => (-Param.A * Cos (Trig_Theta), Param.A * Sin (Trig_Theta), -D),
         Rotation    => Unchecked_Unit_Quaternion
           (S        => Cos (Half_Trig_Alpha) * Cos (Half_Trig_Theta),
            X        => -Sin (Half_Trig_Alpha) * Cos (Half_Trig_Theta),
            Y        => Sin (Half_Trig_Alpha) * Sin (Half_Trig_Theta),
            Z        => -Cos (Half_Trig_Alpha) * Sin (Half_Trig_Theta)));
   end To_Inverse_Pose;

   function Partial_Jacobian (Ti_T_Obj : Math_DOF_6.Pose_Type) return Math_DOF_6.Dual_Cart_Vector_Type
   is
      use Math_DOF_3;
      TX : constant Real_Type := Ti_T_Obj.Translation (X);
      TY : constant Real_Type := Ti_T_Obj.Translation (Y);
      QS : constant Real_Type := S (Ti_T_Obj.Rotation);
      QX : constant Real_Type := X (Ti_T_Obj.Rotation);
      QY : constant Real_Type := Y (Ti_T_Obj.Rotation);
      QZ : constant Real_Type := Z (Ti_T_Obj.Rotation);

      Qs_Qx : constant Real_Type := 2.0*QS*QX;
      Qs_Qy : constant Real_Type := 2.0*QS*QY;
      Qs_Qz : constant Real_Type := 2.0*QS*QZ;
      Qx2   : constant Real_Type := 2.0*QX*QX;
      Qx_Qy : constant Real_Type := 2.0*QX*QY;
      Qx_Qz : constant Real_Type := 2.0*QX*QZ;
      Qy2   : constant Real_Type := 2.0*QY*QY;
      Qy_Qz : constant Real_Type := 2.0*QY*QZ;
      Qz2   : constant Real_Type := 2.0*QZ*QZ;

   begin
      return
        (Math_DOF_6.TX => (Qz2 + Qy2 - 1.0) * TY + (Qx_Qy + Qs_Qz) * TX,
         Math_DOF_6.TY => (Qs_Qz - Qx_Qy) * TY + (1.0 - Qz2 - Qx2) * TX,
         Math_DOF_6.TZ => (-Qx_Qz - Qs_Qy) * TY + (Qy_Qz - Qs_Qx) * TX,
         Math_DOF_6.RX => Qx_Qz - Qs_Qy,
         Math_DOF_6.RY => Qy_Qz + Qs_Qx,
         Math_DOF_6.RZ => 1.0 - Qy2 - Qx2);
   end Partial_Jacobian;

   function Mult
     (Left           : in Math_DOF_6.Pose_Type;
      Right          : in Den_Hart_Type;
      Right_Position : in Real_Type)
     return Math_DOF_6.Pose_Type
   is
      use Math_DOF_3.Cart_Vector_Ops;
      use Math_DOF_3;
      use Math_DOF_3_Left;
      Right_Pose : constant Math_DOF_6.Pose_Type := To_Pose (Right, Right_Position);
   begin
      return
        (Translation => Left.Translation + Left.Rotation * Right_Pose.Translation,
         Rotation    => Left.Rotation * Right_Pose.Rotation);
   end Mult;

   function Mult
     (Left          : in Den_Hart_Type;
      Left_Position : in Real_Type;
      Right         : in Math_DOF_6.Pose_Type)
     return Math_DOF_6.Pose_Type
   is
      use Math_Scalar, Math_DOF_3;
      Half_Trig_Alpha : constant Trig_Pair_Type := Half_Trig (Left.Trig_Alpha);
      Half_Trig_Theta : Trig_Pair_Type;
      Trig_Theta      : Trig_Pair_Type;
      D               : Real_Type;
      Left_Rot        : Unit_Quaternion_Type;
   begin
      case Left.Class is
      when Prismatic =>
         Half_Trig_Theta := Half_Trig (Left.Trig_Theta);
         D               := Left_Position;
      when Revolute =>
         Trig_Theta      := Sin_Cos (Left_Position);
         Half_Trig_Theta := Half_Trig (Trig_Theta);
         D               := Left.D;
      end case;

      --  Extracted from body of To_Pose (Den_Hart, Position)
      Left_Rot := Unchecked_Unit_Quaternion
        (S => Cos (Half_Trig_Alpha) * Cos (Half_Trig_Theta),
         X => Sin (Half_Trig_Alpha) * Cos (Half_Trig_Theta),
         Y => -Sin (Half_Trig_Alpha) * Sin (Half_Trig_Theta),
         Z => Cos (Half_Trig_Alpha) * Sin (Half_Trig_Theta));

      return
        (Translation => --  [1] see Result[1]
           (X => Left.A - Sin (Trig_Theta) * Right.Translation (Y) + Cos (Trig_Theta) * Right.Translation (X),
            Y => -Sin (Left.Trig_Alpha) * (D + Right.Translation (Z)) + Cos (Left.Trig_Alpha) *
              (Sin (Trig_Theta) * Right.Translation (X) + Cos (Trig_Theta) * Right.Translation (Y)),

            Z => Cos (Left.Trig_Alpha) * (D + Right.Translation (Z)) + Sin (Left.Trig_Alpha) *
              (Sin (Trig_Theta) * Right.Translation (X) + Cos (Trig_Theta) * Right.Translation (Y))),

         Rotation =>
           Left_Rot * Right.Rotation);
   end Mult;

end SAL.Gen_Math.Gen_Den_Hart.Gen_Left;
