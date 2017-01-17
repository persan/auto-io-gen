--  Abstract:
--
--  see spec
--
--  Copyright (C) 2007 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_Den_Hart.Gen_Wertz is

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
            X        => -Sin (Half_Trig_Alpha) * Cos (Half_Trig_Theta),
            Y        => Sin (Half_Trig_Alpha) * Sin (Half_Trig_Theta),
            Z        => -Cos (Half_Trig_Alpha) * Sin (Half_Trig_Theta)));
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
            X        => Sin (Half_Trig_Alpha) * Cos (Half_Trig_Theta),
            Y        => -Sin (Half_Trig_Alpha) * Sin (Half_Trig_Theta),
            Z        => Cos (Half_Trig_Alpha) * Sin (Half_Trig_Theta)));
   end To_Inverse_Pose;

end SAL.Gen_Math.Gen_Den_Hart.Gen_Wertz;
