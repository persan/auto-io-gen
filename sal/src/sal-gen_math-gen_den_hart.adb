--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2007 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_Den_Hart is

   function To_Rate_Transform
     (Param    : in Den_Hart_Type;
      Position : in Real_Type)
     return Math_DOF_6.Rate_Transform_Type
   is
      use Math_Scalar, Math_DOF_3;
      Theta : Trig_Pair_Type;
      Alpha : constant Trig_Pair_Type := Param.Trig_Alpha;
      D     : Real_Type;
      A     : constant Real_Type      := Param.A;
   begin
      case Param.Class is
      when Prismatic =>
         Theta := Param.Trig_Theta;
         D     := Position;
      when Revolute =>
         Theta := Sin_Cos (Position);
         D     := Param.D;
      end case;

      return Math_DOF_6.Unchecked_Rate_Transform
        (Rot =>
           ((Cos (Theta), Cos (Alpha) * Sin (Theta), Sin (Alpha) * Sin (Theta)),
            (-Sin (Theta), Cos (Alpha) * Cos (Theta), Sin (Alpha) * Cos (Theta)),
            (0.0,             -Sin (Alpha),               Cos (Alpha))),
         Rot_Cross =>
           (X =>
              (X => -D * Sin (Theta),
               Y => Cos (Alpha) * D * Cos (Theta) - A * Sin (Alpha) * Sin (Theta),
               Z => A * Cos (Alpha) * Sin (Theta) + Sin (Alpha) * D * Cos (Theta)),
            Y =>
              (X => -D * Cos (Theta),
               Y => -Cos (Alpha) * D * Sin (Theta) - A * Sin (Alpha) * Cos (Theta),
               Z => A * Cos (Alpha) * Cos (Theta) - Sin (Alpha) * D * Sin (Theta)),
            Z =>
              (X => 0.0,
               Y => -A * Cos (Alpha),
               Z => -A * Sin (Alpha))));
   end To_Rate_Transform;

   function To_Inverse_Rate_Transform
     (Param    : in Den_Hart_Type;
      Position : in Real_Type)
     return Math_DOF_6.Rate_Transform_Type
   is
      use Math_Scalar, Math_DOF_3;
      Theta : Trig_Pair_Type;
      Alpha : constant Trig_Pair_Type := Param.Trig_Alpha;
      D     : Real_Type;
      A     : constant Real_Type      := Param.A;
   begin
      case Param.Class is
      when Prismatic =>
         Theta := Param.Trig_Theta;
         D     := Position;
      when Revolute =>
         Theta := Sin_Cos (Position);
         D     := Param.D;
      end case;

      return Math_DOF_6.Unchecked_Rate_Transform
        (Rot =>
           ((Cos (Theta),             -Sin (Theta),           0.0),
            (Cos (Alpha) * Sin (Theta), Cos (Alpha) * Cos (Theta), -Sin (Alpha)),
            (Sin (Alpha) * Sin (Theta), Sin (Alpha) * Cos (Theta),   Cos (Alpha))),
         Rot_Cross =>
           (X =>
              (X => -D * Sin (Theta),
               Y => -D * Cos (Theta),
               Z => 0.0),
            Y =>
              (X => Cos (Alpha) * D * Cos (Theta) - A * Sin (Alpha) * Sin (Theta),
               Y => -Cos (Alpha) * D * Sin (Theta) - A * Sin (Alpha) * Cos (Theta),
               Z => -A * Cos (Alpha)),
            Z =>
              (X => A * Cos (Alpha) * Sin (Theta) + Sin (Alpha) * D * Cos (Theta),
               Y => A * Cos (Alpha) * Cos (Theta) - Sin (Alpha) * D * Sin (Theta),
               Z => -A * Sin (Alpha))));
   end To_Inverse_Rate_Transform;

   function To_Wrench_Transform
     (Param : in Den_Hart_Type;
      Position : in Real_Type)
     return Math_DOF_6.Wrench_Transform_Type
   is
      use Math_Scalar, Math_DOF_3;
      Theta : Trig_Pair_Type;
      Alpha : constant Trig_Pair_Type := Param.Trig_Alpha;
      D     : Real_Type;
      A     : constant Real_Type      := Param.A;
   begin
      case Param.Class is
      when Prismatic =>
         Theta := Param.Trig_Theta;
         D     := Position;
      when Revolute =>
         Theta := Sin_Cos (Position);
         D     := Param.D;
      end case;

      return Math_DOF_6.Unchecked_Wrench_Transform
        (Rot =>
           ((Cos (Theta), Cos (Alpha) * Sin (Theta), Sin (Alpha) * Sin (Theta)),
            (-Sin (Theta), Cos (Alpha) * Cos (Theta), Sin (Alpha) * Cos (Theta)),
            (0.0,             -Sin (Alpha),               Cos (Alpha))),
         Rot_Cross =>
           (X =>
              (X => -D * Sin (Theta),
               Y => Cos (Alpha) * D * Cos (Theta) - A * Sin (Alpha) * Sin (Theta),
               Z => A * Cos (Alpha) * Sin (Theta) + Sin (Alpha) * D * Cos (Theta)),
            Y =>
              (X => -D * Cos (Theta),
               Y => -Cos (Alpha) * D * Sin (Theta) - A * Sin (Alpha) * Cos (Theta),
               Z => A * Cos (Alpha) * Cos (Theta) - Sin (Alpha) * D * Sin (Theta)),
            Z =>
              (X => 0.0,
               Y => -A * Cos (Alpha),
               Z => -A * Sin (Alpha))));
   end To_Wrench_Transform;

   function To_Inverse_Wrench_Transform
     (Param    : in Den_Hart_Type;
      Position : in Real_Type)
     return Math_DOF_6.Wrench_Transform_Type
   is
      use Math_Scalar, Math_DOF_3;
      Theta : Trig_Pair_Type;
      Alpha : constant Trig_Pair_Type := Param.Trig_Alpha;
      D     : Real_Type;
      A     : constant Real_Type      := Param.A;
   begin
      case Param.Class is
      when Prismatic =>
         Theta := Param.Trig_Theta;
         D     := Position;
      when Revolute =>
         Theta := Sin_Cos (Position);
         D     := Param.D;
      end case;

      return Math_DOF_6.Unchecked_Wrench_Transform
        (Rot =>
           ((Cos (Theta),             -Sin (Theta),           0.0),
            (Cos (Alpha) * Sin (Theta), Cos (Alpha) * Cos (Theta), -Sin (Alpha)),
            (Sin (Alpha) * Sin (Theta), Sin (Alpha) * Cos (Theta),   Cos (Alpha))),
         Rot_Cross =>
           (X =>
              (X => -D * Sin (Theta),
               Y => -D * Cos (Theta),
               Z => 0.0),
            Y =>
              (X => Cos (Alpha) * D * Cos (Theta) - A * Sin (Alpha) * Sin (Theta),
               Y => -Cos (Alpha) * D * Sin (Theta) - A * Sin (Alpha) * Cos (Theta),
               Z => -A * Cos (Alpha)),
            Z =>
              (X => A * Cos (Alpha) * Sin (Theta) + Sin (Alpha) * D * Cos (Theta),
               Y => A * Cos (Alpha) * Cos (Theta) - Sin (Alpha) * D * Sin (Theta),
               Z => -A * Sin (Alpha))));
   end To_Inverse_Wrench_Transform;

end SAL.Gen_Math.Gen_Den_Hart;
