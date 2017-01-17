--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2006 - 2008 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
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

package body SAL.Gen_Math.Gen_Tank is

   procedure Compute_Sphere
     (Rho            : in     Real_Type;
      Mass           : in     Real_Type;
      Radius         :    out Real_Type;
      Center_Inertia :    out Real_Type)
   is
      --  [1] 4.1.2-1, 4.1.2-2 eqn:sphere_tank
      use Ada.Numerics;
      use Elementary;
   begin
      Radius         := (3.0 * Mass / (4.0 * Pi * Rho))**(1.0/3.0);
      Center_Inertia := 0.4 * Mass * Radius ** 2;
   end Compute_Sphere;

   procedure Compute_Sphere_Sect
     (Rho           : in     Real_Type;
      Radius        : in     Real_Type;
      Mass          : in     Real_Type;
      Height        :    out Real_Type;
      CM_Sym        :    out Real_Type;
      I_Sym         :    out Real_Type;
      I_Non_Sym_Bot :    out Real_Type)
   is
      use Ada.Numerics;
      use Complex_Types;
      use Complex_Elementary;
      use Elementary;

      M : Real_Type renames Mass;
      R : Real_Type renames Radius;

      --  [1] 4.1.2-3 eqn:sphere_sect_h(mass)

      Two_Pi_Rho : constant Real_Type := 2.0 * Pi * Rho;
      Zeta       : constant Complex   := (Re => -0.5, Im => Sqrt (3.0) / 2.0);

      A1  : constant Real_Type := 2.0 * Two_Pi_Rho * R**3 - 3.0 * M;
      --  A1 can be slightly negative when m = 4/3 pi rho R**3, due to roundoff error.
      A2  : constant Real_Type := Sqrt (3.0) * Sqrt (abs M) * Sqrt (abs A1) / Two_Pi_Rho;
      A   : constant Complex   := (Re => 0.0, Im => A2);
      B   : constant Real_Type := (Two_Pi_Rho * R**3 - 3.0 * M) / Two_Pi_Rho;
      U_3 : constant Complex   := (B + A)**(1.0 / 3.0);

      --  See test/debug_math_double_tank_cubic.adb;
      --  U_3 is Maxima root 3, but SAL.Math_Double.Cubic root_2; we
      --  need SAL.Math_Double.Cubic root_3, which is Maxima root 1.
      U_1 : constant Complex := U_3 / Zeta;
      H   : constant Real_Type := Re (R**2 / U_1 + R + U_1);

      H_Cube_Pi_Rho : constant Real_Type := H**3 * Pi * Rho;
   begin
      Height := H;

      CM_Sym        := H * (8.0 * R - 3.0 * H) / (4.0 * (3.0 * R - H));
      I_Sym         := H_Cube_Pi_Rho * (20.0 * R**2 - 15.0 * H * R + 3.0 * H**2) / 30.0;
      I_Non_Sym_Bot := H_Cube_Pi_Rho * (20.0 * R**2 + 15.0 * H * R - 9.0 * H**2) / 60.0;

   end Compute_Sphere_Sect;

   function I_Non_Sym_CM
     (Mass          : in Real_Type;
      CM_Sym        : in Real_Type;
      I_Non_Sym_Bot : in Real_Type)
     return Real_Type
   is begin
      return I_Non_Sym_Bot - Mass * CM_Sym**2;
   end I_Non_Sym_CM;

end SAL.Gen_Math.Gen_Tank;
