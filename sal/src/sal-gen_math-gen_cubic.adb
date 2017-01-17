--  Abstract :
--
--  See spec
--
--  Copyright (C) 2006 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_Cubic is

   procedure Solve
     (Coef   : in     Coefficients_Type;
      Root_1 :    out Complex_Types.Complex;
      Root_2 :    out Complex_Types.Complex;
      Root_3 :    out Complex_Types.Complex)
   is
      use Complex_Elementary;
      use Complex_Types;
      use Elementary;

      --  Apply Cardano's method from http://en.wikipedia.org/wiki/Cubic_equation

      function "+" (Real : in Real_Type) return Complex renames Compose_From_Cartesian;

      Zeta   : constant Complex := (Re   => -0.5, Im => Sqrt (3.0) / 2.0);
      Zeta_2 : constant Complex := (Re => -0.5, Im => -Sqrt (3.0) / 2.0);

      A   : constant Real_Type := Coef (2) / Coef (3);
      B   : constant Real_Type := Coef (1) / Coef (3);
      C   : constant Real_Type := Coef (0) / Coef (3);
      P   : constant Real_Type := B - A**2 / 3.0;
      Q   : constant Real_Type := C + (2.0 * A ** 3 - 9.0 * A * B) / 27.0;
      U_1 : constant Complex   := (Q / 2.0 + Sqrt (+(Q**2 / 4.0 + P**3 / 27.0))) ** (1.0 / 3.0);
      U_2 : constant Complex   := U_1 * Zeta;
      U_3 : constant Complex   := U_1 * Zeta_2;

   begin
      Root_1 := P / (3.0 * U_1) - U_1 - A / 3.0;
      Root_2 := P / (3.0 * U_2) - U_2 - A / 3.0;
      Root_3 := P / (3.0 * U_3) - U_3 - A / 3.0;
   end Solve;

   function Discriminant (Coef : in Coefficients_Type) return Real_Type
   is
      A : Real_Type renames Coef (3);
      B : Real_Type renames Coef (2);
      C : Real_Type renames Coef (1);
      D : Real_Type renames Coef (0);
   begin
      return 4.0 * B**3 * D - B**2 * C**2 + 4.0 * A * C**3 - 18.0 * A * B * C * D + 27.0 * A**2 * D**2;
   end Discriminant;

end SAL.Gen_Math.Gen_Cubic;
