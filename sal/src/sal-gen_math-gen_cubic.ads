--  Abstract :
--
--  Solve cubic polynomial equations.
--
--  It may be more efficient to re-implement this package to take
--  advantage of structure in your particular cubic; see
--  sal-gen_math-gen_tank for an example. In that case, this package
--  can be used to test the new package.
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

pragma License (Modified_GPL);

with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;
generic
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real_Type);
   with package Complex_Elementary is new Ada.Numerics.Generic_Complex_Elementary_Functions (Complex_Types);
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
package SAL.Gen_Math.Gen_Cubic is
   pragma Pure;

   type Coefficients_Type is array (0 .. 3) of Real_Type;
   --  equation is 0 = sum coeff (I) x**I

   procedure Solve
     (Coef   : in     Coefficients_Type;
      Root_1 :    out Complex_Types.Complex;
      Root_2 :    out Complex_Types.Complex;
      Root_3 :    out Complex_Types.Complex);

   function Discriminant (Coef : in Coefficients_Type) return Real_Type;

end SAL.Gen_Math.Gen_Cubic;
