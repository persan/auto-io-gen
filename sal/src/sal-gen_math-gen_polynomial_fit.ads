--  Abstract :
--
--  Non linear polynomial fit
--
--  References:
--
--  [1] Numerical Recipies, William H. Press ed, 1986
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with SAL.Gen_Math.Gen_Square_Array;
generic
   type Index_Type is range <>;
   type Index_Array_Real_Type is array (Index_Type) of Real_Type;
   type Square_Array_Type is array (Index_Type) of Index_Array_Real_Type;
   with package Square_Array is new SAL.Gen_Math.Gen_Square_Array
     (Index_Type, Index_Array_Real_Type, Square_Array_Type);
   with function Inverse (Right : in Square_Array_Type) return Square_Array_Type;
   with function Sqrt (Right : in Real_Type) return Real_Type;
package SAL.Gen_Math.Gen_Polynomial_Fit is
   pragma Pure;

   type Data_Type is private;

   procedure Reset (Data : in out Data_Type);
   --  Initialize Data to start a new fit.

   procedure Accumulate (Data : in out Data_Type; X, Y : in Real_Type);

   procedure Fit
     (Data  : in     Data_Type;
      Coeff :    out Index_Array_Real_Type;
      Sigma :    out Index_Array_Real_Type);
   --  Fit Y = sum Coeff (I) * X ** I, for I in coeff'first ..
   --  coeff'last. Sigma (I) is the standard deviation in Coeff (I).

private
   type Data_Type is record
      --  Alpha and Beta from [1] eqns 14.3.8, 14.3.9
      Alpha : Square_Array_Type     := (others => (others => 0.0));
      Beta  : Index_Array_Real_Type := (others => 0.0);
   end record;

end SAL.Gen_Math.Gen_Polynomial_Fit;
