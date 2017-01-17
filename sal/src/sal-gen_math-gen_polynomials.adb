--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2003, 2008 Stephen Leake.  All Rights Reserved.
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
--
package body SAL.Gen_Math.Gen_Polynomials is

   function Compute (Argument : in Real_Type; Coefficients : in Coefficients_Type) return Real_Type
   is
      Result : Real_Type := Coefficients (Coefficients'Last);
   begin
      for I in reverse Coefficients'First .. Coefficients'Last - 1 loop
         Result := Argument * Result + Coefficients (I);
      end loop;

      if Coefficients'First /= 0 then
         --  Need to multiply by Argument some more if Coeff'first /= 0
         for I in 1 .. Coefficients'First loop
            Result := Argument * Result;
         end loop;

      end if;

      return Result;
   end Compute;

end SAL.Gen_Math.Gen_Polynomials;
