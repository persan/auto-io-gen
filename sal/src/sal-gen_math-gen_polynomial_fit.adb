--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2005 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_Polynomial_Fit is

   procedure Reset (Data : in out Data_Type)
   is begin
      Data :=
        (Alpha => (others => (others => 0.0)),
         Beta  => (others => 0.0));
   end Reset;

   procedure Accumulate (Data : in out Data_Type; X, Y : in Real_Type)
   is
      --  [1] eqns 14.3.8, 14.3.9

      X_J : Real_Type; -- X ** J
      X_K : Real_Type := X ** Integer (Index_Type'First); -- X ** K
   begin
      for K in Index_Type loop
         X_J := X ** Integer (Index_Type'First);

         --  IMPROVEME: Alpha is symmetrical, so we could optimize this.
         for J in Index_Type loop
            Data.Alpha (J)(K) := Data.Alpha (J)(K) + X_J * X_K;

            X_J := X_J * X;
         end loop;

         Data.Beta (K) := Data.Beta (K) + Y * X_K;

         X_K := X_K * X;
      end loop;

   end Accumulate;

   procedure Fit
     (Data  : in     Data_Type;
      Coeff :    out Index_Array_Real_Type;
      Sigma :    out Index_Array_Real_Type)
   is
      --  [1] 14.3.11
      Alpha_Inverse : constant Square_Array_Type := Inverse (Data.Alpha);
      use Square_Array;
   begin
      Coeff := Alpha_Inverse * Data.Beta;
      for I in Sigma'Range loop
         Sigma (I) := Sqrt (Alpha_Inverse (I)(I));
      end loop;
   end Fit;

end SAL.Gen_Math.Gen_Polynomial_Fit;
