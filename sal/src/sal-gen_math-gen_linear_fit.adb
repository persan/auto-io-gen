--  Abstract :
--
--  See spec.
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

package body SAL.Gen_Math.Gen_Linear_Fit is

   procedure Reset (Data : in out Data_Type)
   is begin
      Data :=
        (Count         => 0,
         Sum_X         => 0.0,
         Sum_Y         => 0.0,
         Sum_X_Y       => 0.0,
         Sum_X_Squared => 0.0);
   end Reset;

   procedure Accumulate (Data : in out Data_Type; X, Y : in Real_Type) is
   begin
      Data :=
        (Count         => Data.Count + 1,
         Sum_X         => Data.Sum_X + X,
         Sum_Y         => Data.Sum_Y + Y,
         Sum_X_Y       => Data.Sum_X_Y + X * Y,
         Sum_X_Squared => Data.Sum_X_Squared + X * X);
   end Accumulate;

   procedure Fit
     (Data : in     Data_Type;
      M    :    out Real_Type;
      B    :    out Real_Type)
   is
      Count : constant Real_Type := Real_Type (Data.Count);
   begin
      M := (Count * Data.Sum_X_Y - Data.Sum_X * Data.Sum_Y) /
        (Count * Data.Sum_X_Squared - Data.Sum_X * Data.Sum_X);

      B := Data.Sum_Y / Count - M * Data.Sum_X / Count;
   end Fit;

end SAL.Gen_Math.Gen_Linear_Fit;
