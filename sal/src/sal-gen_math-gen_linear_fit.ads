--  Abstract :
--
--  Linear fit
--
--  References:
--
--  [1] CRC Standard Mathematical Tables, 21st edition, "Probability
--      and Statistics / Curve Fitting" page 576
--
--  Copyright (C) 2004, 2007 Stephen Leake.  All Rights Reserved.
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

generic
package SAL.Gen_Math.Gen_Linear_Fit is
   pragma Pure;

   type Data_Type is private;

   procedure Reset (Data : in out Data_Type);
   --  Initialize Data to start a new fit.

   procedure Accumulate (Data : in out Data_Type; X, Y : in Real_Type);

   procedure Fit
     (Data : in     Data_Type;
      M    :    out Real_Type;
      B    :    out Real_Type);
   --  fit Y = M x + B

private
   type Data_Type is record
      Count         : Natural   := 0;
      Sum_X         : Real_Type := 0.0;
      Sum_Y         : Real_Type := 0.0;
      Sum_X_Y       : Real_Type := 0.0;
      Sum_X_Squared : Real_Type := 0.0;
   end record;

end SAL.Gen_Math.Gen_Linear_Fit;
