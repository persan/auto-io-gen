--  Abstract :
--
--  Statistics; mean, standard deviation, min, max.
--
--  Copyright (C) 2003, 2005 Stephen Leake.  All Rights Reserved.
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

with Ada.Numerics.Generic_Elementary_Functions;
with Interfaces;
generic
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
package SAL.Gen_Math.Gen_Stats is
   pragma Pure;

   type Stats_Type is private;

   procedure Reset (Stats : in out Stats_Type);
   --  Reset accumulated values, count to zero.

   procedure Accumulate (Stats : in out Stats_Type; Value : in Real_Type);

   function Standard_Deviation (Stats : in Stats_Type) return Real_Type;
   --  Returns 0.0 if count is < 2.

   function Mean (Stats : in Stats_Type) return Real_Type;

   function Min (Stats : in Stats_Type) return Real_Type;

   function Max (Stats : in Stats_Type) return Real_Type;

   type Display_Type is record
      Mean               : Real_Type;
      Standard_Deviation : Real_Type;
      Min                : Real_Type;
      Max                : Real_Type;
   end record;

   function Display (Stats : in Stats_Type) return Display_Type;

private
   type Stats_Type is record
      --  Count is a definite size for Network_Order child
      Count       : Interfaces.Integer_32 := 0;
      Sum         : Real_Type             := 0.0;
      Sum_Squared : Real_Type             := 0.0;
      Min         : Real_Type             := Real_Type'Last;
      Max         : Real_Type             := Real_Type'First;
   end record;

end SAL.Gen_Math.Gen_Stats;
