--  Abstract :
--
--  See spec.
--
--  This body uses Ada.Calendar
--
--  Copyright (C) 2002, 2006 Stephen Leake.  All Rights Reserved.
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
--  Historical interest :
--
--  1 June 1990 Stephe Leake Created

with Ada.Calendar;
with Ada.Text_IO;
package body SAL.Time_Me is

   package Duration_Text_IO is new Ada.Text_IO.Fixed_IO (Duration);

   function Gen_Time_Me (Iterations : in Natural) return Duration
   is
      use Ada.Calendar;
      Start_Time : Time;
   begin
      Time_Me;

      Start_Time := Clock;

      for I in 1 .. Iterations
      loop
         Time_Me;
      end loop;

      return Clock - Start_Time;
   end Gen_Time_Me;

   procedure Display (Comment : in String; Total_Time : in Duration; Iterations : in Natural)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Comment);
      Put ("Total time (seconds) :");
      Duration_Text_IO.Put (Total_Time);
      New_Line;
      Put ("Microseconds per call :");
      Duration_Text_IO.Put ((1_000_000 * Total_Time) / Iterations);
      New_Line;
   end Display;

end SAL.Time_Me;
