--  Abstract :
--
--  test
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen.Gray_Code;
procedure Test_Gray_Code is

   type Unsigned_5 is mod 2**5;

   package Gray_Code_5 is new SAL.Gen.Gray_Code (5, Unsigned_5);
   use Gray_Code_5;

   package Unsigned_5_IO is new Ada.Text_IO.Modular_IO (Unsigned_5);
   use Unsigned_5_IO;

begin
   Put_Line ("Binary    Gray");
   for I in Unsigned_5 loop
      Put (I, Width => 8, Base => 2);
      Put (To_Gray_Code (I), Width => 10, Base => 2);
      New_Line;
   end loop;
end Test_Gray_Code;
