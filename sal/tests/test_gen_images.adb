--  Abstract :
--
--  Test generic image packages.
--
--  Copyright (C) 2001 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;
with SAL.Generic_Binary_Image;
with SAL.Generic_Decimal_Image;
with SAL.Generic_Float_Image;
procedure Test_Gen_Images
is
   function Binary_Image is new SAL.Generic_Binary_Image
      (Nibbles     => 2,
       Number_Type => Interfaces.Unsigned_8);

   function Decimal_Image is new SAL.Generic_Decimal_Image (Number_Type => Interfaces.Integer_8);

   function Float_Image is new SAL.Generic_Float_Image (Number_Type => Float);

begin
   Put_Line ("Binary_Image (16#23#) => " & Binary_Image (16#23#));

   Put_Line ("Decimal_Image (23, Width => 4) => " & Decimal_Image (23, 4));

   Put_Line ("Float_Image (-23.5, Fore => 4, Aft => 2) => " & Float_Image (-23.5, Fore => 4, Aft => 2));
end Test_Gen_Images;
