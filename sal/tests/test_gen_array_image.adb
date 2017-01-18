--  Abstract :
--
--  Test SAL.Gen_Array_Image
--
--  Copyright (C) 2002, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen_Array_Image;
procedure Test_Gen_Array_Image
is
   type Wheel_Index_Type is range 1 .. 3;
   type Wheel_Array_Float_Type is array (Wheel_Index_Type) of Float;

   function Wheel_Array_Float_Image is new SAL.Gen_Array_Image
     (Index_Type               => Wheel_Index_Type,
      Element_Type             => Float,
      Index_Array_Element_Type => Wheel_Array_Float_Type,
      Image                    => Float'Image);

   A : constant Wheel_Array_Float_Type := (1.0, 2.0, 3.0);

begin
   Put_Line ("A => " & Wheel_Array_Float_Image (A));
end Test_Gen_Array_Image;
