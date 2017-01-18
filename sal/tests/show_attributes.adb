--  Abstract :
--
--  Show various attributes of Float_Type and Double_Type.
--
--  Copyright (C) 2003 Stephen Leake.  All Rights Reserved.
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
--

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Math_Double.Elementary;
with SAL.Math_Float.Elementary;
procedure Show_Attributes
is
begin
   declare
      use SAL.Math_Float, SAL.Math_Float.Elementary;
   begin
      Put_Line ("Float_Type");
      Put_Line ("Model_Epsilon  => " & Real_Type'Image (Real_Type'Model_Epsilon));
      Put_Line ("Sqrt (Model_Epsilon)  => " & Real_Type'Image (Sqrt (Real_Type'Model_Epsilon)));
   end;

   declare
      use SAL.Math_Double, SAL.Math_Double.Elementary;
   begin
      Put_Line ("Double_Type");
      Put_Line ("Model_Epsilon  => " & Real_Type'Image (Real_Type'Model_Epsilon));
      Put_Line ("Sqrt (Model_Epsilon)  => " & Real_Type'Image (Sqrt (Real_Type'Model_Epsilon)));
   end;

end Show_Attributes;
