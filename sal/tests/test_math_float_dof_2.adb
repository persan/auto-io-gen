--  Abstract:
--
--  Test SAL.Math_Float.DOF_2.
--
--  Copyright (C) 2001 - 2003, 2009 Stephen Leake.  All Rights Reserved.
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
with SAL.Math_Float.DOF_2; use SAL.Math_Float.DOF_2;
with SAL.Math_Float.DOF_2.Text_IO; use SAL.Math_Float.DOF_2.Text_IO;
with SAL.Math_Float.Text_IO; use SAL.Math_Float.Text_IO;
procedure Test_Math_Float_DOF_2
is
   use SAL.Math_Float;

   Point_A   : constant Point_Type     := (2.0, 1.0);
   Point_B   : constant Point_Type     := (4.0, 3.0);
   Point_C   : Point_Type;
   S         : constant Real_Type      := 10.0;
   Rect_A    : Rectangle_Type          := (Point_A, Point_B);
   Rect_B    : constant Rectangle_Type := ((-1.0, -2.0), (1.0, 2.0));
   Point_D   : constant Point_Type     := (-1.0, 2.0);
   Rect_C    : constant Rectangle_Type := ((1.0, 2.0), (5.0, 3.0));
   Rect_Size : constant Point_Type     := (4.0, 5.0);
begin
   Real_Text_IO.Default_Exp := 0;

   Put_Line ("---------- test point operations");
   Put ("Mag "); Put (Point_A); Put (" => ");
   Put (Mag (Point_A));
   New_Line;

   Put ("Value " & Image (Point_A) & " => ");
   Put (Image (Point_Type'(Value (Image (Point_A)))));
   New_Line;

   Put (Point_A); Put (" * "); Put (S); Put (" => ");
   Put (Point_A * S);
   New_Line;

   Put (S); Put (" * "); Put (Point_A); Put (" => ");
   Put (S * Point_A);
   New_Line;

   Put (Point_A); Put (" / "); Put (S); Put (" => ");
   Put (Point_A / S);
   New_Line;

   Put (Point_A); Put (" + "); Put (Point_B); Put (" => ");
   Put (Point_A + Point_B);
   New_Line;

   Put (Point_A); Put (" - "); Put (Point_B); Put (" => ");
   Put (Point_A - Point_B);
   New_Line;

   Put_Line ("-------test rectangle operations");

   Put ("Size "); Put (Rect_A); Put (" => ");
   Put (Size (Rect_A));
   New_Line;

   Put ("Value "); Put (Rect_A); Put (" => ");
   Put (Rectangle_Type'(Value (Image (Rect_A))));
   New_Line;

   Put (Rect_A); Put (" + "); Put (Rect_B); Put_Line (" => ");
   Put (Rect_A + Rect_B);
   New_Line;

   Put (Rect_A); Put (" - "); Put (Rect_B); Put_Line (" => ");
   Put (Rect_A - Rect_B);
   New_Line;

   Put (Rect_A); Put (" + "); Put (Point_D); Put_Line (" => ");
   Put (Rect_A + Point_D);
   New_Line;

   Put ("Clip "); Put (Rect_A); Put (", "); Put (Rect_B); Put_Line (" => ");
   Put (Clip (Rect_A, Rect_B));
   New_Line;

   Put ("Clip "); Put (Rect_B); Put (", "); Put (Rect_A); Put_Line (" => ");
   Put (Clip (Rect_B, Rect_A));
   New_Line;

   Put ("Clip "); Put (Rect_A); Put (", "); Put (Rect_C); Put_Line (" => ");
   Put (Clip (Rect_A, Rect_C));
   New_Line;

   declare
      procedure Test (Point : in Point_Type; Rect : in Rectangle_Type)
      is begin
         Put ("Inside "); Put (Point); Put (", "); Put (Rect); Put (" => ");
         Put_Line (Boolean'Image (Inside (Point, Rect)));
      end Test;
   begin
      Test ((3.0, 2.0), Rect_A);
      Test ((-1.0, -1.0), Rect_A);
      Test ((5.0, 4.0), Rect_A);
   end;

   declare
      procedure Test (Inner, Outer : in Rectangle_Type)
      is begin
         Put ("Inside "); Put (Inner); Put (", "); Put (Outer); Put (" => ");
         Put_Line (Boolean'Image (Inside (Inner, Outer)));
      end Test;
   begin
      Test (Rect_A, Rect_A);
      Test (Rect_B, Rect_A);
      Test (((4.0, 4.0), (5.0, 3.0)), Rect_A);
   end;

   Put ("Make_Rectangle (Center => "); Put (Point_D); Put (" Size => "); Put (Rect_Size); Put_Line (") => ");
   Put (Make_Rectangle (Point_D, Rect_Size));
   New_Line;

   Put_Line ("Update_Bounding_Rect");
   Put ("Rect  => "); Put (Rect_A); New_Line;
   Put ("Point => "); Put (Point_D); New_Line;
   Update_Bounding_Rect (Point_D, Rect_A);
   Put ("      => "); Put (Rect_A);
   New_Line;

   Point_C := (3.0, 2.0);
   Put ("Inner_Size => "); Put (Point_C); New_Line;
   Put ("Outer      => "); Put (Rect_A); New_Line;
   Put ("Left_Top   => "); Put (Center (Point_C, Rect_A));
   New_Line;

end Test_Math_Float_DOF_2;
