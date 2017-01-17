--  Abstract
--
--  see spec
--
--  Copyright (C) 2001, 2002, 2003 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Ada.Strings.Fixed;
package body SAL.Gen_Math.Gen_DOF_2 is

   function Mag (Right : in Point_Type) return Real_Type
   is
   begin
      return Elementary.Sqrt (Right.X * Right.X + Right.Y * Right.Y);
   end Mag;

   function Image (Item : in Point_Type) return String
   is begin
      return "(" & Real_Type'Image (Item.X) & ", " & Real_Type'Image (Item.Y) & ")";
   end Image;

   function Value (Image : in String) return Point_Type
   is
      Right_Paren : constant Natural := Ada.Strings.Fixed.Index (Image, "(");
      Comma       : constant Natural := Ada.Strings.Fixed.Index (Image, ",");
      Left_Paren  : constant Natural := Ada.Strings.Fixed.Index (Image, ")");
   begin
      return (Real_Type'Value (Image (Right_Paren + 1 .. Comma - 1)),
              Real_Type'Value (Image (Comma + 1 .. Left_Paren - 1)));
   end Value;

   function "*" (Left  : in Point_Type;
                 Right : in Real_Type) return Point_Type
   is begin
      return (Left.X * Right, Left.Y * Right);
   end "*";

   function "*" (Left  : in Real_Type;
                 Right : in Point_Type) return Point_Type
   is begin
      return (Left * Right.X, Left * Right.Y);
   end "*";

   function "/" (Left  : in Point_Type;
                 Right : in Real_Type) return Point_Type
   is begin
      return (Left.X / Right, Left.Y / Right);
   end "/";

   function "+" (Left, Right : in Point_Type) return Point_Type
   is begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function "-" (Left, Right : in Point_Type) return Point_Type
   is begin
      return (Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   ----------------
   --  Rectangles

   function Image (Item : in Rectangle_Type) return String
   is begin
      return "(" & Image (Item.Left_Bottom) & ", " & Image (Item.Right_Top) & ")";
   end Image;

   function Value (Image : in String) return Rectangle_Type
   is
      First_Point  : constant Natural := Ada.Strings.Fixed.Index (Image, "(");
      Second_Point : constant Natural := Ada.Strings.Fixed.Index (Image, "),");
   begin
      return (Value (Image (First_Point + 1 .. Second_Point + 1)),
              Value (Image (Second_Point + 2 .. Image'Last)));
   end Value;

   function Size (Item : in Rectangle_Type) return Point_Type
   is begin
      return (Item.Right_Top - Item.Left_Bottom);
   end Size;

   function "+" (Left, Right : in Rectangle_Type) return Rectangle_Type
   is begin
      return (Left.Left_Bottom + Right.Left_Bottom, Left.Right_Top + Right.Right_Top);
   end "+";

   function "-" (Left, Right : in Rectangle_Type) return Rectangle_Type
   is begin
      return (Left.Left_Bottom - Right.Left_Bottom, Left.Right_Top - Right.Right_Top);
   end "-";

   function "+" (Left : in Rectangle_Type; Right : in Point_Type) return Rectangle_Type
   is begin
      return (Left.Left_Bottom + Right, Left.Right_Top + Right);
   end "+";

   function Clip (Inner, Outer : in Rectangle_Type) return Rectangle_Type
   is begin
      return
         (Left_Bottom =>
             (X => Real_Type'Min (Real_Type'Max (Inner.Left_Bottom.X, Outer.Left_Bottom.X), Outer.Right_Top.X),
              Y => Real_Type'Min (Real_Type'Max (Inner.Left_Bottom.Y, Outer.Left_Bottom.Y), Outer.Right_Top.Y)),
          Right_Top =>
             (X => Real_Type'Min (Real_Type'Max (Inner.Right_Top.X, Outer.Left_Bottom.X), Outer.Right_Top.X),
              Y => Real_Type'Min (Real_Type'Max (Inner.Right_Top.Y, Outer.Left_Bottom.Y), Outer.Right_Top.Y)));
   end Clip;

   function Inside
      (Point : in Point_Type;
       Outer : in Rectangle_Type)
      return Boolean
   is begin
      return
         Point.X >= Outer.Left_Bottom.X and
         Point.X <= Outer.Right_Top.X and
         Point.Y >= Outer.Left_Bottom.Y and
         Point.Y <= Outer.Right_Top.Y;
   end Inside;

   function Inside
      (Inner : in Rectangle_Type;
       Outer : in Rectangle_Type)
      return Boolean
   is begin
      return Inside (Inner.Left_Bottom, Outer) and Inside (Inner.Right_Top, Outer);
   end Inside;

   function Make_Rectangle
     (Center : in Point_Type;
      Size   : in Point_Type) return Rectangle_Type
   is begin
      return
         (Left_Bottom =>
             (X => Center.X - Size.X/2.0,
              Y => Center.Y - Size.Y/2.0),
          Right_Top =>
             (X => Center.X + Size.X/2.0,
              Y => Center.Y + Size.Y/2.0));
   end Make_Rectangle;

   procedure Update_Bounding_Rect
      (Point : in     Point_Type;
       Rect  : in out Rectangle_Type)
   is begin
      if Point.X < Rect.Left_Bottom.X then
         Rect.Left_Bottom.X := Point.X;
      elsif Point.X > Rect.Right_Top.X then
         Rect.Right_Top.X := Point.X;
      end if;

      if Point.Y < Rect.Left_Bottom.Y then
         Rect.Left_Bottom.Y := Point.Y;
      elsif Point.Y > Rect.Right_Top.Y then
         Rect.Right_Top.Y := Point.Y;
      end if;
   end Update_Bounding_Rect;

   function Center
      (Inner_Size : in Point_Type;
       Outer      : in Rectangle_Type)
      return Point_Type
   is
      Outer_Center : constant Point_Type := (Outer.Left_Bottom + Outer.Right_Top) / 2.0;
   begin
      return Outer_Center - Inner_Size / 2.0;
   end Center;

end SAL.Gen_Math.Gen_DOF_2;
