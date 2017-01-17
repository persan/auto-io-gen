--  Abstract:
--
--  Point and Rectangle operations
--
--  Copyright (C) 2001, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Numerics.Generic_Elementary_Functions;
generic
   --  Auto_Text_IO : ignore
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
package SAL.Gen_Math.Gen_DOF_2 is
   pragma Preelaborate; -- Ada.Strings.Fixed in body.

   type Point_Type is
   record
      X : Real_Type;
      Y : Real_Type;
   end record;

   function Mag   (Right : in Point_Type) return Real_Type;
   function Image (Item  : in Point_Type) return String;
   function Value (Image : in String) return Point_Type;
   --  positional aggregate syntax

   function "*" (Left  : in Point_Type;
                 Right : in Real_Type) return Point_Type;
   function "*" (Left  : in Real_Type;
                 Right : in Point_Type) return Point_Type;
   function "/" (Left  : in Point_Type;
                 Right : in Real_Type) return Point_Type;
   function "+" (Left, Right : in Point_Type) return Point_Type;
   function "-" (Left, Right : in Point_Type) return Point_Type;

   -----------------
   --  Rectangles

   type Rectangle_Type is record
      Left_Bottom : Point_Type;
      Right_Top   : Point_Type;
      --  Left_Bottom.x < Right_Top.x
      --  Left_Bottom.y < Right_Top.y
   end record;

   function Image (Item  : in Rectangle_Type) return String;
   function Value (Image : in String) return Rectangle_Type;
   function Size  (Item  : in Rectangle_Type) return Point_Type;
   --  size of rectangle: X is width Y is height

   function "+" (Left, Right : in Rectangle_Type) return Rectangle_Type;
   function "-" (Left, Right : in Rectangle_Type) return Rectangle_Type;
   --  Add, subtract corner points

   function "+" (Left : in Rectangle_Type; Right : in Point_Type) return Rectangle_Type;
   --  Add Right to corners of Left.

   function Clip (Inner, Outer : in Rectangle_Type) return Rectangle_Type;
   --  Return Inner clipped to fit inside Outer.

   function Inside
      (Point : in Point_Type;
       Outer : in Rectangle_Type)
      return Boolean;
   --  returns True if Point is inside or just touching Outer.

   function Inside
      (Inner : in Rectangle_Type;
       Outer : in Rectangle_Type)
      return Boolean;
   --  returns True if Inner is fully inside or just touching Outer.

   pragma Inline (Inside);

   function Make_Rectangle
      (Center : in Point_Type;
       Size   : in Point_Type)
      return Rectangle_Type;
   --  Create rectangle centered at Center, with size Size.

   procedure Update_Bounding_Rect
      (Point : in     Point_Type;
       Rect  : in out Rectangle_Type);
   --  expand Rect to contain Point.

   function Center
      (Inner_Size : in Point_Type;
       Outer      : in Rectangle_Type)
      return Point_Type;
   --  Compute Left_Bottom for a rectangle of size Inner_Size so it
   --  will be centered in Outer.

end SAL.Gen_Math.Gen_DOF_2;
