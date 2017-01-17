--  Abstract:
--
--  All reasonable operations on 1 dimensional vectors with real
--  elements.
--
--  Design:
--
--  The array types are generic formal parameters, instead of being
--  declared here, to allow users to choose their own names without
--  having to use subtypes. This makes it possible to simply
--  instantiate this package, and not rename all the operators.
--
--  Copyright (C) 2001, 2006 Stephen Leake.  All Rights Reserved.
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
with SAL.Gen_Math.Gen_Scalar;
generic
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
   with package Math_Scalar is new SAL.Gen_Math.Gen_Scalar (Elementary);

   type Index_Type is (<>);

   type Index_Array_Boolean_Type is array (Index_Type) of Boolean;
   type Index_Array_Real_Type is array (Index_Type) of Real_Type;
   type Index_Array_Limit_Type is array (Index_Type) of Math_Scalar.Limit_Type;

package SAL.Gen_Math.Gen_Vector is
   pragma Pure;

   function Any (Item : in Index_Array_Boolean_Type) return Boolean;
   --  If any of the members of the array is True then result is True;
   --  else return False.

   function "-" (Right : in Index_Array_Real_Type) return Index_Array_Real_Type;
   function "abs" (Item : in Index_Array_Real_Type) return Index_Array_Real_Type;
   --  Unary operations applied element by element.

   function "+" (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type;
   function "-" (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type;
   function "*" (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type;
   function "/" (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type;
   --  Operation applied element by element


   function "+" (Left : in Index_Array_Real_Type; Right : in Real_Type) return Index_Array_Real_Type;
   function "-" (Left : in Index_Array_Real_Type; Right : in Real_Type) return Index_Array_Real_Type;
   function "*" (Left : in Index_Array_Real_Type; Right : in Real_Type) return Index_Array_Real_Type;
   function "/" (Left : in Index_Array_Real_Type; Right : in Real_Type) return Index_Array_Real_Type;
   --  Operation applied element by element


   function "+" (Left : in Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type;
   function "-" (Left : in Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type;
   function "*" (Left : in Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type;
   function "/" (Left : in Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type;
   --  Operation applied element by element

   function Interpolate
     (X  : in Real_Type;
      X1 : in Real_Type;
      X2 : in Real_Type;
      Y1 : in Index_Array_Real_Type;
      Y2 : in Index_Array_Real_Type)
     return Index_Array_Real_Type;
   --  Given Y1 = f (X1), Y2 = f (X2), return Y = f (X), using linear interpolation.

   function Dot (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Real_Type;
   function "*" (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Real_Type
      renames Dot;
   --  Dot product

   function Mask (Item : in Index_Array_Real_Type; Mask : in Index_Array_Boolean_Type) return Index_Array_Real_Type;
   --  A value of TRUE in Mask sets the corresponding element of Item to
   --  0.0.
   --
   --  `Select' was the prefered name, with opposite sense, but that's
   --  an Ada reserved word.


   -----------
   --  The following functions call the corresponding Math_Scalar
   --  function for each element.

   function Dead_Band
      (Item        : in Index_Array_Real_Type;
       Lower_Limit : in Index_Array_Real_Type)
       return Index_Array_Real_Type;

   function Detent
      (Item        : in Index_Array_Real_Type;
       Dead_Band   : in Index_Array_Real_Type;
       Upper_Limit : in Index_Array_Real_Type)
       return Index_Array_Real_Type;

   function To_Limit (Low, High : in Index_Array_Real_Type) return Index_Array_Limit_Type;

   function "and" (Left, Right : in Index_Array_Limit_Type) return Index_Array_Limit_Type;

   procedure Clip
      (Item    : in out Index_Array_Real_Type;
       Limit   : in     Index_Array_Limit_Type;
       Clipped :    out Index_Array_Boolean_Type);

   function "<="
      (Item  : in     Index_Array_Real_Type;
       Limit : in     Index_Array_Limit_Type)
       return Boolean;

   procedure Scale_Limit
      (Item   : in out Index_Array_Real_Type;
       Limit  : in     Real_Type;
       Scaled :    out Boolean);
   --  Scale all elements of Item so they are all within +-|Limit|,
   --  but maintain direction in the vector space. Scaled is True if
   --  Item is modified, False if not.

end SAL.Gen_Math.Gen_Vector;
