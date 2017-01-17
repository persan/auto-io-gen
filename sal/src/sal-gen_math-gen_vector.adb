--  Abstract:
--
--  see spec
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

package body SAL.Gen_Math.Gen_Vector is

   function Any (Item : in Index_Array_Boolean_Type) return Boolean
   is begin
      for I in Item'Range loop
         if Item (I) then
            return True;
         end if;
      end loop;
      return False;
   end Any;

   function "-" (Right : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Right'Range
      loop
         Result (I) := -Right (I);
      end loop;
      return Result;
   end "-";

   function "abs" (Item : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := abs Item (I);
      end loop;
      return Result;
   end "abs";

   function "+" (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left (I) + Right (I);
      end loop;
      return Result;
   end "+";

   function "-" (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left (I) - Right (I);
      end loop;
      return Result;
   end "-";

   function "*" (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left (I) * Right (I);
      end loop;
      return Result;
   end "*";

   function "/" (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left (I) / Right (I);
      end loop;
      return Result;
   end "/";

   --  array op element

   function "+" (Left : in Index_Array_Real_Type; Right : in Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left (I) + Right;
      end loop;
      return Result;
   end "+";

   function "-" (Left : in Index_Array_Real_Type; Right : in Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left (I) - Right;
      end loop;
      return Result;
   end "-";

   function "*" (Left : in Index_Array_Real_Type; Right : in Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left (I) * Right;
      end loop;
      return Result;
   end "*";

   function "/" (Left : in Index_Array_Real_Type; Right : in Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left (I) / Right;
      end loop;
      return Result;
   end "/";

   --  element op array, dot

   function "+" (Left : in Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left + Right (I);
      end loop;
      return Result;
   end "+";

   function "-" (Left : in Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left - Right (I);
      end loop;
      return Result;
   end "-";

   function "*" (Left : in Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left * Right (I);
      end loop;
      return Result;
   end "*";

   function "/" (Left : in Real_Type; Right : in Index_Array_Real_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Left / Right (I);
      end loop;
      return Result;
   end "/";

   function Interpolate
     (X  : in Real_Type;
      X1 : in Real_Type;
      X2 : in Real_Type;
      Y1 : in Index_Array_Real_Type;
      Y2 : in Index_Array_Real_Type)
     return Index_Array_Real_Type
   is begin
      return Y1 + ((Y2 - Y1) / (X2 - X1)) * (X - X1);
   end Interpolate;

   function Dot (Left : in Index_Array_Real_Type; Right : in Index_Array_Real_Type) return Real_Type
   is
      Result : Real_Type := 0.0;
   begin
      for I in Left'Range
      loop
         Result := Result + Left (I) * Right (I);
      end loop;
      return Result;
   end Dot;

   --  Mask, Dead_Band, Detent

   function Mask (Item : in Index_Array_Real_Type; Mask : in Index_Array_Boolean_Type) return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         if Mask (I) then
            Result (I) := 0.0;
         else
            Result (I) := Item (I);
         end if;
      end loop;
      return Result;
   end Mask;

   function Dead_Band
      (Item        : in Index_Array_Real_Type;
       Lower_Limit : in Index_Array_Real_Type)
      return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Math_Scalar.Dead_Band (Item (I), Lower_Limit (I));
      end loop;
      return Result;
   end Dead_Band;

   function Detent
      (Item        : in Index_Array_Real_Type;
       Dead_Band   : in Index_Array_Real_Type;
       Upper_Limit : in Index_Array_Real_Type)
      return Index_Array_Real_Type
   is
      Result : Index_Array_Real_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Math_Scalar.Detent (Item (I), Dead_Band (I), Upper_Limit (I));
      end loop;
      return Result;
   end Detent;

   --  limits

   function To_Limit (Low, High : in Index_Array_Real_Type) return Index_Array_Limit_Type
   is
      Result : Index_Array_Limit_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Math_Scalar.To_Limit (Low => Low (I), High => High (I));
      end loop;
      return Result;
   end To_Limit;

   function "and" (Left, Right : in Index_Array_Limit_Type) return Index_Array_Limit_Type
   is
      Result : Index_Array_Limit_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := Math_Scalar."and" (Left (I), Right (I));
      end loop;
      return Result;
   end "and";

   procedure Clip
      (Item    : in out Index_Array_Real_Type;
       Limit   : in     Index_Array_Limit_Type;
       Clipped : out    Index_Array_Boolean_Type)
   is
   begin
      for I in Item'Range
      loop
         Math_Scalar.Clip (Item (I), Limit (I), Clipped (I));
      end loop;
   end Clip;

   function "<="
      (Item    : in     Index_Array_Real_Type;
       Limit   : in     Index_Array_Limit_Type)
   return Boolean
   is
      Result : Boolean := True;
   begin
      for I in Item'Range
      loop
         Result := Result and Math_Scalar."<=" (Item (I), Limit (I));
      end loop;
      return Result;
   end "<=";

   procedure Scale_Limit
      (Item   : in out Index_Array_Real_Type;
       Limit  : in     Real_Type;
       Scaled : out    Boolean)
   is
      Scale : Real_Type := 1.0;
      Temp  : Real_Type;
   begin
      for I in Item'Range loop
         if abs Item (I) > abs Limit then
            Temp := abs Limit / abs Item (I);
            if Temp < Scale then
               Scale := Temp;
            end if;
         end if;
      end loop;
      if Scale < 1.0 then
         Scaled := True;
         Item := Item * Scale;
      else
         Scaled := False;
      end if;

   end Scale_Limit;

end SAL.Gen_Math.Gen_Vector;
