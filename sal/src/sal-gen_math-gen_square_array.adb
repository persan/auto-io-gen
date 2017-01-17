--  Abstract :
--
--  see spec
--
--  Copyright (C) 2001 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_Square_Array is

   function Identity return Array_Type
   is
      Result : Array_Type := (others => (others => 0.0));
   begin
      for I in Result'Range loop
         Result (I)(I) := 1.0;
      end loop;
      return Result;
   end Identity;

   function "-" (Right : in Array_Type) return Array_Type
   is
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for K in Index_Type loop
            Result (I) (K) := -Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "-";

   function "-" (Left, Right : in Array_Type) return Array_Type
   is
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for K in Index_Type loop
            Result (I) (K) := Left (I) (K) - Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "-";

   function "+" (Left, Right : in Array_Type) return Array_Type
   is
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for K in Index_Type loop
            Result (I) (K) := Left (I) (K) + Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "+";

   function "*" (Left, Right : in Array_Type) return Array_Type
   is
      Temp   : Real_Type;
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            Temp := 0.0;
            for K in Index_Type loop
               Temp := Temp + Left (I) (K) * Right (K) (J);
            end loop;
            Result (I) (J) := Temp;
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : in Real_Type; Right : in Array_Type) return Array_Type
   is
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for K in Index_Type loop
            Result (I) (K) := Left * Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : in Array_Type; Right : in Row_Type) return Row_Type
   is
      Temp   : Real_Type;
      Result : Row_Type;
   begin
      for I in Index_Type loop
         Temp := 0.0;
         for K in Index_Type loop
            Temp := Temp + Left (I) (K) * Right (K);
         end loop;
         Result (I) := Temp;
      end loop;
      return Result;
   end "*";

   function Square_Right (Right : in Array_Type) return Array_Type
   is
      Temp   : Real_Type;
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for J in Index_Type'First .. I loop
            Temp := 0.0;
            for K in Index_Type loop
               Temp := Temp + Right (I) (K) * Right (J) (K);
            end loop;
            Result (I) (J) := Temp;
            Result (J) (I) := Temp;
         end loop;
      end loop;
      return Result;
   end Square_Right;

   function Square_Left (Right : in Array_Type) return Array_Type
   is
      Temp   : Real_Type;
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for J in Index_Type'First .. I loop
            Temp := 0.0;
            for K in Index_Type loop
               Temp := Temp + Right (K) (I) * Right (K) (J);
            end loop;
            Result (I) (J) := Temp;
            Result (J) (I) := Temp;
         end loop;
      end loop;
      return Result;
   end Square_Left;

   function Times_Diag (Left : in Array_Type; Right : in Row_Type) return Array_Type
   is
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            Result (I) (J) := Left (I) (J) * Right (J);
         end loop;
      end loop;
      return Result;
   end Times_Diag;

   function Diag_Times (Left : in Row_Type; Right : in Array_Type) return Array_Type
   is
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            Result (I) (J) := Left (I) * Right (I) (J);
         end loop;
      end loop;
      return Result;
   end Diag_Times;

   function Transpose_Times (Left, Right : in Array_Type) return Array_Type
   is
      Temp   : Real_Type;
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            Temp := 0.0;
            for K in Index_Type
            loop
               Temp := Temp + Left (K) (I) * Right (K) (J);
            end loop;
            Result (I) (J) := Temp;
         end loop;
      end loop;
      return Result;
   end Transpose_Times;

   function Times_Transpose (Left, Right : in Array_Type) return Array_Type
   is
      Temp   : Real_Type;
      Result : Array_Type;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            Temp := 0.0;
            for K in Index_Type loop
               Temp := Temp + Left (I) (K) * Right (J) (K);
            end loop;
            Result (I) (J) := Temp;
         end loop;
      end loop;
      return Result;
   end Times_Transpose;

   function Transpose_Times (Left : in Array_Type; Right : in Row_Type) return Row_Type
   is
      Temp   : Real_Type;
      Result : Row_Type;
   begin
      for I in Index_Type loop
         Temp := 0.0;
         for K in Index_Type loop
            Temp := Temp + Left (K) (I) * Right (K);
         end loop;
         Result (I) := Temp;
      end loop;
      return Result;
   end Transpose_Times;

end SAL.Gen_Math.Gen_Square_Array;
