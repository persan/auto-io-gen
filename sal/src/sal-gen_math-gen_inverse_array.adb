--  Abstract:
--
--  see spec
--
--  Copyright (C) 2002 Stephen Leake.  All Rights Reserved.
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
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--

package body SAL.Gen_Math.Gen_Inverse_Array is

   function "-" (Right : in A_Array_BAR_Type) return A_Array_BAR_Type
   is
      Result : A_Array_BAR_Type;
   begin
      for I in A_Index_Type loop
         for K in B_Index_Type loop
            Result (I) (K) := -Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "-";

   function "-" (Left, Right : in A_Array_BAR_Type) return A_Array_BAR_Type
   is
      Result : A_Array_BAR_Type;
   begin
      for I in A_Index_Type loop
         for K in B_Index_Type loop
            Result (I) (K) := Left (I) (K) - Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "-";

   function "+" (Left, Right : in A_Array_BAR_Type) return A_Array_BAR_Type
   is
      Result : A_Array_BAR_Type;
   begin
      for I in A_Index_Type
      loop
         for K in B_Index_Type
         loop
            Result (I) (K) := Left (I) (K) + Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "+";

   function "*" (Left : in Real_Type; Right : in A_Array_BAR_Type) return A_Array_BAR_Type
   is
      Result : A_Array_BAR_Type;
   begin
      for I in A_Index_Type
      loop
         for K in B_Index_Type
         loop
            Result (I) (K) := Left * Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "*";

   function "-" (Right : in B_Array_AAR_Type) return B_Array_AAR_Type
   is
      Result : B_Array_AAR_Type;
   begin
      for I in B_Index_Type
      loop
         for K in A_Index_Type
         loop
            Result (I) (K) := -Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "-";

   function "-" (Left, Right : in B_Array_AAR_Type) return B_Array_AAR_Type
   is
      Result : B_Array_AAR_Type;
   begin
      for I in B_Index_Type
      loop
         for K in A_Index_Type
         loop
            Result (I) (K) := Left (I) (K) - Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "-";

   function "+" (Left, Right : in B_Array_AAR_Type) return B_Array_AAR_Type
   is
      Result : B_Array_AAR_Type;
   begin
      for I in B_Index_Type
      loop
         for K in A_Index_Type
         loop
            Result (I) (K) := Left (I) (K) + Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "+";

   function "*" (Left : in Real_Type; Right : in B_Array_AAR_Type) return B_Array_AAR_Type
   is
      Result : B_Array_AAR_Type;
   begin
      for I in B_Index_Type
      loop
         for K in A_Index_Type
         loop
            Result (I) (K) := Left * Right (I) (K);
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : in A_Array_BAR_Type; Right : in B_Array_Real_Type) return A_Array_Real_Type
   is
      Temp   : Real_Type;
      Result : A_Array_Real_Type;
   begin
      for I in A_Index_Type
      loop
         Temp := 0.0;
         for K in B_Index_Type
         loop
            Temp := Temp + Left (I) (K) * Right (K);
         end loop;
         Result (I) := Temp;
      end loop;
      return Result;
   end "*";

   function "*" (Left : in A_Array_BAR_Type; Right : in A_Array_Real_Type) return B_Array_Real_Type
   is
      Temp   : Real_Type;
      Result : B_Array_Real_Type;
   begin
      for I in B_Index_Type
      loop
         Temp := 0.0;
         for K in A_Index_Type
         loop
            Temp := Temp + Left (K) (I) * Right (K);
         end loop;
         Result (I) := Temp;
      end loop;
      return Result;
   end "*";

   function "*" (Left : in B_Array_AAR_Type; Right : in A_Array_Real_Type) return B_Array_Real_Type
   is
      Temp   : Real_Type;
      Result : B_Array_Real_Type;
   begin
      for I in B_Index_Type
      loop
         Temp := 0.0;
         for K in A_Index_Type
         loop
            Temp := Temp + Left (I) (K) * Right (K);
         end loop;
         Result (I) := Temp;
      end loop;
      return Result;
   end "*";

   function "*" (Left : in B_Array_AAR_Type; Right : in B_Array_Real_Type) return A_Array_Real_Type
   is
      Temp   : Real_Type;
      Result : A_Array_Real_Type;
   begin
      for I in A_Index_Type
      loop
         Temp := 0.0;
         for K in B_Index_Type
         loop
            Temp := Temp + Left (K) (I) * Right (K);
         end loop;
         Result (I) := Temp;
      end loop;
      return Result;
   end "*";

   function Square_Right (Right : in A_Array_BAR_Type) return A_Array_AAR_Type
   is
      Temp   : Real_Type;
      Result : A_Array_AAR_Type;
   begin
      for I in A_Index_Type
      loop
         for J in A_Index_Type
         loop
            Temp := 0.0;
            for K in B_Index_Type
            loop
               Temp := Temp + Right (I) (K) * Right (J) (K);
            end loop;
            Result (I) (J) := Temp;
         end loop;
      end loop;
      return Result;
   end Square_Right;

   function "*" (Left : in A_Array_BAR_Type; Right : in B_Array_AAR_Type) return A_Array_AAR_Type
   is
      Temp   : Real_Type;
      Result : A_Array_AAR_Type;
   begin
      for I in A_Index_Type
      loop
         for J in A_Index_Type
         loop
            Temp := 0.0;
            for K in B_Index_Type
            loop
               Temp := Temp + Left (I) (K) * Right (K) (J);
            end loop;
            Result (I) (J) := Temp;
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : in B_Array_AAR_Type; Right : in A_Array_BAR_Type) return B_Array_BAR_Type
   is
      Temp   : Real_Type;
      Result : B_Array_BAR_Type;
   begin
      for I in B_Index_Type
      loop
         for J in B_Index_Type
         loop
            Temp := 0.0;
            for K in A_Index_Type
            loop
               Temp := Temp + Left (I) (K) * Right (K) (J);
            end loop;
            Result (I) (J) := Temp;
         end loop;
      end loop;
      return Result;
   end "*";

   function "*" (Left : in A_Array_BAR_Type; Right : in A_Array_AAR_Type) return B_Array_AAR_Type
   is
      Temp   : Real_Type;
      Result : B_Array_AAR_Type;
   begin
      for I in B_Index_Type
      loop
         for J in A_Index_Type
         loop
            Temp := 0.0;
            for K in A_Index_Type
            loop
               Temp := Temp + Left (K) (I) * Right (K) (J);
            end loop;
            Result (I) (J) := Temp;
         end loop;
      end loop;
      return Result;
   end "*";

   function Inverse (Right : in A_Array_BAR_Type) return B_Array_AAR_Type
   is
   begin
      return Right * Inverse (Square_Right (Right));
   end Inverse;

end SAL.Gen_Math.Gen_Inverse_Array;
