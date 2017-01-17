--  Abstract:
--
--  see spec.
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

package body SAL.Gen.Gray_Code is

   type Map_Type is array (Binary_Type) of Binary_Type;

   Map,
   Inverse_Map : Map_Type; -- initialized by elaboration code below.

   function To_Gray_Code (Binary : in Binary_Type) return Binary_Type
   is begin
      return Map (Binary);
   end To_Gray_Code;

   function To_Binary (Gray_Code : in Binary_Type) return Binary_Type
   is begin
      return Inverse_Map (Gray_Code);
   end To_Binary;

begin
   declare
      Binary_Bits : Bit_Array_Type;
      Gray_Bits : Bit_Array_Type;
      Binary : Binary_Type;
   begin
      for Gray in Binary_Type loop
         Gray_Bits := To_Bit_Array (Gray);
         Binary_Bits := Gray_Bits;
         for I in Binary_Bits'Range loop
            for J in I + 1 .. Binary_Bits'Last loop
               Binary_Bits (I) := Binary_Bits (I) xor Gray_Bits (J);
            end loop;
         end loop;
         Binary := To_Binary (Binary_Bits);
         Map (Binary) := Gray;
         Inverse_Map (Gray) := Binary;
      end loop;
   end;
end SAL.Gen.Gray_Code;
