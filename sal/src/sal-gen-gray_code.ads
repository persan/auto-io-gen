--  Abstract:
--
--  Generic n bit gray codes. Since most gray codes are used with
--  hardware, where individual bit access may be needed, we provide
--  that too.
--
--  A "gray code" satisfies the condition that the binary
--  representation of any two successive values differ by only one
--  bit.
--
--  This package implements this by defining:
--
--  Bi = Gi xor Gi+1 ... Gn-1
--
--  Where Bi is bit i from the n bit binary representation, and Gi is
--  bit i from the n bit gray code representation.
--
--  A lookup table is computed at elaboration time.
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

with Ada.Unchecked_Conversion;
generic
   Bits : in Natural;
   type Binary_Type is mod <>; -- <> must be 2**Bits
package SAL.Gen.Gray_Code is
   pragma Elaborate_Body; -- Elaboration code computes table

   function To_Gray_Code (Binary : in Binary_Type) return Binary_Type;
   pragma Inline (To_Gray_Code);

   function To_Binary (Gray_Code : in Binary_Type) return Binary_Type;
   function From_Gray_Code (Gray_Code : in Binary_Type) return Binary_Type renames To_Binary;
   pragma Inline (To_Binary);

   -----------
   --  Access to individual bits

   type Bit_Array_Type is array (Integer range 0 .. Bits - 1) of Boolean;
   pragma Pack (Bit_Array_Type);
   --  for Bit_Array_Type'Size use Binary_Type'size; -- not static by
   --  LRM:4.9(26)

   function To_Bit_Array is new Ada.Unchecked_Conversion
      (Source => Binary_Type,
       Target => Bit_Array_Type);

   function To_Binary is new Ada.Unchecked_Conversion
      (Source => Bit_Array_Type,
       Target => Binary_Type);

end SAL.Gen.Gray_Code;
