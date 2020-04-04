--  Abstract :
--
--  Run-time utilities used by Auto_Text_IO files generated for access types.
--
--  Copyright (C) 2020  O. Kellogg <okellogg@users.sourceforge.net>
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

with System.Storage_Elements;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Conversion;

package Auto_Text_IO.Access_IO is

   -- 16 hexadecimal characters represent an 8 byte value
   -- (assuming 64 bit addresses)
   subtype Address_String is String (1 .. 16);

   function Unsigned_to_Hex_Str
              (Value : in System.Storage_Elements.Integer_Address)
      return Address_String;

   function Hex_Str_to_Unsigned (S : in String)
            return System.Storage_Elements.Integer_Address;

   function Is_Valid_Hex_String (S : in String) return Boolean;

   use type System.Storage_Elements.Integer_Address;
   use type System.Address;

   -- On initial *write*, the Element will be directly derived from the
   -- Key.
   -- However, on *reading* (or write after read etc), there will be no
   -- such correlation.
   package Address_To_ID is new Ada.Containers.Ordered_Maps
     (Key_Type     => System.Address,
      Element_Type => System.Storage_Elements.Integer_Address);

   -- Reverse mapping is required on reading, for reconstructing
   -- the proper pointer relationships in the user data structures.
   package ID_To_Address is new Ada.Containers.Ordered_Maps
     (Key_Type     => System.Storage_Elements.Integer_Address,
      Element_Type => System.Address);

   Addr2Id_Map : Address_To_ID.Map;

   Id2Addr_Map : ID_To_Address.Map;

   generic
      type T is private;
      type T_Access is access all T;
   package Conversions is

      function To_Address is new Ada.Unchecked_Conversion
                                   (T_Access, System.Address);
      function To_Access  is new Ada.Unchecked_Conversion
                                   (System.Address, T_Access);

      function To_Integer (A : T_Access)
               return System.Storage_Elements.Integer_Address;

      function To_String (A : T_Access) return Address_String;

   end Conversions;

end Auto_Text_IO.Access_IO;

