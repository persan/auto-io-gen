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
with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Conversion;

package Auto_Text_IO.Access_IO is

   type ID_T is new System.Storage_Elements.Integer_Address;

   package ID_IO is new Ada.Text_IO.Modular_IO (ID_T);

   use type System.Address;

   -- Forward mapping for writing keeps track of which pointers
   -- were written and which were not.
   -- Pointers which were not yet written create a "definition" (#num)
   -- Pointers which were written before create a "reference" (^num)
   package Address_To_ID is new Ada.Containers.Ordered_Maps
     (Key_Type     => System.Address,
      Element_Type => ID_T);

   -- Reverse mapping is required on reading, for reconstructing
   -- the proper pointer relationships in the user data structures.
   package ID_To_Address is new Ada.Containers.Ordered_Maps
     (Key_Type     => ID_T,
      Element_Type => System.Address);

   Addr2Id_Map : Address_To_ID.Map;

   Id2Addr_Map : ID_To_Address.Map;

   -- Return stringified ID without leading space.
   function To_String (ID : ID_T) return String;

   Count : ID_T := 0;

   function Next_ID return ID_T;   -- increment Count and return it

   procedure Reset;                -- reset Count to 0 and reset the maps

   generic
      type T is private;
      type T_Access is access all T;
   package Conversions is

      function To_Address is new Ada.Unchecked_Conversion
                                   (T_Access, System.Address);
      function To_Access  is new Ada.Unchecked_Conversion
                                   (System.Address, T_Access);

   end Conversions;

end Auto_Text_IO.Access_IO;

