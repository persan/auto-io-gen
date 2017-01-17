--  Abstract :
--
--  Helper package for instantiating containers with a non-limited
--  indefinite item type.
--
--  Copyright (C) 1998, 2003, 2004 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.

with Ada.Unchecked_Deallocation;
generic
   type Item_Type (<>) is private;
   type Item_Access_Type is access Item_Type;
package SAL.Aux.Indefinite_Private_Items is
   pragma Preelaborate; -- access types

   function Copy (Item : in Item_Access_Type) return Item_Access_Type;
   function Copy_Item_Node (Item : in Item_Access_Type) return Item_Access_Type
     renames Copy;
   function New_Item (Item : in Item_Type) return Item_Access_Type;
   function To_Item_Node (Item : in Item_Type) return Item_Access_Type renames New_Item;
   function To_Item (Item : in Item_Access_Type) return Item_Type;
   procedure Free_Item is new Ada.Unchecked_Deallocation (Item_Type, Item_Access_Type);

   pragma Inline (New_Item, To_Item);

end SAL.Aux.Indefinite_Private_Items;
