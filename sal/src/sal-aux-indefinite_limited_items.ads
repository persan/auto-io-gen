--  Abstract :
--
--  Helper package for instantiating containers with a limited item
--  type.
--
--  Copyright (C) 1998, 2000, 2004 Stephen Leake.  All Rights Reserved.
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
--
with Ada.Unchecked_Deallocation;
generic
   type Create_Parameters_Type (<>) is private;
   type Limited_Type (<>) is limited private;
   type Item_Access_Type is access Limited_Type;

   with function Allocate_Item (Parameters : in Create_Parameters_Type) return Item_Access_Type;
   --  Allows setting discriminants of Limited_Type when item is
   --  allocated.

   with procedure Initialize_Item (Item : in out Limited_Type; Parameters : in Create_Parameters_Type);
   --  Called after item is allocated.

package SAL.Aux.Indefinite_Limited_Items is
   pragma Preelaborate; --  Item_Access_Type is an access type.

   function New_Item (Parameters : in Create_Parameters_Type) return Item_Access_Type;
   function To_Item_Node (Parameters : in Create_Parameters_Type) return Item_Access_Type
     renames New_Item;

   procedure Free_Item is new Ada.Unchecked_Deallocation (Limited_Type, Item_Access_Type);

   function No_Copy (Source : in Item_Access_Type) return Item_Access_Type;
   function Copy_Item_Node (Source : in Item_Access_Type) return Item_Access_Type
     renames No_Copy;
   --  Limited items can't be copied, but most containers support
   --  non-limited items, and thus require a Copy (Item) function.
   --  This function raises SAL.Invalid_Operation.
   --
   --  Note that this means some errors will be detected at run-time,
   --  not compile-time. The alternative is to provide a version of
   --  each container that only supports limited items.

end SAL.Aux.Indefinite_Limited_Items;
