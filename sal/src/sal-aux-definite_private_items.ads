--  Abstract :
--
--  Helper package for instantiating containers with a definite
--  non-limited Item type.
--
--  Copyright (C) 1998, 2004 Stephen Leake.  All Rights Reserved.
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

generic
   type Item_Type is private;
package SAL.Aux.Definite_Private_Items is
   pragma Pure;

   function To_Item_Node (Item : in Item_Type) return Item_Type;
   --  Returns Item.

   procedure Free_Item (Item : in out Item_Type);
   --  Does nothing.

   function Copy (Item : in Item_Type) return Item_Type;
   function Copy_Item_Node (Item : in Item_Type) return Item_Type renames Copy;
   --  Returns Item.

   pragma Inline (To_Item_Node, Free_Item, Copy);

end SAL.Aux.Definite_Private_Items;
