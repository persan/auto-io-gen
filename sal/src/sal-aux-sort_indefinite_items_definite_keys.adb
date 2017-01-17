--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2000 Stephen Leake.  All Rights Reserved.
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

package body SAL.Aux.Sort_Indefinite_Items_Definite_Keys is

   function Is_Equal_Node_Item
      (Left  : in Item_Node_Type;
       Right : in Item_Type)
      return Boolean
   is begin
      return Is_Equal (To_Key_Node (Left), To_Key_Item (Right));
   end Is_Equal_Node_Item;

   function Is_Equal_Node_Key
      (Left  : in Item_Node_Type;
       Right : in Key_Type)
      return Boolean
   is begin
      return Is_Equal (To_Key_Node (Left), Right);
   end Is_Equal_Node_Key;

   function Is_Equal_Node_Node
     (Left, Right : in Item_Node_Type)
      return Boolean
   is begin
      return Is_Equal (To_Key_Node (Left), To_Key_Node (Right));
   end Is_Equal_Node_Node;

   function Is_Greater_Equal_Node_Item
      (Left  : in Item_Node_Type;
       Right : in Item_Type)
      return Boolean
   is begin
      return Is_Greater_Equal (To_Key_Node (Left), To_Key_Item (Right));
   end Is_Greater_Equal_Node_Item;

   function Is_Greater_Equal_Node_Key
      (Left  : in Item_Node_Type;
       Right : in Key_Type)
      return Boolean
   is begin
      return Is_Greater_Equal (To_Key_Node (Left), Right);
   end Is_Greater_Equal_Node_Key;

   function Is_Greater_Equal_Node_Node
     (Left, Right : in Item_Node_Type)
      return Boolean
   is begin
      return Is_Greater_Equal (To_Key_Node (Left), To_Key_Node (Right));
   end Is_Greater_Equal_Node_Node;

end SAL.Aux.Sort_Indefinite_Items_Definite_Keys;
