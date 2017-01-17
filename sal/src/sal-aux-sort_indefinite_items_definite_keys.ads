--  Abstract :
--
--  Helper package for instantiating sort algorithms with an access
--  node type and a definite key type.
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

generic
   type Item_Type (<>) is limited private;
   type Item_Node_Type is private;
   type Key_Type is private;
   with function To_Key_Item (Item : in Item_Type) return Key_Type;
   with function To_Key_Node (Item : in Item_Node_Type) return Key_Type;
   with function Is_Equal (Left, Right : in Key_Type) return Boolean;
   with function Is_Greater_Equal (Left, Right : in Key_Type) return Boolean;
package SAL.Aux.Sort_Indefinite_Items_Definite_Keys is
   pragma Pure;

   function Is_Equal_Node_Item (Left : in Item_Node_Type; Right : in Item_Type) return Boolean;
   function Is_Equal_Node_Key (Left : in Item_Node_Type; Right : in Key_Type) return Boolean;
   function Is_Equal_Node_Node (Left, Right : in Item_Node_Type) return Boolean;

   function Is_Greater_Equal_Node_Item (Left : in Item_Node_Type; Right : in Item_Type) return Boolean;
   function Is_Greater_Equal_Node_Key (Left : in Item_Node_Type; Right : in Key_Type) return Boolean;
   function Is_Greater_Equal_Node_Node (Left, Right : in Item_Node_Type) return Boolean;

end SAL.Aux.Sort_Indefinite_Items_Definite_Keys;
