--  Abstract:
--
--  Generic utility for binary searching in an ordered container.
--
--  Copyright (C) 1999, 2000 Stephen Leake.  All Rights Reserved.
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

generic
   type Key_Type (<>) is limited private;
   --  Key_Type can be Item_Type, or a value derived from Item_Type.

   with function Is_Less_Equal_Node_Key (Left : in Item_Node_Type; Right : in Key_Type) return Boolean;
   with function Is_Equal_Node_Key (Left  : in Item_Node_Type; Right : in Key_Type) return Boolean;
   with function Is_Greater_Equal_Node_Key (Left  : in Item_Node_Type; Right : in Key_Type) return Boolean;

   with function Middle (First, Last : in Iterator_Type) return Iterator_Type is <>;
   --  Return an iterator that points to an item halfway between First
   --  and Last. If there is no such item, return either First or
   --  Last.

   with procedure Prev (Iterator : in out Iterator_Type) is <>;

package SAL.Gen.Alg.Find_Binary is
   pragma Pure;

   function Find_Less_Equal
      (Container : in Container_Type;
       Key       : in Key_Type)
      return Iterator_Type;
   --  Return an iterator for the item in Container with the largest
   --  key less than or equal to Key. If a sequence of items have keys
   --  equal to Key, return an iterator for the last item in the
   --  sequence.
   --
   --  Return None if no such item exists.

   function Find_Greater_Equal
      (Container : in Container_Type;
       Key       : in Key_Type)
      return Iterator_Type;
   --  Return an iterator for the item in Container with the smallest
   --  key greater than or equal to Key. If a sequence of items have
   --  keys equal to Key, return an iterator for first item in the
   --  sequence.
   --
   --  Return None if no such item exists.

end SAL.Gen.Alg.Find_Binary;
