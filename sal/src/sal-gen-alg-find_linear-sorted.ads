--  Abstract :
--
--  Utilities for building and maintaining (small) sorted containers.
--
--  Linear searching is used for all searching, so this is appropriate
--  for small containers, containers where comparison costs are low, or
--  where the data is already mostly sorted.
--
--  See SAL.Aux.Sort_Definite_Keys for help in declaring the comparison
--  functions.
--
--  Copyright (C) 2000, 2003, 2007 Stephen Leake.  All Rights Reserved.
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
   with function Is_Equal_Node_Item (Left : in Item_Node_Type; Right : in Item_Type) return Boolean is <>;
   with function Is_Equal_Node_Node (Left, Right : in Item_Node_Type) return Boolean is <>;

   with function Is_Greater_Equal_Node_Item (Left : in Item_Node_Type; Right : in Item_Type) return Boolean is <>;
   with function Is_Greater_Equal_Node_Key (Left : in Item_Node_Type; Right : in Key_Type) return Boolean is <>;
   with function Is_Greater_Equal_Node_Node (Left, Right : in Item_Node_Type) return Boolean is <>;
   --  All order comparisons use one of these functions.
   --
   --  If Key_Type is not Item_Type, all comparisons should, in effect,
   --  use To_Key (Item) for comparing.

   with function Prev_Function (Iterator : in Iterator_Type) return Iterator_Type;
   with procedure Prev_Procedure (Iterator : in out Iterator_Type);

   with procedure Splice_Before
      (Source : in out Container_Type;
       First  : in     Iterator_Type;
       Last   : in     Iterator_Type;
       Dest   : in out Container_Type;
       Before : in     Iterator_Type) is <>;
   --  Standard Iterator operations.

   Allow_Duplicate_Keys : in Boolean := False;
package SAL.Gen.Alg.Find_Linear.Sorted is
   pragma Pure;

   procedure Add
      (Container     : in out Container_Type;
       Item          : in     Item_Type;
       Item_Iterator :    out Iterator_Type;
       Direction     : in     SAL.Direction_Type := SAL.Forward);
   procedure Add
      (Container : in out Container_Type;
       Item      : in     Item_Type;
       Direction : in     SAL.Direction_Type := SAL.Forward);
   --  Insert copy of Item into Container at appropriate place.
   --
   --  If Direction is Forward, search starts at Head (Container).
   --  Otherwise, search starts at Tail (Container).
   --
   --  Item_Iterator points to added Item, for easy access.
   --
   --  Raises Duplicate_Key if Allow_Duplicate_Keys is False and To_Key
   --  (Item) is already in Container.

   procedure Find_Greater_Equal_Item
      (Container : in     Container_Type;
       Item      : in     Item_Type;
       Iterator  :    out Iterator_Type;
       Found     :    out Boolean;
       Direction : in     Direction_Type := Forward);
   procedure Find_Greater_Equal_Key
      (Container : in     Container_Type;
       Key       : in     Key_Type;
       Iterator  :    out Iterator_Type;
       Found     :    out Boolean;
       Direction : in     Direction_Type := Forward);
   procedure Find_Greater_Equal_Node
      (Container : in     Container_Type;
       Item_Node : in     Item_Node_Type;
       Iterator  :    out Iterator_Type;
       Found     :    out Boolean;
       Direction : in     Direction_Type := Forward);
   --  Container must be ordered by Is_Greater_Equal. Set Iterator to
   --  first item where Is_Greater_Equal (Current (Iterator), {Item |
   --  Key | Item_Node}).
   --
   --  Set Found to Is_Equal (Key, Current (Iterator)).
   --
   --  If Direction is Forward, start search at First (Container), and
   --  proceed using Next. If Direction is Backward, start search at
   --  Last (Container), and proceed using Prev.
   --
   --  Item can be inserted into Container using Insert_Before (Iterator).
   --
   --  These are not all named Find_Greater_Equal, because Item_Type,
   --  Item_Node_Type, and Key_Type can all be the same type, in which
   --  case they would be homographs and thus ambiguous.

   procedure Delete
      (Container : in out Container_Type;
       Key       : in     Key_Type;
       Direction : in     SAL.Direction_Type := SAL.Forward);
   --  Delete item matching Key from Container.
   --
   --  If Direction is Forward, search starts at Head (Container).
   --  Otherwise, search starts at Tail (Container).
   --
   --  Raises Not_Found if item matching Key is not found.

   procedure Remove_Out_Of_Order
      (Source : in out Container_Type;
       Dest   : in out Container_Type);
   --  Move all out-of-order items from Source to Dest, in order, using
   --  linear search. No items or nodes are freed or copied.
   --
   --  Note that Dest need not be empty to start with; items from
   --  Source are added in order.
   --
   --  Out-of-order definition is optimized for a mostly sorted Source.

   procedure Merge
      (Container : in out Container_Type;
       From      : in out Container_Type);
   --  Merge From into Container, in order, leaving From empty.
   --  Container and From must be sorted in Is_Greater_Equal order. No
   --  items or nodes are freed or copied.

   procedure Sort
      (Container      : in out Container_Type;
       Temp_Container : in out Container_Type);
   --  Sort Container into Is_Greater_Equal order, using
   --  Remove_Out_Of_Order and Merge. Temp_Container holds the out of
   --  order items temporarily; items in it to begin with are also
   --  merged into Container.
   --
   --  This is efficient if Container is already mostly sorted.
   --
   --  The user must declare Temp_Container, because Container_Type is
   --  indefinite, and Sort would not know how to initialize it.

end SAL.Gen.Alg.Find_Linear.Sorted;
