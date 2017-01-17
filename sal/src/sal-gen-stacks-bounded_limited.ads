--  Abstract:
--
--  The base generic stack, allowing definite or indefinite limited
--  item types.
--
--  Copyright (C) 1998 - 1999 Stephen Leake.  All Rights Reserved.
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
   type Item_Type (<>) is limited private;
   type Item_Node_Type is private;
   with function To_Item_Node (Item : in Item_Type) return Item_Node_Type;
   with procedure Free_Item (Item : in out Item_Node_Type);
   --  If Item_Type is definite non-limited, Item_Node_Type should
   --  just be Item_Type. Then To_Item_Node should just return Item,
   --  and Free_Item should be null (and both should be inlined). See
   --  SAL.Aux.Definite_Private_Items.
   --
   --  If Item_Type is indefinite, Item_Node_Type should be access
   --  Item_Type. Then To_Item_Node should allocate Item and return a
   --  pointer to it, and Free_Item should be Unchecked_Deallocation.
   --  See SAL.Aux.Indefinite_Private_Items.
   --
   --  To create a stack of limited objects (say of type
   --  Limited_Type), Item_Type can be a non-limited type holding the
   --  parameters needed to create an object (non-limited to allow the
   --  user to create aggregates of creation parameters), and
   --  Item_Node_Type can be 'access Limited_Type'. Then To_Item_Node
   --  must allocate an object of type Limited_Type and initialize it
   --  using the parameters in Item_Type. See
   --  SAL.Aux.Indefinite_Limited_Items.
   --
   --  Other usages may be possible.

package SAL.Gen.Stacks.Bounded_Limited is
   pragma Pure;

   type Stack_Type (Size : Positive) is limited private;
   --  Size is maximum number of items on the stack.

   ------------
   --  Operations on Stack_Type (alphabetical order)

   procedure Clear (Stack : in out Stack_Type);
   --  Empty Stack of all items.
   --
   --  Calls Free_Item on all non-empty Stack slots.

   function Depth (Stack : in Stack_Type) return Natural;
   --  Returns current total items in the Stack
   pragma Inline (Depth);

   function Is_Empty (Stack : in Stack_Type) return Boolean;
   --  Returns true if no items are in Stack.
   pragma Inline (Is_Empty);

   function Is_Full (Stack : in Stack_Type) return Boolean;
   --  Returns true if Stack is full.
   pragma Inline (Is_Full);

   function Max_Depth (Stack : in Stack_Type) return Natural;
   --  Return maximum of Depth since Stack was initialized.
   pragma Inline (Max_Depth);

   function Peek (Stack : in Stack_Type; Index : in Positive) return Item_Node_Type;
   --  Return the Index'th item from the top of Stack; the Item is
   --  _not_ removed. Top item has index 1.
   --
   --  Raises Constraint_Error if Index > Depth.
   pragma Inline (Peek);

   procedure Pop (Stack : in out Stack_Type);
   --  Call Free_Item on top item of Stack, remove it from Stack.
   --
   --  Raises Container_Empty if Is_Empty.
   pragma Inline (Pop);

   procedure Push (Stack : in out Stack_Type; Item : in Item_Type);
   --  Add Item to the top of Stack.
   --
   --  Raises Container_Full if Is_Full.
   pragma Inline (Push);

   function Top (Stack : in Stack_Type) return Item_Node_Type;
   --  Return the item at the top of Stack; the Item is _not_ removed.
   --  Same as Peek (Stack, 1).
   --
   --  Raises Container_Empty if Is_Empty.
   pragma Inline (Top);

private

   type Item_Array_Type is array (Positive range <>) of Item_Node_Type;

   type Stack_Type (Size : Positive) is record
      Top       : Natural         := 0;
      Max_Depth : Natural         := 0;
      Items     : Item_Array_Type (1 .. Size);
   end record;

end SAL.Gen.Stacks.Bounded_Limited;
