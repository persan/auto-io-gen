--  Abstract:
--
--  The base non-limited generic stack, allowing definite non-limited
--  item types.
--
--  For indefinite and/or limited item types, see
--  SAL.Gen.Stacks.Bounded_Limted. The restrictions allow a simpler
--  implementation, and allow stack copy by simple assignment. Note
--  that we can't use a Controlled type to fix assignment, because the
--  SAL.Gen uses _no_ tagged types.
--
--  Copyright (C) 1999, 2003 Stephen Leake.  All Rights Reserved.
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
   Null_Item : in Item_Type;
package SAL.Gen.Stacks.Bounded_Nonlimited is
   pragma Pure;

   type Stack_Type (Size : Positive) is private;
   --  Size is maximum number of items on the stack.

   ------------
   --  Operations on Stack_Type (alphabetical order)

   procedure Clear (Stack : in out Stack_Type);
   --  Empty Stack of all items.
   pragma Inline (Clear);

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

   function Peek (Stack : in Stack_Type; Index : in Positive) return Item_Type;
   --  Return the Index'th item from the top of Stack; the Item is
   --  _not_ removed. Top item has index 1.
   --
   --  Raises Constraint_Error if Index > Depth.
   pragma Inline (Peek);

   procedure Pop (Stack : in out Stack_Type);
   --  Remove top item from Stack.
   --
   --  Raises Container_Empty if Is_Empty.
   pragma Inline (Pop);

   procedure Push (Stack : in out Stack_Type; Item : in Item_Type);
   --  Add Item to the top of Stack.
   --
   --  Raises Container_Full if Is_Full.
   pragma Inline (Push);

   function Top (Stack : in Stack_Type) return Item_Type;
   --  Return the item at the top of Stack; the Item is _not_ removed.
   --  Same as Peek (Stack, 1).
   --
   --  Raises Container_Empty if Is_Empty.
   pragma Inline (Top);

private

   type Item_Array_Type is array (Positive range <>) of Item_Type;

   type Stack_Type (Size : Positive) is record

      Top       : Natural                     := 0;
      Max_Depth : Natural                     := 0;
      Items     : Item_Array_Type (1 .. Size) := (others => Null_Item);
   end record;

end SAL.Gen.Stacks.Bounded_Nonlimited;
