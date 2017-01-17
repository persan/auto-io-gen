--  Abstract:
--
--  Abstract stack interface.
--
--  Copyright (C)1998, 2003 Stephen Leake.  All Rights Reserved.
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
   --  items are copied to/from stack.
package SAL.Poly.Stacks is
   pragma Pure;

   type Stack_Type is abstract tagged null record;

   ------------
   --  Dispatching operations on Stack_Type (alphabetical order)

   procedure Clear (Stack : in out Stack_Type) is abstract;
   --  Empty Stack of all items.

   function Depth (Stack : in Stack_Type) return Natural is abstract;
   --  Returns current total items in the Stack

   function Is_Empty (Stack : in Stack_Type) return Boolean is abstract;
   --  Returns true iff no items are in Stack.

   function Max_Depth (Stack : in Stack_Type) return Natural is abstract;
   --  Return maximum of Depth since Stack was initialized.

   function Peek (Stack : in Stack_Type; Index : in Natural) return Item_Type is abstract;
   --  Return the Index'th item from the top of Stack; the Item is _not_ removed.
   --  Top item has index 1.
   --
   --  Raises Constraint_Error if Index > Depth.

   procedure Pop (Stack : in out Stack_Type) is abstract;
   --  Remove Item from the top of Stack
   --
   --  Raises Container_Empty if Is_Empty.

   procedure Pop (Stack : in out Stack_Type; Item : out Item_Type) is abstract;
   --  Remove Item from the top of Stack, and return it.
   --
   --  Raises Container_Empty if Is_Empty.

   procedure Push (Stack : in out Stack_Type; Item : in Item_Type) is abstract;
   --  Add Item to the top of Stack.
   --
   --  May raise Container_Full.

   function Top (Stack : in Stack_Type) return Item_Type is abstract;
   --  Return the item at the top of Stack; the Item is _not_ removed.
   --  Same as Peek (Stack, 1).
   --
   --  Raises Container_Empty if Is_Empty.

end SAL.Poly.Stacks;
