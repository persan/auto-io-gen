--  Abstract:
--
--  A generic queue, allowing definite non-limited item types.
--
--  Copyright (C) 2004, 2008 Stephen Leake.  All Rights Reserved.
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
   type Item_Type is private;
package SAL.Gen.Queues.Gen_Bounded_Nonlimited is
   pragma Pure;

   type Queue_Type (Size : Positive) is private;
   --  Size is maximum number of items in the queue.
   --  See Add for meaning of Overflow_Handling.

   ------------
   --  Operations on Queue_Type (alphabetical order)

   procedure Clear (Queue : in out Queue_Type);
   --  Empty Queue of all items.

   function Count (Queue : in Queue_Type) return Natural;
   --  Returns current total items in the Queue

   function Is_Empty (Queue : in Queue_Type) return Boolean;
   --  Returns true if no items are in Queue.

   function Is_Full (Queue : in Queue_Type) return Boolean;
   --  Returns true if Queue is full.

   procedure Remove (Queue : in out Queue_Type; Item : out Item_Type);
   --  Remove head item from Queue, return it in Item.
   --
   --  Raises Container_Empty if Is_Empty.

   procedure Add (Queue : in out Queue_Type; Item : in Item_Type);
   --  Add Item to the tail of Queue.
   --
   --  If Queue is full, result depends on Queue.Overflow_Handling:
   --
   --  when Overwrite, an implicit Remove is done (and the data
   --  discarded), then Add is done.
   --
   --  when Error, raises Container_Full.

   function Get_Overflow_Handling (Queue : in Queue_Type) return Overflow_Action_Type;
   procedure Set_Overflow_Handling (Queue : in out Queue_Type; Handling : in Overflow_Action_Type);

private

   type Item_Array_Type is array (Positive range <>) of Item_Type;

   type Queue_Type (Size : Positive) is record
      Overflow_Handling : Overflow_Action_Type := Error;

      Head  : Natural := 0;
      Tail  : Natural := 0;
      Count : Natural := 0;
      Data  : Item_Array_Type (1 .. Size);
      --  Add at Tail + 1, remove at Head. Count is current count;
      --  easier to keep track of that than to compute Is_Empty for
      --  each Add and Remove.
   end record;

end SAL.Gen.Queues.Gen_Bounded_Nonlimited;
