--  Abstract :
--
--  Iterators for a generic sorted binary tree.
--
--  Restrictions :
--
--  If the Tree is changed after the Iterator is Initialized, Next and
--  Find_Next may not visit all items.
--
--  Design :
--
--  Iterator_Type is Controlled to allocate/deallocate the node stack.
--
--  An iterator does not know which tree it points to. This allows
--  declaring iterators without using 'Unchecked_Access. However, it
--  must always be used with the same tree!
--
--  Copyright (C) 1997, 1998, 2000, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Finalization;
with SAL.Gen.Stacks.Bounded_Nonlimited;
with System.Storage_Pools;
generic
   Stack_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   --  Root_Storage_Pool is limited, which does not allow defaults.
   --  Default for a global list type should be
   --  <some_global_access_type>'Storage_Pool. Default for a local
   --  list type, which should be reclaimed when the list type goes
   --  out of scope, is implementation defined (sigh).
package SAL.Poly.Binary_Trees.Sorted.Iterators is
   pragma Elaborate_Body; -- System.Storage_Pools is.

   type Iterator_Type is new Ada.Finalization.Controlled with private;

   ----------
   --  Override Controlled operations

   overriding procedure Finalize (Iterator : in out Iterator_Type);
   --  Deallocate the node stack.

   overriding procedure Adjust (Iterator : in out Iterator_Type);
   --  Copy the node stack.

   -----------
   --  Initial values

   function Find (Tree : in Tree_Type'class; Key : in Key_Type) return Iterator_Type;
   --  Do binary tree search, return Iterator pointing to item with
   --  key Key. If there is no such item, Iterator points to no item.

   function First (Tree : in Tree_Type'class) return Iterator_Type;
   --  Return iterator that points to First element in Tree, or none
   --  if Tree is empty.

   function None (Tree : in Tree_Type'class) return Iterator_Type;
   --  Return iterator that points to no item.

   ----------
   --  operations

   function Current (Iterator : in Iterator_Type) return Item_Node_Type;
   --  Raise Constraint_Error if Is_Done is True.

   procedure Find_Next (Iterator : in out Iterator_Type);
   --  Set Iterator to point to next item with same key as current
   --  item. If there is no such item, Iterator points to no item, and
   --  Is_Done is True.

   function Is_Null (Iterator : in Iterator_Type) return Boolean;
   function Is_Done (Iterator : in Iterator_Type) return Boolean renames Is_Null;
   --  True if Iterator Next has moved Iterator beyond last item.

   procedure Next (Iterator : in out Iterator_Type);
   --  Advance to next item in Tree. If at last item, point to no
   --  item, mark Done True.

private
   pragma Inline (Current);
   pragma Inline (Is_Null);

   package Node_Stacks is new SAL.Gen.Stacks.Bounded_Nonlimited (Node_Access_Type, null);

   type Node_Stack_Access_Type is access Node_Stacks.Stack_Type;
   for Node_Stack_Access_Type'Storage_Pool use Stack_Storage_Pool;

   type Iterator_Type is new Ada.Finalization.Controlled with record
      Last_Down_Left : Node_Stack_Access_Type;
      Current        : Node_Access_Type       := null; -- null if iterator is done
   end record;

end SAL.Poly.Binary_Trees.Sorted.Iterators;
