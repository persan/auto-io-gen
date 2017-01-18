--  Abstract :
--
--  A generic singly linked list, with useful operations, allowing
--  definite or indefinite, limited or non-limited item types. All
--  reasonable operations on lists are defined; see child package
--  Iterators for iterator operations.
--
--  List_Type is Controlled to recover storage when a List goes out of
--  scope. List_Type is Limited because I don't want to test a deep
--  copy.
--
--  Copyright (C) 1998, 1999, 2000 Stephen Leake.  All Rights Reserved.
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
with Ada.Unchecked_Deallocation;
with System.Storage_Pools;
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
   --  If Item_Type is indefinite, Item_Node_Type should be 'access
   --  Item_Type'. Then To_Item_Node should allocate Item and return a
   --  pointer to it, and Free_Item should be Unchecked_Deallocation.
   --  See SAL.Aux.Indefinite_Private_Items.
   --
   --  To create a list of limited objects (say of type Limited_Type),
   --  Item_Type can be a non-limited type holding the parameters
   --  needed to create an object (non-limited to allow the user to
   --  create aggregates of creation parameters), and Item_Node_Type
   --  can be access Limited_Type. Then To_Item_Node must allocate an
   --  object of type Limited_Type and initialize it using the
   --  parameters in Item_Type. See SAL.Aux.Indefinite_Limited_Items.
   --
   --  Other usages may be possible.

   Node_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   --  Root_Storage_Pool is limited, which does not allow defaults.
   --  Default for a global list type should be
   --  <some_global_access_type>'Storage_Pool. Default for a local
   --  list type, which should be reclaimed when the list type goes
   --  out of scope, is implementation defined (sigh).
package SAL.Poly.Lists.Single is
   pragma Elaborate_Body; -- System.Storage_Pools.

   ----------
   --  List operations (alphabetical)

   type List_Type is new Ada.Finalization.Limited_Controlled with private;

   function Count (List : in List_Type) return Natural;

   procedure Delete_Head (List : in out List_Type);
   --  Delete item at head of List.
   --
   --  Raises Constraint_Error if List is empty.

   overriding procedure Finalize (List : in out List_Type);
   --  Free all items in List.

   function Head (List : in List_Type) return Item_Node_Type;
   --  (Access to) item at head of list.

   procedure Insert_Head (List : in out List_Type; Item : in Item_Type);
   --  Add To_Item_Node (Item) to head of List.

   procedure Insert_Tail (List : in out List_Type; Item : in Item_Type);
   --  Add To_Item_Node (Item) to end of List.
   --
   --  Note that Delete_Tail is not implemented, since it requires
   --  traversing the list to find Prev (Tail).

   function Is_Empty (List : in List_Type) return Boolean;

   function Tail (List : in List_Type) return Item_Node_Type;
   --  (Access to) item at tail of list.

private
   type Node_Type;

   type Node_Access_Type is access Node_Type;
   for Node_Access_Type'Storage_Pool use Node_Storage_Pool;

   type Node_Type is record
      Item : Item_Node_Type;
      Next : Node_Access_Type;
   end record;

   type List_Type is new Ada.Finalization.Limited_Controlled with record
      Head  : Node_Access_Type;
      Tail  : Node_Access_Type;  -- to make it fast to append to end of list
      Count : Natural := 0;
   end record;

   ---------
   --  visible for children

   procedure Free_Node is new Ada.Unchecked_Deallocation (Node_Type, Node_Access_Type);

end SAL.Poly.Lists.Single;
