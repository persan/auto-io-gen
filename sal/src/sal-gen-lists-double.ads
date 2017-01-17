--  Abstract :
--
--  A generic doubly linked list, with useful operations, allowing
--  definite or indefinite, limited or non-limited item types.
--
--  See SAL.Gen.Lists.Double.Iterators for iterators.
--
--  Since List_Type is not Controlled, the user must recover list
--  storage when a list goes out of scope.
--
--  See SAL.Poly.Lists.Double for a Controlled list.
--
--  Item access functions return Item_Node_Type, which may be
--  Item_Type or an access type, depending on the instantiation
--  choices. We don't return Item_Access_Type, because it is
--  impossible for the user to provide 'function To_Access (Item : in
--  out Item_Node_Type) return Item_Access_Type', since it might use
--  copy-in/copy-out parameter passing, which would then return the
--  address of Item on the stack, which is destroyed on return. Thus
--  if the user needs to modify elements in place, they must choose
--  Item_Node_Type to be an access type.
--
--  Because the child package Iterators supports Splice, we don't
--  store a count in list; that would force Splice to count the items
--  in the splice, which would make it not constant time.
--
--  Copyright (C) 1998, 2000 Stephen Leake.  All Rights Reserved.
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
   --  can be 'access Limited_Type'. Then To_Item_Node must allocate
   --  an object of type Limited_Type and initialize it using the
   --  parameters in Item_Type. See SAL.Aux.Indefinite_Limited_Items.
   --
   --  Other usages may be possible.

   with function Copy (Source : in Item_Node_Type) return Item_Node_Type is <>;
   --  Deep copy of Source, for use in allocators in Copy (List).

   Node_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   --  Root_Storage_Pool is limited, which does not allow defaults.
   --  Default for a global list type should be
   --  <some_global_access_type>'Storage_Pool. Default for a local
   --  list type, which should be reclaimed when the list type goes
   --  out of scope, is implementation defined (sigh).
package SAL.Gen.Lists.Double is
   pragma Preelaborate; -- System.Storage_Pools, Node_Access_Type.

   ----------
   --  List operations (alphabetical by name)

   type List_Type is limited private;

   procedure Copy (Source : in List_Type; Dest : in out List_Type);
   --  Deep copy of Source to end of Dest.

   procedure Delete_Head (List : in out List_Type);
   --  Raises Constraint_Error if List is empty.

   procedure Delete_Tail (List : in out List_Type);
   --  Raises Constraint_Error if List is empty.

   procedure Finalize (List : in out List_Type);
   --  Free all items in List.

   function Head (List : in List_Type) return Item_Node_Type;
   --  (Access to) first item.
   --
   --  Raises Constraint_Error if List is empty.

   procedure Insert_Head (List : in out List_Type; Item : in Item_Type);
   --  Add To_Item_Node (Item) to head of List.

   procedure Insert_Tail (List : in out List_Type; Item : in Item_Type);
   --  Add To_Item_Node (Item) to tail of List.

   function Is_Empty (List : in List_Type) return Boolean;

   procedure Swap (A : in out List_Type; B : in out List_Type);
   --  Swap contents of lists, by swapping head and tail pointers.

   function Tail (List : in List_Type) return Item_Node_Type;
   --  (Access to) last item.

private
   type Node_Type;

   type Node_Access_Type is access Node_Type;
   for Node_Access_Type'Storage_Pool use Node_Storage_Pool;

   type Node_Type is record
      Item : Item_Node_Type;
      Prev : Node_Access_Type;
      Next : Node_Access_Type;
   end record;

   type List_Type is limited record
      Head  : Node_Access_Type := null;
      Tail  : Node_Access_Type := null;
   end record;

   ------------
   --  The following are needed by Iterators child package.

   procedure Free_Node is new Ada.Unchecked_Deallocation (Node_Type, Node_Access_Type);

end SAL.Gen.Lists.Double;
