--  Abstract :
--
--  A generic doubly linked list, with useful operations, allowing
--  definite or indefinite, limited or non-limited item types. All
--  reasonable operations on lists are defined.
--
--  List_Type is Controlled to recover storage when a List goes out of
--  scope.
--
--  See SAL.Gen.Lists.Double for a non-tagged list.
--
--  Because the Iterators support Splice, we don't store a count in
--  List_Type; that would force Splice to count the items in the
--  splice, which would make it not constant time.
--
--  Copyright (C) 2000 - 2004, 2007 Stephen Leake. All Rights
--  Reserved.
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
   --  Deep copy of Source, for use in Adjust (List).
   --
   --  May raise SAL.Invalid_Operation for items that should not be
   --  copied.

   Node_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   --  Root_Storage_Pool is limited, which does not allow defaults.
   --  Default for a global list type should be
   --  System.Storage_Pools.Root_Storage_Pool'Class
   --  (<some_global_access_type>'Storage_Pool). Default for a local
   --  list type, which should be reclaimed when the list type goes
   --  out of scope, is implementation defined (sigh).
package SAL.Poly.Lists.Double is
   pragma Preelaborate; --  System.Storage_Pools, Node_Access_Type.

   type List_Type is new Ada.Finalization.Controlled with private;

   Null_List : constant List_Type;

   --------------
   --  Override List_Type operations

   overriding procedure Adjust (List : in out List_Type);
   --  Deep copy.

   overriding procedure Finalize (List : in out List_Type);
   --  Free all items in List.

   -------------
   --  New List_Type operations

   procedure Delete_Head (List : in out List_Type);
   --  Delete item at head of List.
   --
   --  Raises Constraint_Error if List is empty.

   procedure Delete_Tail (List : in out List_Type);
   --  Delete item at Tail of List.
   --
   --  Raises Constraint_Error if List is empty.

   function Head (List : in List_Type) return Item_Node_Type;
   --  (Access to) first item.
   --
   --  Raises Constraint_Error if List is empty.

   procedure Insert_Head (List : in out List_Type; Item : in Item_Type);
   --  Add To_Item_Node (Item) to head of List.

   procedure Insert_Tail (List : in out List_Type; Item : in Item_Type);
   procedure Add (List : in out List_Type; Item : in Item_Type) renames Insert_Tail;
   --  Add To_Item_Node (Item) to tail of List.

   function Is_Empty (List : in List_Type) return Boolean;

   function Tail (List : in List_Type) return Item_Node_Type;
   --  (Access to) last item.

   ----------
   --  Iterators
   --
   --  Note that you can easily define more than one iterator for a
   --  list; deleting items with one iterator may invalidate the
   --  other.

   type Iterator_Type is private;

   Null_Iterator : constant Iterator_Type;

   function None return Iterator_Type;
   function None (List : in List_Type) return Iterator_Type;
   --  Return Iterator that points to no item. List is not used; it
   --  serves to make this a primitive operation.

   function First (List : in List_Type) return Iterator_Type;
   --  Return Iterator that points to first item, or none if list is
   --  empty.

   function Last (List : in List_Type) return Iterator_Type;
   --  Return Iterator that points to last item, or none if list is
   --  empty.

   ----------
   --  Iterator operations

   function Current (Iterator : in Iterator_Type) return Item_Node_Type;
   --  Raises Constraint_Error if Is_Null is True.

   function Is_Null (Iterator : in Iterator_Type) return Boolean;
   function Is_Done (Iterator : in Iterator_Type) return Boolean renames Is_Null;
   --  True if Iterator points to no item.

   function Next (Iterator : in Iterator_Type) return Iterator_Type;
   procedure Next (Iterator : in out Iterator_Type);
   function Prev (Iterator : in Iterator_Type) return Iterator_Type;
   procedure Prev (Iterator : in out Iterator_Type);
   --  Advance to next/previous Item in list. If at last/first item,
   --  point to no item.
   --
   --  Raises Constraint_Error if Is_Null is True.

   ----------
   --  Iterator operations on Lists

   procedure Copy_Before
     (Source : in     List_Type;
      First  : in     Iterator_Type;
      Last   : in     Iterator_Type;
      Dest   : in out List_Type;
      Before : in     Iterator_Type);
   --  Insert a copy of all items in range [First .. Last] in Source
   --  before Before in Dest.
   --
   --  If First is null, copy from head of Source.
   --
   --  If Last is null, copy thru tail of Source.
   --
   --  If Before is null, insert after tail of Dest.
   --
   --  Raises Iterator_Error if Before between First and Last. In this
   --  case, items from Before thru Last on Last's list have been
   --  copied.
   --
   --  Raises Iterator_Error if First not before Last on same list. In
   --  this case, items from Last thru Head on Last's list have been
   --  copied.
   --
   --  Cannot detect if First, Last on same list that is not Source.

   procedure Delete
     (List     : in out List_Type;
      Iterator : in out Iterator_Type);
   --  Delete Iterator's current item from List. Iterator then points
   --  to the next item, or none.
   --
   --  Raises Constraint_Error if Is_Null (Iterator) is True.

   procedure Delete
     (List  : in out List_Type;
      First : in out Iterator_Type;
      Last  : in out Iterator_Type);
   --  Delete items in range [First, Last] from List. First and Last
   --  then point to the next item after Last (or none).
   --
   --  If First is null, delete from the beginning of the list.
   --
   --  If Last is null, delete thru the end of the list.
   --
   --  If First, Last both null, delete entire list. If list is empty,
   --  no effect.
   --
   --  Raises Constraint_Error if First not before Last on same list.
   --  In this case, items from First thru Tail on First's list have
   --  been deleted.
   --
   --  Cannot detect if First, Last on same list that is not List.

   procedure Insert_After
     (List   : in out List_Type;
      After  : in     Iterator_Type;
      Item   : in     Item_Type;
      Copies : in     Natural       := 1);
   --  Insert To_Item_Node (Item) after After's current position in
   --  List, Copies times. Later copies are inserted after earlier
   --  copies.
   --
   --  If After is null, insert at head of List.
   --
   --  Cannot detect if After is not on List.

   procedure Insert_Before
     (List   : in out List_Type;
      Before : in     Iterator_Type;
      Item   : in     Item_Type;
      Copies : in     Natural       := 1);
   --  Insert To_Item_Node (Item) before Before's current position in
   --  List, Copies times. Later copies are inserted before earlier
   --  copies.
   --
   --  If Before is Null, insert at tail of List.
   --
   --  Cannot detect if Before is not on List.

   procedure Replace
     (List     : in out List_Type;
      Iterator : in     Iterator_Type;
      Item     : in     Item_Type);
   --  Replace Current (Iterator) with Item.
   --
   --  Cannot detect if Before is not on List.
   --
   --  Raise Constraint_Error if Iterator is Null.

   procedure Splice_After
     (Source : in out List_Type;
      First  : in     Iterator_Type;
      Last   : in     Iterator_Type;
      Dest   : in out List_Type;
      After  : in     Iterator_Type);
   --  Insert all Items in range [First, Last] after After's current
   --  position in Dest, removing from Source. After may be from the
   --  same list as First and Last, as long as it is not between them.
   --
   --  If After is null, items are inserted before head of Dest.
   --
   --  If First is null, insert from head of Source.
   --
   --  If Last is null, insert thru tail of Source.
   --
   --  Does not detect if [First, Last] contains Before, or if First
   --  not before Last on same list.

   procedure Splice_Before
     (Source : in out List_Type;
      First  : in     Iterator_Type;
      Last   : in     Iterator_Type;
      Dest   : in out List_Type;
      Before : in     Iterator_Type);
   --  Insert all Items in range [First, Last] before Before's current
   --  position in Dest, removing from Source.
   --
   --  Before may be from the same list as First and Last, as long as
   --  it is not between them.
   --
   --  If Before is null, items are inserted at beginning of Dest.
   --
   --  If First is null, splice from head of Source.
   --
   --  If Last is null, splice thru tail of Source.
   --
   --  Does not detect if [First, Last] contains Before, or if First
   --  not before Last on same list.

private
   type Node_Type;

   type Node_Access_Type is access Node_Type;
   for Node_Access_Type'Storage_Pool use Node_Storage_Pool;

   type Node_Type is record
      Item : Item_Node_Type;
      Prev : Node_Access_Type;
      Next : Node_Access_Type;
   end record;

   type List_Type is new Ada.Finalization.Controlled with record
      Head : Node_Access_Type := null;
      Tail : Node_Access_Type := null;
   end record;

   type Iterator_Type is new Node_Access_Type;

   Null_Iterator : constant Iterator_Type := null;

   Null_List : constant List_Type := (Ada.Finalization.Controlled with null, null);

end SAL.Poly.Lists.Double;
