--  Abstract :
--
--  Iterators for SAL.Gen.Lists.Double.
--
--  Note that you can easily define more than one iterator for a list;
--  deleting items with one iterator may invalidate the other.
--
--  Copyright (C) 1998 - 2000 Stephen Leake.  All Rights Reserved.
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
package SAL.Gen.Lists.Double.Iterators is
   pragma Preelaborate; -- SAL.Gen.Lists.Double

   type Iterator_Type is private;

   ----------
   --  Initial values

   function None (List : in List_Type) return Iterator_Type;
   --  Return Iterator that points to no item.

   function First (List : in List_Type) return Iterator_Type;
   --  Return Iterator that points to first item, or none if list is empty.

   function Last (List : in List_Type) return Iterator_Type;
   --  Return Iterator that points to last item, or none if list is empty.

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
   --  Raises Constraint_Error if Iterator already Done.

   ----------
   --  Operations on Lists

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
   --  If Before is null, insert at head of Dest.
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
       Copies : in     Natural         := 1);
   --  Insert To_Item_Node (Item) after After's current position in
   --  List, Copies times. Later copies are inserted after earlier
   --  copies.
   --
   --  If Iterator is null, insert at tail of List.
   --
   --  Cannot detect if First is not on List.

   procedure Insert_Before
      (List   : in out List_Type;
       Before : in     Iterator_Type;
       Item   : in     Item_Type;
       Copies : in     Natural         := 1);
   --  Insert To_Item_Node (Item) before Iterator's current position
   --  in List, Copies times. Later copies are inserted before earlier
   --  copies.
   --
   --  If Iterator is Done, insert at head of List.
   --
   --  Cannot detect if First is not on List.

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
   --  If After is null, items are inserted after tail of Dest.
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
   --  position in Dest, removing from their current list. Before may
   --  be from the same list as First and Last, as long as it is not
   --  between them.
   --
   --  If Before is null, items are inserted at beginning of Dest.
   --
   --  If First is null, insert from head of Source.
   --
   --  If Last is null, insert thru tail of Source.
   --
   --  Does not detect if [First, Last] contains Before, or if First
   --  not before Last on same list.


private
   type Iterator_Type is new Node_Access_Type;

end SAL.Gen.Lists.Double.Iterators;
