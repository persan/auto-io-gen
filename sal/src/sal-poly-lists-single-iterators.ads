--  Abstract :
--
--  Iterators for singly linked lists.
--
--  Design :
--
--  We use a non-limited iterator type, to allow copying. Making it
--  limited would not prevent the user from declaring two iterator
--  objects for the same item or the same list, so a limited iterator
--  type would not be any safer.
--
--  If items are deleted from the list, iterators that point to them
--  become invalid; this is not detected.
--
--  Iterators do not know which list they point to, so operations
--  involving ranges identified by two iterators do not enforce the
--  requirement that the iterators point to the same list before
--  performing some of the operation.
--
--  An alternate design would keep a Prev component in the iterator,
--  to allow inserting and deleting items at other than the head or
--  tail. However, the Prev component becomes invalid when other
--  iterators are also used to delete or insert. Use a doubly linked
--  list if these operations are required.
--
--  Copyright (C) 1998, 1999, 2005 Stephen Leake. All Rights Reserved.
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
package SAL.Poly.Lists.Single.Iterators is
   pragma Elaborate_Body; -- System.Storage_Pools, via SAL.Poly.Lists.Single.

   type Iterator_Type is tagged private;

   -----------
   --  Initial values

   function First (List : in List_Type) return Iterator_Type;
   --  Return iterator that points to First element in List, or none
   --  if List is empty.

   function Last (List : in List_Type) return Iterator_Type;
   --  Return iterator that points to Last element in List, or none
   --  if List is empty.

   function None (List : in List_Type) return Iterator_Type;
   --  Return iterator that points to no item.

   ----------
   --  operations

   function Current (Iterator : in Iterator_Type) return Item_Node_Type;
   --  Raises Constraint_Error if Is_Null is True.

   function Is_Null (Iterator : in Iterator_Type) return Boolean;
   function Is_Done (Iterator : in Iterator_Type) return Boolean renames Is_Null;
   --  True if Iterator points to no item.

   procedure Next (Iterator : in out Iterator_Type);
   function Next (Iterator : in Iterator_Type) return Iterator_Type;
   --  Advance to next Item in list. If at last item, point to no
   --  item; then Is_Done becomes true.

private
   pragma Inline (First);
   pragma Inline (None);
   pragma Inline (Current);
   pragma Inline (Is_Null);
   pragma Inline (Next);

   type Iterator_Type is tagged record
      Current : Node_Access_Type := null; -- Iterator is Done if Current is null.
   end record;

end SAL.Poly.Lists.Single.Iterators;
