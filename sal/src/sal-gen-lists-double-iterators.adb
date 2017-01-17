--  Abstract :
--
--  see spec
--
--  Copyright (C) 1998 - 2000, 2002, 2005 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen.Lists.Double.Iterators is

   ----------
   --  Private operations

   procedure Delete
      (List : in out List_Type;
       Node : in out Node_Access_Type)
   is begin
      if Node.Next = null then
         List.Tail := Node.Prev;
      else
         Node.Next.Prev := Node.Prev;
      end if;
      if Node.Prev = null then
         List.Head := Node.Next;
      else
         Node.Prev.Next := Node.Next;
      end if;
      Free_Item (Node.Item);
      Free_Node (Node);
   end Delete;

   procedure Insert
      (List     : in out List_Type;
       Prev     : in     Node_Access_Type;
       Item     : in     Item_Node_Type;
       Next     : in     Node_Access_Type;
       Inserted :    out Node_Access_Type)
   is  begin
      Inserted := new Node_Type'(Item, Prev, Next);
      if Prev = null then
         List.Head := Inserted;
      else
         Prev.Next := Inserted;
      end if;
      if Next = null then
         List.Tail := Inserted;
      else
         Next.Prev := Inserted;
      end if;
   end Insert;

   ----------
   --  Public operations

   function None (List : in List_Type) return Iterator_Type
   is
      pragma Unreferenced (List);
   begin
      return null;
   end None;

   function First (List : in List_Type) return Iterator_Type
   is begin
      return Iterator_Type (List.Head);
   end First;

   function Last (List : in List_Type) return Iterator_Type
   is begin
      return Iterator_Type (List.Tail);
   end Last;

   ----------
   --  Iterator operations

   function Current (Iterator : in Iterator_Type) return Item_Node_Type
   is begin
      return Iterator.Item;
   end Current;

   function Is_Null (Iterator : in Iterator_Type) return Boolean
   is begin
      return Iterator = null;
   end Is_Null;

   function Next (Iterator : in Iterator_Type) return Iterator_Type
   is begin
      return Iterator_Type (Iterator.Next);
   end Next;

   procedure Next (Iterator : in out Iterator_Type)
   is begin
      Iterator := Iterator_Type (Iterator.Next);
   end Next;

   function Prev (Iterator : in Iterator_Type) return Iterator_Type
   is begin
      return Iterator_Type (Iterator.Prev);
   end Prev;

   procedure Prev (Iterator : in out Iterator_Type)
   is begin
      Iterator := Iterator_Type (Iterator.Prev);
   end Prev;

   ----------
   --  Operations on Lists

   procedure Copy_Before
      (Source : in     List_Type;
       First  : in     Iterator_Type;
       Last   : in     Iterator_Type;
       Dest   : in out List_Type;
       Before : in     Iterator_Type)
   is
      --  Copy from Last first, so we can catch Before between First
      --  and Last

      Source_Current : Node_Access_Type := Node_Access_Type (Last);
      Last_Inserted  : Node_Access_Type := Node_Access_Type (Before);
      Insert_After   : Node_Access_Type;
      Source_First   : Node_Access_Type := Node_Access_Type (First);
   begin
      if Source.Tail = null then
         --  empty source
         pragma Assert (First = null and Last = null);
         return;
      end if;

      pragma Assert (Source.Head /= null and Source.Tail /= null);

      if Source_Current = null then
         Source_Current := Source.Tail;
      end if;

      if Source_First = null then
         Source_First := Source.Head;
      end if;

      if Before = null then
         Insert_After := null;
      else
         Insert_After := Before.Prev;
      end if;

      loop
         if Source_Current = Node_Access_Type (Before) then
            --  true if Before between First and Last; avoid infinite loop.
            raise Iterator_Error;
         elsif Source_Current = null then
            --  true if First after Last
            raise Iterator_Error;
         end if;

         Insert
            (List     => Dest,
             Prev     => Insert_After,
             Item     => Copy (Source_Current.Item),
             Next     => Last_Inserted,
             Inserted => Last_Inserted);
         exit when Source_Current = Source_First;
         Source_Current := Source_Current.Prev;
      end loop;
   end Copy_Before;

   procedure Delete
      (List     : in out List_Type;
       Iterator : in out Iterator_Type)
   is
      Temp : constant Iterator_Type := Iterator_Type (Iterator.Next);
   begin
      Delete (List, Node_Access_Type (Iterator));
      Iterator := Temp;
   end Delete;

   procedure Delete
      (List  : in out List_Type;
       First : in out Iterator_Type;
       Last  : in out Iterator_Type)
   is
      Next         : Iterator_Type;
      Deleted_Node : Iterator_Type;
   begin
      if First = null then
         First := Iterator_Type (List.Head);
      end if;

      if Last = null then
         Last := Iterator_Type (List.Tail);
      end if;

      if Last = null then
         --  empty range, empty list
         pragma Assert (First = null);
         return;
      else
         loop
            if First = null then
               --  First not before last
               raise Iterator_Error;
            else
               Next := Iterator_Type (First.Next);
            end if;
            Deleted_Node := First;
            Delete (List, First);
            First := Next;
            exit when Deleted_Node = Last;
         end loop;
         Last  := Next;
         First := Next;
      end if;
   end Delete;

   procedure Insert_After
      (List   : in out List_Type;
       After  : in     Iterator_Type;
       Item   : in     Item_Type;
       Copies : in     Natural         := 1)
   is
      Last_Inserted : Node_Access_Type := Node_Access_Type (After);
      Insert_Before : Node_Access_Type;
   begin
      if After = null then
         Insert_Before := null;
         Last_Inserted := List.Tail;
      else
         Insert_Before := After.Next;
      end if;

      for I in 1 .. Copies loop
         Insert
            (List     => List,
             Prev     => Last_Inserted,
             Item     => To_Item_Node (Item),
             Next     => Insert_Before,
             Inserted => Last_Inserted);
      end loop;
   end Insert_After;

   procedure Insert_Before
      (List   : in out List_Type;
       Before : in     Iterator_Type;
       Item   : in     Item_Type;
       Copies : in     Natural         := 1)
   is
      Insert_After  : Node_Access_Type;
      Last_Inserted : Node_Access_Type := Node_Access_Type (Before);
   begin
      if Before = null then
         Insert_After := null;
         Last_Inserted := List.Head;
      else
         Insert_After := Before.Prev;
      end if;

      for I in 1 .. Copies loop
         Insert
            (List     => List,
             Prev     => Insert_After,
             Item     => To_Item_Node (Item),
             Next     => Last_Inserted,
             Inserted => Last_Inserted);
      end loop;
   end Insert_Before;

   procedure Splice_After
      (Source : in out List_Type;
       First  : in     Iterator_Type;
       Last   : in     Iterator_Type;
       Dest   : in out List_Type;
       After  : in     Iterator_Type)
   is
      Source_First : Node_Access_Type := Node_Access_Type (First);
      Source_Last  : Node_Access_Type := Node_Access_Type (Last);
      Dest_After   : constant Node_Access_Type := Node_Access_Type (After);
      Dest_Before  : Node_Access_Type;
   begin
      if Source_First = null then
         if Source.Head = null then
            pragma Assert (Source.Tail = null);
            --  source is empty
            return;
         else
            Source_First := Source.Head;
         end if;
      end if;

      if Source_Last = null then
         Source_Last := Source.Tail;
         pragma Assert (Source_Last /= null); -- only true if Tail = null with Head /= null.
      end if;

      if Source.Head = Source_First then
         Source.Head := Source_Last.Next;
      else
         Source_First.Prev.Next := Source_Last.Next;
      end if;

      if Source.Tail = Source_Last then
         Source.Tail := Source_First.Prev;
      else
         Source_Last.Next.Prev := Source_First.Prev;
      end if;

      if Dest_After = null or Dest_After = Dest.Tail then
         if Dest.Tail = null then
            --  Dest is empty
            Dest.Head := Source_First;
            Source_First.Prev := null;
            Dest.Tail := Source_Last;
            Source_Last.Next := null;
         else
            --  insert at tail
            Dest.Tail.Next := Source_First;
            Source_First.Prev := Dest.Tail;
            Dest.Tail := Source_Last;
            Source_Last.Next := null;
         end if;
      else
         Dest_Before := Dest_After.Next;

         Dest_After.Next := Source_First;
         Source_First.Prev := Dest_After;
         Source_Last.Next := Dest_Before;
         Dest_Before.Prev := Source_Last;
      end if;

   end Splice_After;

   procedure Splice_Before
      (Source : in out List_Type;
       First  : in     Iterator_Type;
       Last   : in     Iterator_Type;
       Dest   : in out List_Type;
       Before : in     Iterator_Type)
   is
      Source_First : Node_Access_Type := Node_Access_Type (First);
      Source_Last  : Node_Access_Type := Node_Access_Type (Last);
      Dest_After   : Node_Access_Type;
      Dest_Before  : constant Node_Access_Type := Node_Access_Type (Before);
   begin
      if Source_First = null then
         if Source.Head = null then
            pragma Assert (Source.Tail = null);
            --  source is empty
            return;
         else
            Source_First := Source.Head;
         end if;
      end if;

      if Source_Last = null then
         Source_Last := Source.Tail;
         pragma Assert (Source_Last /= null); -- only true if Tail = null with Head /= null.
      end if;

      if Source.Head = Source_First then
         Source.Head := Source_Last.Next;
      else
         Source_First.Prev.Next := Source_Last.Next;
      end if;

      if Source.Tail = Source_Last then
         Source.Tail := Source_First.Prev;
      else
         Source_Last.Next.Prev := Source_First.Prev;
      end if;

      if Dest_Before = null or Dest_Before = Dest.Head then
         if Dest.Head = null then
            --  Dest is empty
            Dest.Head := Source_First;
            Source_First.Prev := null;
            Dest.Tail := Source_Last;
            Source_Last.Next := null;
         else
            --  insert at Head
            Dest.Head.Prev := Source_Last;
            Source_Last.Next := Dest.Head;
            Dest.Head := Source_First;
            Source_First.Prev := null;
         end if;
      else
         Dest_After := Dest_Before.Prev;

         Dest_After.Next := Source_First;
         Source_First.Prev := Dest_After;
         Source_Last.Next := Dest_Before;
         Dest_Before.Prev := Source_Last;
      end if;

   end Splice_Before;

end SAL.Gen.Lists.Double.Iterators;
