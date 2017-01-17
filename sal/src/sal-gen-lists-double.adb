--  Abstract :
--
--  see spec
--
--  Copyright (C) 1998, 2003 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen.Lists.Double is

   ---------
   --  Public operations

   procedure Copy (Source : in List_Type; Dest : in out List_Type)
   is
      Next_Source : Node_Access_Type := Source.Head;
      New_Node : Node_Access_Type;
   begin
      loop
         exit when Next_Source = null;
         New_Node := new Node_Type'
            (Item => Copy (Next_Source.Item),
             Next => null,
             Prev => Dest.Tail);
         if Dest.Tail = null then
            Dest.Head := New_Node;
            Dest.Tail := New_Node;
         else
            Dest.Tail.Next := New_Node;
            Dest.Tail := New_Node;
         end if;
         Next_Source := Next_Source.Next;
      end loop;
   end Copy;

   procedure Delete_Head (List : in out List_Type)
   is
      Temp : Node_Access_Type := List.Head;
   begin
      List.Head := Temp.Next;
      if Temp.Next /= null then
         Temp.Next.Prev := null;
      end if;
      Free_Item (Temp.Item);
      Free_Node (Temp);
   end Delete_Head;

   procedure Delete_Tail (List : in out List_Type)
   is
      Temp : Node_Access_Type := List.Tail;
   begin
      List.Tail := Temp.Prev;
      if Temp.Prev /= null then
         Temp.Prev.Next := null;
      end if;
      Free_Item (Temp.Item);
      Free_Node (Temp);
   end Delete_Tail;

   procedure Finalize (List : in out List_Type)
   is
      Next : Node_Access_Type := List.Head;
   begin
      loop
         exit when Next = null;
         Next := List.Head.Next;
         Free_Item (List.Head.Item);
         Free_Node (List.Head);
         List.Head := Next;
      end loop;
      List.Tail := null;
   end Finalize;

   function Head (List : in List_Type) return Item_Node_Type
   is begin
      return List.Head.Item;
   end Head;

   procedure Insert_Head (List : in out List_Type; Item : in Item_Type)
   is
      New_Node : constant Node_Access_Type := new Node_Type'
         (Item => To_Item_Node (Item),
          Prev => null,
          Next => List.Head);
   begin
      if List.Tail = null then
         pragma Assert (List.Head = null, "head, tail not both null");
         List.Head := New_Node;
         List.Tail := New_Node;
      else
         pragma Assert (List.Head.Prev = null, "head.prev not null");
         List.Head.Prev := New_Node;
         List.Head := New_Node;
      end if;
   end Insert_Head;

   procedure Insert_Tail (List : in out List_Type; Item : in Item_Type)
   is
      New_Node : constant Node_Access_Type := new Node_Type'
         (Item => To_Item_Node (Item),
          Prev => List.Tail,
          Next => null);
   begin
      if List.Tail = null then
         pragma Assert (List.Head = null, "head, tail not both null");
         List.Head := New_Node;
         List.Tail := New_Node;
      else
         List.Tail.Next := New_Node;
         List.Tail := New_Node;
      end if;
   end Insert_Tail;

   function Is_Empty (List : in List_Type) return Boolean
   is begin
      return List.Head = null;
   end Is_Empty;

   procedure Swap (A : in out List_Type; B : in out List_Type)
   is
      Temp : Node_Access_Type;
   begin
      Temp := A.Head;
      A.Head := B.Head;
      B.Head := Temp;

      Temp := A.Tail;
      A.Tail := B.Tail;
      B.Tail := Temp;
   end Swap;

   function Tail (List : in List_Type) return Item_Node_Type
   is begin
      return List.Tail.Item;
   end Tail;

end SAL.Gen.Lists.Double;
