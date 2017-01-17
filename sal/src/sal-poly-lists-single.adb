--  Abstract :
--
--  see spec
--
--  Copyright (C) 1998, 2003, 2005 Stephen Leake.  All Rights Reserved.
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

package body SAL.Poly.Lists.Single is

   function Count (List : in List_Type) return Natural
   is begin
      return List.Count;
   end Count;

   procedure Delete_Head (List : in out List_Type)
   is
      Temp : Node_Access_Type := List.Head;
   begin
      pragma Assert (List.Head /= null);

      if List.Tail = List.Head then
         List.Tail := null;
      end if;
      List.Head := List.Head.Next;
      Free_Item (List.Head.Item);
      Free_Node (Temp);
      List.Count := List.Count - 1;
   end Delete_Head;

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
      List.Count := 0;
   end Finalize;

   function Head (List : in List_Type) return Item_Node_Type
   is begin
      return List.Head.Item;
   end Head;

   procedure Insert_Head (List : in out List_Type; Item : in Item_Type)
   is
      New_Node : constant Node_Access_Type := new Node_Type'(To_Item_Node (Item), null);
   begin
      if List.Head = null then
         List.Head := New_Node;
         List.Tail := New_Node;
         List.Count := 1;
      else
         New_Node.Next := List.Head;
         List.Head := New_Node;
         List.Count := List.Count + 1;
      end if;
   end Insert_Head;

   procedure Insert_Tail (List : in out List_Type; Item : in Item_Type)
   is
      New_Node : constant Node_Access_Type := new Node_Type'(To_Item_Node (Item), null);
   begin
      if List.Tail = null then
         List.Head := New_Node;
         List.Tail := New_Node;
         List.Count := 1;
      else
         List.Tail.Next := New_Node;
         List.Tail := New_Node;
         List.Count := List.Count + 1;
      end if;
   end Insert_Tail;

   function Is_Empty (List : in List_Type) return Boolean
   is begin
      return List.Count = 0;
   end Is_Empty;

   function Tail (List : in List_Type) return Item_Node_Type
   is begin
      return List.Tail.Item;
   end Tail;

end SAL.Poly.Lists.Single;
