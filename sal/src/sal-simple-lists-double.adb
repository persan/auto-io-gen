--  Abstract :
--
--  see spec
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
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

with Ada.Unchecked_Deallocation;
package body SAL.Simple.Lists.Double is

   procedure Free_Node is new Ada.Unchecked_Deallocation (Node_Type, Node_Access_Type);

   procedure Finalize (List : in out List_Type)
   is
      Next : Node_Access_Type := List.Head;
   begin
      loop
         exit when Next = null;
         Next := List.Head.Next;
         Free_Node (List.Head);
         List.Head := Next;
      end loop;
      List.Tail := null;
   end Finalize;

   procedure Delete_Tail (List : in out List_Type)
   is
      Temp : Node_Access_Type := List.Tail;
   begin
      if Temp.Prev = null then
         --  deleting last item
         List.Head := null;
         List.Tail := null;
      else
         List.Tail := Temp.Prev;
         List.Tail.Next := null;
      end if;
      Free_Node (Temp);
   end Delete_Tail;

   procedure Insert_Tail (List : in out List_Type; Item : in Item_Type)
   is
      New_Node : constant Node_Access_Type := new Node_Type'
         (Item => Item,
          Prev => List.Tail,
          Next => null);
   begin
      if List.Tail = null then
         List.Head := New_Node;
         List.Tail := New_Node;
      else
         List.Tail.Next := New_Node;
         List.Tail := New_Node;
      end if;
   end Insert_Tail;

end SAL.Simple.Lists.Double;
