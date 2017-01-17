--  Abstract :
--
--  See spec.
--
--  Design Notes:
--
--  Traversal algorithm based on Knuth Volume 1 section 2.3.1 algorithm T.
--
--  Copyright (C) 1998 - 2001 Stephen Leake.  All Rights Reserved.
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
package body SAL.Poly.Binary_Trees.Sorted.Iterators is

   procedure Free is new Ada.Unchecked_Deallocation (Node_Stacks.Stack_Type, Node_Stack_Access_Type);

   procedure Locate_Node
      (Tree           : in     Node_Access_Type;
       Key            : in     Key_Type;
       Last_Down_Left : in out Node_Stacks.Stack_Type;
       Node           :    out Node_Access_Type;
       Result         :    out Locate_Result_Type)
      --  Set Found to node where Key is, or should be inserted.
      --  Update Last_Down_Left appropriately for an iterator. Assumes
      --  Tree non-null.
   is begin
      if Is_Equal (Key, To_Key (Tree.Item)) then
         Node := Tree;
         Result := Found;
      elsif Is_Greater (Key, To_Key (Tree.Item)) then
         if Tree.Right = null then
            Node := Tree;
            Result := Insert_Right;
         else
            Locate_Node (Tree.Right, Key, Last_Down_Left, Node, Result);
         end if;
      else
         if Tree.Left = null then
            Node := Tree;
            Result := Insert_Left;
         else
            Node_Stacks.Push (Last_Down_Left, Tree);
            Locate_Node (Tree.Left, Key, Last_Down_Left, Node, Result);
         end if;
      end if;
   end Locate_Node;

   -----------
   --  Public operations

   ----------
   --  Override Controlled operations

   overriding procedure Finalize (Iterator : in out Iterator_Type)
   is begin
      Free (Iterator.Last_Down_Left);
   end Finalize;

   overriding procedure Adjust (Iterator : in out Iterator_Type)
   is begin
      if Iterator.Last_Down_Left = null then
         --  Empty tree, no stack
         null;
      else
         Iterator.Last_Down_Left := new Node_Stacks.Stack_Type'(Iterator.Last_Down_Left.all);
      end if;
   end Adjust;

   -----------
   --  Initial values

   function Find (Tree : in Tree_Type'class; Key : in Key_Type) return Iterator_Type
   is
      Node : Node_Access_Type;
      Result : Locate_Result_Type;
      Iterator : Iterator_Type;
   begin
      if Tree.Root = null then
         Iterator.Current := null;
      else
         Iterator.Last_Down_Left := new Node_Stacks.Stack_Type (Tree.Height);
         Locate_Node (Tree.Root, Key, Iterator.Last_Down_Left.all, Node, Result);
         case Result is
         when Found =>
            Iterator.Current := Node;
         when others =>
            Iterator.Current := null;
         end case;
      end if;
      return Iterator;
   end Find;

   function First (Tree : in Tree_Type'class) return Iterator_Type
   is
      Iterator : Iterator_Type;
   begin
      if Tree.Height = 0 then
         --  Don't need a stack if the tree is empty.
         return Iterator;
      end if;

      Iterator.Last_Down_Left := new Node_Stacks.Stack_Type (Tree.Height);

      if Tree.Root = null then
         Iterator.Current := null;
      else
         Iterator.Current := Tree.Root;
         loop
            exit when Iterator.Current.Left = null;
            Node_Stacks.Push (Iterator.Last_Down_Left.all, Iterator.Current);
            Iterator.Current := Iterator.Current.Left;
         end loop;
      end if;
      return Iterator;
   end First;

   function None (Tree : in Tree_Type'class) return Iterator_Type
   is begin
      return (Ada.Finalization.Controlled with
              Last_Down_Left => new Node_Stacks.Stack_Type (Tree.Height),
              Current        => null);
   end None;

   ----------
   --  operations

   function Current (Iterator : in Iterator_Type) return Item_Node_Type
   is begin
      return Iterator.Current.Item;
   end Current;

   procedure Find_Next (Iterator : in out Iterator_Type)
   is begin
      --  Add stores duplicate keys in Right child.
      if Duplicate_Key_Action = Allow and
         (Iterator.Current /= null and then
          Iterator.Current.Right /= null)
      then
         if Is_Equal (To_Key (Iterator.Current.Right.Item), To_Key (Iterator.Current.Item)) then
            Iterator.Current := Iterator.Current.Right;
         else
            Iterator.Current := null;
         end if;
      else
         Iterator.Current := null;
      end if;
   end Find_Next;

   function Is_Null (Iterator : in Iterator_Type) return Boolean
   is begin
      return Iterator.Current = null;
   end Is_Null;

   procedure Next (Iterator : in out Iterator_Type)
   is
      use Node_Stacks;
   begin
      if Iterator.Current = null then
         return;
      end if;

      if Iterator.Current.Right /= null then
         --  find first node in Right subtree
         Iterator.Current := Iterator.Current.Right;
         loop
            exit when Iterator.Current.Left = null;
            Push (Iterator.Last_Down_Left.all, Iterator.Current);
            Iterator.Current := Iterator.Current.Left;
         end loop;
      else
         if Is_Empty (Iterator.Last_Down_Left.all) then
            Iterator.Current := null;
         else
            Iterator.Current := Top (Iterator.Last_Down_Left.all);
            Pop (Iterator.Last_Down_Left.all);
         end if;
      end if;
   end Next;

end SAL.Poly.Binary_Trees.Sorted.Iterators;
