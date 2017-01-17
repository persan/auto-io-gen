--  Abstract :
--
--  See spec.
--
--  References :
--
--  [1] Practical Algorithms for Programmers, Andrew Binstock, John Rex, Addison-Wesley 1995
--
--  Copyright (C) 1998 - 2003, 2005 - 2007 Stephen Leake.  All Rights Reserved.
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
package body SAL.Poly.Binary_Trees.Sorted is

   --  Local subprogram declarations (alphabetical order)

   procedure Finalize (Tree : in out Node_Access_Type);

   function Find_Height (Tree : in Node_Access_Type) return Integer;

   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access_Type);

   procedure Locate_Node
      (Tree   : in     Node_Access_Type;
       Key    : in     Key_Type;
       Node   :    out Node_Access_Type;
       Result :    out Locate_Result_Type;
       Depth  : in out Integer);
   --  Set Node to node where Key is, or should be inserted.
   --  Assumes Tree non-null.
   --  Depth should be depth of Tree; updated to depth of Node.

   procedure Move
      (To   : in out Tree_Type'class;
       From : in     Node_Access_Type);
   --  Move From into To, adjusting To.Height and To.Count as necessary.
   --
   --  Raises Duplicate_Key as appropriate.

   ----------
   --  Local subprogram bodies

   procedure Add
     (Tree     : in     Node_Access_Type;
      New_Node : in     Node_Access_Type;
      Depth    : in out Integer)
   is
      Located_Node : Node_Access_Type;
      Result : Locate_Result_Type;
   begin
      Locate_Node (Tree, To_Key (New_Node.Item), Located_Node, Result, Depth);
      case Result is
      when Found =>
         case Duplicate_Key_Action is
         when Error =>
            raise Duplicate_Key;

         when Ignore =>
            null;

         when Allow =>
            --  Add to Right, so Iterator finds duplicate keys in insertion order
            if Located_Node.Right = null then
               Located_Node.Right := New_Node;
            else
               Add (Located_Node.Right, New_Node, Depth);
            end if;
         end case;

      when Insert_Right =>
         Depth := Depth + 1;
         Located_Node.Right := New_Node;

      when Insert_Left =>
         Depth := Depth + 1;
         Located_Node.Left := New_Node;

      end case;
   end Add;

   procedure Add
     (Tree  : in     Node_Access_Type;
      Item  : in     Item_Type;
      Depth : in out Integer)
   is
      New_Node : Node_Access_Type := new Node_Type'(To_Item_Node (Item), Left => null, Right => null);
   begin
      Add (Tree, New_Node, Depth);
   exception
   when Duplicate_Key =>
      Finalize (New_Node);
      raise;
   end Add;

   procedure Add_Height_Count
      (Tree   : in     Node_Access_Type;
       Height : in out Integer;
       Count  : in out Integer)
      --  Set Height and Count for Tree.
   is
      Left_Height  : Integer := 0;
      Right_Height : Integer := 0;
   begin
      if Tree = null then
         Height := 0;
      else
         Count := Count + 1;
         Add_Height_Count (Tree.Left, Left_Height, Count);
         Add_Height_Count (Tree.Right, Right_Height, Count);
         Height := 1 + Integer'Max (Left_Height, Right_Height);
      end if;
   end Add_Height_Count;

   procedure Delete
      (Tree     : in out Tree_Type;
       Sub_Tree : in out Node_Access_Type;
       Key      : in     Key_Type)
      --  Find and delete node containing Key from Sub_Tree.
   is begin
      --  See [1] figure 6-5.
      if Sub_Tree = null then
         raise Not_Found;
      elsif Is_Equal (Key, To_Key (Sub_Tree.Item)) then
         declare
            Left  : constant Node_Access_Type := Sub_Tree.Left;
            Right : constant Node_Access_Type := Sub_Tree.Right;
         begin
            Free_Item (Sub_Tree.Item);
            Free (Sub_Tree);
            if Right = null then
               Sub_Tree := Left;
            elsif Right.Left = null then
               Sub_Tree := Right;
               Sub_Tree.Left := Left;
            else
               declare
                  Leftmost_Parent : Node_Access_Type := Right;
                  Leftmost        : Node_Access_Type := Right.Left;
               begin
                  loop
                     exit when Leftmost.Left = null;
                     Leftmost_Parent := Leftmost;
                     Leftmost        := Leftmost.Left;
                  end loop;
                  Sub_Tree             := Leftmost;
                  Leftmost_Parent.Left := Leftmost.Right;
                  Sub_Tree.Right       := Right;
                  Sub_Tree.Left        := Left;
               end;
            end if;
         end;
      elsif Is_Greater (Key, To_Key (Sub_Tree.Item)) then
         Delete (Tree, Sub_Tree.Right, Key);
      else
         Delete (Tree, Sub_Tree.Left, Key);
      end if;
   end Delete;

   procedure Finalize (Tree : in out Node_Access_Type)
   is begin
      if Tree /= null then
         Finalize (Tree.Left);
         Finalize (Tree.Right);
         Free_Item (Tree.Item);
         Free (Tree);
      end if;
   end Finalize;

   function Find_Height (Tree : in Node_Access_Type) return Integer
   is begin
      if Tree = null then
         return 0;
      else
         return 1 + Integer'Max (Find_Height (Tree.Left), Find_Height (Tree.Right));
      end if;
   end Find_Height;

   procedure Locate_Node
      (Tree   : in     Node_Access_Type;
       Key    : in     Key_Type;
       Node   :    out Node_Access_Type;
       Result :    out Locate_Result_Type;
       Depth  : in out Integer)
   is begin
      if Is_Equal (Key, To_Key (Tree.Item)) then
         Node := Tree;
         Result := Found;
      elsif Is_Greater (Key, To_Key (Tree.Item)) then
         if Tree.Right = null then
            Node := Tree;
            Result := Insert_Right;
         else
            Depth := Depth + 1;
            Locate_Node (Tree.Right, Key, Node, Result, Depth);
         end if;
      else
         if Tree.Left = null then
            Node := Tree;
            Result := Insert_Left;
         else
            Depth := Depth + 1;
            Locate_Node (Tree.Left, Key, Node, Result, Depth);
         end if;
      end if;
   end Locate_Node;

   procedure Move
      (To   : in out Tree_Type'class;
       From : in     Node_Access_Type)
   is begin
      if From = null then
         return;
      elsif To.Root = null then
         To.Root := From;
         To.Height :=  0;
         To.Count :=  0;
         Add_Height_Count (To.Root, To.Height, To.Count);
      else
         declare
            Depth : Integer                   := 1;
            Left  : constant Node_Access_Type := From.Left;
            Right : constant Node_Access_Type := From.Right;
         begin
            From.Left := null;
            From.Right := null;
            Add (To.Root, From, Depth);
            To.Height := Integer'Max (Depth, To.Height);
            To.Count := To.Count + 1;
            Move (To, Left);
            Move (To, Right);
         end;
      end if;
   end Move;

   ----------
   --  public subprograms

   procedure Add
      (Tree : in out Tree_Type;
       Item : in     Item_Type)
   is
      Depth : Integer := 1;
   begin
      if Tree.Root = null then
         Tree.Root := new Node_Type'(To_Item_Node (Item), null, null);
         Tree.Height := 1;
         Tree.Count := 1;
      else
         Add (Tree.Root, Item, Depth);
         Tree.Height := Integer'Max (Depth, Tree.Height);
         Tree.Count  := Tree.Count + 1;
      end if;
   end Add;

   function Count (Tree : in Tree_Type) return Integer
   is begin
      return Tree.Count;
   end Count;

   procedure Delete (Tree : in out Tree_Type; Key : in Key_Type)
   is begin
      Delete (Tree, Tree.Root, Key);
      Tree.Height := Find_Height (Tree.Root);
      Tree.Count := Tree.Count - 1;
   end Delete;

   function Height (Tree : in Tree_Type) return Integer
   is begin
      return Tree.Height;
   end Height;

   overriding procedure Finalize (Tree : in out Tree_Type)
   is begin
      Finalize (Tree.Root);
   end Finalize;

   function Is_Present (Tree : in Tree_Type; Key : in Key_Type) return Boolean
   is
      Node   : Node_Access_Type;
      Result : Locate_Result_Type;
      Depth  : Integer := 0;
   begin
      if Tree.Root = null then
         return False;
      else
         Locate_Node (Tree.Root, Key, Node, Result, Depth);
         case Result is
         when Found =>
            return True;
         when others =>
            return False;
         end case;
      end if;
   end Is_Present;

   procedure Move (To : in out Tree_Type; From : in out Tree_Type)
   is begin
      if From.Root = null or
         To.Root = From.Root
      then
         return;
      else
         Move (To, From.Root);
         From.Root := null;
         From.Height := 0;
         From.Count := 0;
      end if;
   end Move;

   function Retrieve (Tree : in Tree_Type; Key : in Key_Type) return Item_Node_Type
   is
      Node   : Node_Access_Type;
      Result : Locate_Result_Type;
      Depth  : Integer := 0;
   begin
      if Tree.Root = null then
         raise Not_Found;
      else
         Locate_Node (Tree.Root, Key, Node, Result, Depth);
         case Result is
         when Found =>
            return Node.Item;
         when others =>
            raise Not_Found;
         end case;
      end if;
   end Retrieve;

end SAL.Poly.Binary_Trees.Sorted;
