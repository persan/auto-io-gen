--  Abstract :
--
--  The base generic sorted binary tree, allowing definite or indefinite,
--  limited or non-limited key and value types.
--
--  Tree_Type is Controlled to recover storage when a tree goes out of scope.
--  Tree_Type is Limited because I don't want to test a deep copy (but I
--  will someday :).
--
--  Copyright (C) 1998, 2000 - 2003, 2005 Stephen Leake.  All Rights Reserved.
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
--
with Ada.Finalization;
with System.Storage_Pools;
generic
   type Item_Type (<>) is limited private;
   type Item_Node_Type is private;
   with function To_Item_Node (Item : in Item_Type) return Item_Node_Type;
   with procedure Free_Item (Item : in out Item_Node_Type);
   --  If Item_Type is definite non-limited, Item_Node_Type should
   --  just be Item_Type. Then To_Item_Node and To_Item should just
   --  return Item, and Free_Item should be null. See
   --  Sal.Aux.Definite_Private_Items.
   --
   --  If Item_Type is indefinite or limited, Item_Node_Type should be
   --  'access Item_Type'. Then To_Item_Node should allocate Item and
   --  return a pointer to it, and Free_Item should be
   --  Unchecked_Deallocation.
   --
   --  Other usages are possible.

   type Key_Type (<>) is limited private;
   with function To_Key (Item : in Item_Node_Type) return Key_Type;
   --  Key_Type can be Item_Type, or a value derived from Item_Type.
   --  The results of To_Key are not cached, so it should be a fast
   --  function.

   with function Is_Greater (Left, Right : in Key_Type) return Boolean;
   with function Is_Equal   (Left, Right : in Key_Type) return Boolean;

   Duplicate_Key_Action : in Duplicate_Action_Type := Error;

   Node_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   --  Root_Storage_Pool is limited, which does not allow defaults.
   --  Default for a global list type should be
   --  System.Storage_Pools.Root_Storage_Pool'Class
   --  (<some_global_access_type>'Storage_Pool). Default for a local
   --  list type, which should be reclaimed when the list type goes
   --  out of scope, is implementation defined (sigh).
package SAL.Poly.Binary_Trees.Sorted is
   pragma Elaborate_Body; -- Storage_Pools is.

   type Tree_Type is new Ada.Finalization.Limited_Controlled with private;

   ----------
   --  Dispatching Tree operations (alphabetical order)

   procedure Add (Tree : in out Tree_Type; Item : in Item_Type);
   --  Insert copy of Item into Tree at appropriate place.
   --
   --  If To_Key (To_Item_Node (Item)) is already in Tree and
   --  Duplicate_Key_Action is:
   --  when Allow  => Item is added
   --  when Ignore => Item is not added
   --  when Error  => raises Duplicate_Key.

   function Count (Tree : in Tree_Type) return Integer;
   --  Return count of items of Tree. Empty Tree has count 0.

   procedure Delete (Tree : in out Tree_Type; Key : in Key_Type);
   --  Delete node containing Key from Tree.
   --
   --  Raises Not_Found if Key is not in Tree.

   function Height (Tree : in Tree_Type) return Integer;
   --  Return Height of Tree. Empty Tree has Height 0.

   overriding procedure Finalize (Tree : in out Tree_Type);
   procedure Clear (Tree : in out Tree_Type) renames Finalize;
   --  Free all items in Tree.

   procedure Move (To : in out Tree_Type; From : in out Tree_Type);
   --  Add all items in From to To; From is emptied.
   --
   --  From is processed top down; merging into a null tree results in
   --  the same tree structure.
   --
   --  Note that nodes are simply relinked, not copied and deleted.

   function Retrieve (Tree : in Tree_Type; Key : in Key_Type) return Item_Node_Type;
   function Find (Tree : in Tree_Type; Key : in Key_Type) return Item_Node_Type
     renames Retrieve;
   --  Do binary tree search, return (access to) Item stored with Key.
   --
   --  Raises SAL.Not_Found if there is no such item.

   function Is_Present (Tree : in Tree_Type; Key : in Key_Type) return Boolean;
   --  True if Key is in Tree, False otherwise.

private

   type Node_Type;

   type Node_Access_Type is access Node_Type;
   for Node_Access_Type'Storage_Pool use Node_Storage_Pool;

   type Node_Type is record
      Item  : Item_Node_Type;
      Left  : Node_Access_Type;
      Right : Node_Access_Type;
   end record;

   type Tree_Type is new Ada.Finalization.Limited_Controlled with record
      Root   : Node_Access_Type;
      Height : Integer := 0;
      Count  : Integer := 0;
   end record;

   --  Used in Iterators child, and in body
   type Locate_Result_Type is (Found, Insert_Left, Insert_Right);

end SAL.Poly.Binary_Trees.Sorted;
