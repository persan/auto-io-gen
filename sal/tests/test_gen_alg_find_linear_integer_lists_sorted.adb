--  Abstract :
--
--  Test SAL.Gen.Alg.Find_Linear.Sorted, with lists of definite
--  non-tagged non-limited items
--
--  Copyright (C) 2000, 2003, 2007 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen.Alg.Find_Linear.Sorted;
with Test_Gen_Alg_Aux.Integers; use Test_Gen_Alg_Aux.Integers;
procedure Test_Gen_Alg_Find_Linear_Integer_Lists_Sorted
is
   package Algorithms_Find_Linear is new List_Algorithms.Find_Linear
     (Item_Type         => Integer,
      Key_Type          => Integer,
      Is_Equal_Node_Key => "=",
      Delete            => Lists.Delete,
      Insert_Before     => Lists.Insert_Before);

   package Algorithms_Sorted is new Algorithms_Find_Linear.Sorted
      (Is_Equal_Node_Item         => "=",
       Is_Equal_Node_Node         => "=",
       Is_Greater_Equal_Node_Item => ">=",
       Is_Greater_Equal_Node_Key  => ">=",
       Is_Greater_Equal_Node_Node => ">=",
       Prev_Function              => Lists.Prev,
       Prev_Procedure             => Lists.Prev,
       Splice_Before              => Lists.Splice_Before);
   use Algorithms_Sorted;

   Short_List : Lists.List_Type;
   List       : Lists.List_Type;
   Temp_List  : Lists.List_Type;
   Iterator   : Lists.Iterator_Type;
   Found      : Boolean;
begin
   Put_Line ("SAL.Gen.Alg.Find_Linear.Sorted, SAL.Poly.Lists.Double.");
   Put_Line ("Definite non-tagged non-limited items (integer_lists)");
   Lists.Insert_Tail (List, 1);
   Lists.Insert_Tail (List, 3);
   Print_List (List);

   Put_Line ("Adding 6, backward");
   Add (List, 6, SAL.Backward);
   Print_List (List);
   Put_Line ("Adding 2, forward");
   Add (List, 2);
   Print_List (List);
   Put_Line ("Adding 4, backward");
   Add (List, 4, SAL.Backward);
   Print_List (List);

   Lists.Insert_Tail (Short_List, 5);

   New_Line;
   Put_Line ("Finding Keys");
   declare
      procedure Find_Key
         (List      : in Lists.List_Type;
          Key       : in Integer;
          Direction : in SAL.Direction_Type)
      is begin
         Find_Greater_Equal_Key (List, Key, Iterator, Found, Direction);
         Put (Integer'Image (Key) & " (" & SAL.Direction_Type'Image (Direction) & ") => ");
         if Found then
            Put_Line (Integer'Image (Lists.Current (Iterator)));
         else
            if Lists.Is_Null (Iterator) then
               Put_Line ("Insert at tail");
            else
               Put_Line ("Insert_Before " & Integer'Image (Lists.Current (Iterator)));
            end if;
         end if;
      end Find_Key;
   begin
      Put_Line ("Short List");
      Print_List (Short_List);
      Find_Key (Short_List, 6, SAL.Forward);
      Find_Key (Short_List, 5, SAL.Forward);
      Find_Key (Short_List, 0, SAL.Forward);
      Find_Key (Short_List, 0, SAL.Backward);

      New_Line;
      Put_Line ("List");
      Print_List (List);
      Find_Key (List, 6, SAL.Forward);
      Find_Key (List, 3, SAL.Backward);
      Find_Key (List, 0, SAL.Forward);
      Find_Key (List, 5, SAL.Backward);
      Find_Key (List, 7, SAL.Backward);
   end;

   New_Line;
   Put_Line ("Finding Nodes");
   declare
      procedure Find_Node
         (List      : in Lists.List_Type;
          Key       : in Integer;
          Direction : in SAL.Direction_Type)
      is begin
         Find_Greater_Equal_Node (List, Key, Iterator, Found, Direction);
         Put (Integer'Image (Key) & " (" & SAL.Direction_Type'Image (Direction) & ") => ");
         if Found then
            Put_Line (Integer'Image (Lists.Current (Iterator)));
         else
            if Lists.Is_Null (Iterator) then
               Put_Line ("Insert at tail");
            else
               Put_Line ("Insert_Before " & Integer'Image (Lists.Current (Iterator)));
            end if;
         end if;
      end Find_Node;
   begin
      Put_Line ("Short List");
      Print_List (Short_List);
      Find_Node (Short_List, 6, SAL.Forward);
      Find_Node (Short_List, 5, SAL.Forward);
      Find_Node (Short_List, 0, SAL.Forward);
      Find_Node (Short_List, 0, SAL.Backward);

      New_Line;
      Put_Line ("List");
      Find_Node (List, 6, SAL.Forward);
      Find_Node (List, 3, SAL.Backward);
      Find_Node (List, 0, SAL.Forward);
      Find_Node (List, 5, SAL.Backward);
      Find_Node (List, 7, SAL.Backward);
   end;

   New_Line;
   Put_Line ("Finding Items");
   declare
      procedure Find_Item
         (List      : in Lists.List_Type;
          Item      : in Integer;
          Direction : in SAL.Direction_Type)
      is begin
         Find_Greater_Equal_Item (List, Item, Iterator, Found, Direction);
         Put (Integer'Image (Item) & " (" & SAL.Direction_Type'Image (Direction) & ") => ");
         if Found then
            Put_Line (Integer'Image (Lists.Current (Iterator)));
         else
            if Lists.Is_Null (Iterator) then
               Put_Line ("Insert at tail");
            else
               Put_Line ("Insert_Before " & Integer'Image (Lists.Current (Iterator)));
            end if;
         end if;
      end Find_Item;
   begin
      Put_Line ("Short List");
      Print_List (Short_List);
      Find_Item (Short_List, 6, SAL.Forward);
      Find_Item (Short_List, 5, SAL.Forward);
      Find_Item (Short_List, 0, SAL.Forward);
      Find_Item (Short_List, 0, SAL.Backward);

      New_Line;
      Put_Line ("List");
      Find_Item (List, 6, SAL.Forward);
      Find_Item (List, 3, SAL.Backward);
      Find_Item (List, 0, SAL.Forward);
      Find_Item (List, 5, SAL.Backward);
      Find_Item (List, 7, SAL.Backward);
   end;

   New_Line;
   declare
      procedure Remove (Comment : in String)
      is begin
         Put_Line ("Remove_Out_Of_Order " & Comment);
         Put_Line ("List (before) => ");
         Print_List (List);
         Remove_Out_Of_Order (List, Temp_List);
         Put_Line ("List (after) => ");
         Print_List (List);
         Put_Line ("Temp_List => ");
         Print_List (Temp_List);
         New_Line;
      end Remove;
   begin
      Remove ("None");

      Lists.Insert_Tail (List, 5);
      Remove ("5 at end");

      Iterator := Lists.First (List);
      Lists.Next (Iterator);
      Lists.Next (Iterator);
      Lists.Insert_After (List, Iterator, 7);
      Lists.Insert_After (List, Iterator, 0);
      Remove ("0, 7 in middle");

      Lists.Insert_Head (List, 10);
      Remove ("10 at head");
   end;

   New_Line;
   Put_Line ("Merge");
   Merge (List, Temp_List);
   Put_Line ("List => ");
   Print_List (List);
   Put_Line ("Temp_List => ");
   Print_List (Temp_List);
   New_Line;

   New_Line;
   Put_Line ("Sorting (no change)");
   Lists.Finalize (List);
   Lists.Finalize (Temp_List);
   Lists.Insert_Tail (List, 1);
   Lists.Insert_Tail (List, 2);
   Lists.Insert_Tail (List, 3);
   Lists.Insert_Tail (List, 4);
   Lists.Insert_Tail (List, 6);
   Print_List (List);
   Sort (List, Temp_List);
   Print_List (List);

   Put_Line ("Sorting (5 at end)");
   Lists.Insert_Tail (List, 5);
   Print_List (List);
   Sort (List, Temp_List);
   Print_List (List);

   Put_Line ("Sorting (0, 7 in middle)");
   Iterator := Lists.First (List);
   Lists.Next (Iterator);
   Lists.Next (Iterator);
   Lists.Insert_After (List, Iterator, 7);
   Lists.Insert_After (List, Iterator, 0);
   Print_List (List);
   Sort (List, Temp_List);
   Put_Line ("Final list => ");
   Print_List (List);

end Test_Gen_Alg_Find_Linear_Integer_Lists_Sorted;
