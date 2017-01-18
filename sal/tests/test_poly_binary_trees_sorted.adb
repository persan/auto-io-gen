--  Abstract:
--
--  Test Sal.Poly.Binary_Trees.Sorted
--
--  Copyright (C) 1997, 1998, 1999 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Poly_Binary_Trees_Sorted_Aux;
with Test_Storage_Pools;
procedure Test_Poly_Binary_Trees_Sorted is

   use Test_Poly_Binary_Trees_Sorted_Aux.String_Trees;
   use Test_Poly_Binary_Trees_Sorted_Aux.String_Trees_Iterators;
   use Test_Poly_Binary_Trees_Sorted_Aux.String_Trees_Test;

   Tree : aliased Tree_Type;

   Debug : Boolean := False;
begin
   if Ada.Command_Line.Argument_Count > 0 then
      Debug := Boolean'Value (Ada.Command_Line.Argument (1));
   end if;

   Test_Storage_Pools.Set_Debug (Test_Poly_Binary_Trees_Sorted_Aux.String_Storage_Pool, Debug);
   Test_Storage_Pools.Set_Debug (Test_Poly_Binary_Trees_Sorted_Aux.Node_Storage_Pool, Debug);
   Test_Storage_Pools.Set_Debug (Test_Poly_Binary_Trees_Sorted_Aux.Iterator_Stack_Storage_Pool,
                                 Debug);

   Put_Line ("null tree:");
   Print_Tree (Tree);

   --  add in an order that gives the following tree:
   --         7
   --    2         9
   --  1   4     8
   --     3 5       8
   --        6        8

   Put_Line ("adding 7");
   Add (Tree, "7");
   Print_Tree (Tree);
   Put_Line ("adding 2");
   Add (Tree, "2");
   Print_Tree (Tree);
   Put_Line ("adding 9");
   Add (Tree, "9");
   Print_Tree (Tree);

   Put_Line ("adding 4, 5, 6");
   Add (Tree, "4");
   Add (Tree, "5");
   Add (Tree, "6");
   Print_Tree (Tree);

   Put_Line ("Adding 1, 3, 8, 8, 8");
   Add (Tree, "1");
   Add (Tree, "3");
   Add (Tree, "8");
   Add (Tree, "8");
   Add (Tree, "8");
   Put_Line ("Initial Tree built");
   Print_Tree (Tree);
   Test_Storage_Pools.Show_Storage (Test_Poly_Binary_Trees_Sorted_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Show_Storage (Test_Poly_Binary_Trees_Sorted_Aux.String_Storage_Pool);

   Put_Line ("Printing nodes in key order");
   declare
      Iterator : Iterator_Type := First (Tree);
   begin
      loop
         exit when Is_Done (Iterator);
         Put_Line (Current (Iterator).all);
         Next (Iterator);
      end loop;
      New_Line;
   end;
   Put_Line ("Iterator finalized");
   Test_Storage_Pools.Check_Deallocated
      (Test_Poly_Binary_Trees_Sorted_Aux.Iterator_Stack_Storage_Pool);

   Put_Line ("Retrieving 1 => " & Retrieve (Tree, "1").all);
   Put_Line ("Retrieving 2 => " & Retrieve (Tree, "2").all);
   Put_Line ("Retrieving 8 => " & Retrieve (Tree, "8").all);
   New_Line;

   Put_Line ("Iterator searching");
   declare
      Iterator : Iterator_Type := First (Tree);

      procedure Test (Key : in String)
         --  Find all nodes with Key. Then find first node with Key,
         --  print rest of tree. This proves Find leaves Iterator setup
         --  correctly.
      is begin
         Put_Line ("Searching for all " & Key & "'s");
         Iterator := Find (Tree, Key);
         loop
            exit when Is_Done (Iterator);
            Put (Current (Iterator).all & ' ');
            Find_Next (Iterator);
         end loop;

         New_Line;
         Put_Line ("Values starting with " & Key);
         Iterator := Find (Tree, Key);
         loop
            exit when Is_Done (Iterator);
            Put (Current (Iterator).all & ' ');
            Next (Iterator);
         end loop;
         New_Line;
      end Test;
   begin
      Test ("10");
      Test ("4");
      Test ("8");
   end;
   Put_Line ("Iterator finalized");
   Test_Storage_Pools.Check_Deallocated
      (Test_Poly_Binary_Trees_Sorted_Aux.Iterator_Stack_Storage_Pool);

   Put_Line ("testing Move");
   declare
      Other_Tree : Tree_Type;
      Empty_Tree : Tree_Type;
   begin
      Add (Other_Tree, "7 seven");
      Add (Other_Tree, "2 two");
      Add (Other_Tree, "1 one");
      Add (Other_Tree, "4 four");
      Add (Other_Tree, "3 three");
      Add (Other_Tree, "5 five");
      Add (Other_Tree, "6 six");

      Put_Line ("Tree to merge into empty tree");
      Print_Tree (Other_Tree);
      Move (Empty_Tree, Other_Tree);
      Print_Tree (Empty_Tree);

      Put_Line ("Original tree (should be empty):");
      Print_Tree (Other_Tree);

      Put_Line ("Merging another tree:");
      Move (Tree, Empty_Tree);
      Print_Tree (Tree);
   end;
   Put_Line ("Move completed");
   Test_Storage_Pools.Show_Storage (Test_Poly_Binary_Trees_Sorted_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Show_Storage (Test_Poly_Binary_Trees_Sorted_Aux.String_Storage_Pool);

   New_Line;
   Put_Line ("Testing Clear/Finalize");
   Clear (Tree);
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Binary_Trees_Sorted_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Binary_Trees_Sorted_Aux.String_Storage_Pool);

   Test_Storage_Pools.Reset_Counts (Test_Poly_Binary_Trees_Sorted_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Reset_Counts (Test_Poly_Binary_Trees_Sorted_Aux.String_Storage_Pool);
   New_Line;
   Put_Line ("Testing Delete");
   Add (Tree, "7");
   Add (Tree, "2");
   Add (Tree, "9");
   Add (Tree, "4");
   Add (Tree, "5");
   Add (Tree, "6");
   Add (Tree, "1");
   Add (Tree, "3");
   Add (Tree, "8");
   Add (Tree, "8");
   Add (Tree, "8");
   Print_Tree (Tree);
   Put_Line ("delete 6");
   Delete (Tree, "6"); -- leaf
   Print_Tree (Tree);
   Put_Line ("delete 2");
   Delete (Tree, "2"); -- sub tree
   Print_Tree (Tree);
   Put_Line ("delete 7");
   Delete (Tree, "7"); -- root
   Print_Tree (Tree);
   Test_Storage_Pools.Show_Storage (Test_Poly_Binary_Trees_Sorted_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Show_Storage (Test_Poly_Binary_Trees_Sorted_Aux.String_Storage_Pool);
   Put_Line ("delete rest");
   Delete (Tree, "8");
   Delete (Tree, "8");
   Delete (Tree, "8");
   Delete (Tree, "3");
   Delete (Tree, "1");
   Delete (Tree, "5");
   Delete (Tree, "4");
   Delete (Tree, "9");
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Binary_Trees_Sorted_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Binary_Trees_Sorted_Aux.String_Storage_Pool);
end Test_Poly_Binary_Trees_Sorted;
