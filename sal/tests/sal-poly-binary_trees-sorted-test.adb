--  Abstract :
--
--  see spec
--
--  Copyright (C) 1999, 2000 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
package body SAL.Poly.Binary_Trees.Sorted.Test is

   procedure Put_Node (Node : in Node_Access_Type; Tree_Level : in Positive_Count := 1)
   is begin
      if Node = null then
         Put ("null");
      else
         Set_Col (Tree_Level * 3);
         Put ("Key => "); Put_Key (To_Key (Node.Item));
         Put ("; Item => "); Put_Item (Node.Item);
         New_Line; Set_Col (Tree_Level * 3);
         Put (" Left  => "); Put_Node (Node.Left, Tree_Level + 1);
         New_Line; Set_Col (Tree_Level * 3);
         Put (" Right => "); Put_Node (Node.Right, Tree_Level + 1); Put (")");
      end if;
   end Put_Node;

   procedure Print_Tree (Tree : in Tree_Type)
   is begin
      Put ("Tree => ");
      if Tree.Root = null then
         Put_Line ("null");
      else
         Put_Node (Tree.Root);
         New_Line;
      end if;
      Put_Line ("Count  => " & Integer'Image (Count (Tree)));
      Put_Line ("Height => " & Integer'Image (Height (Tree)));
      Flush; -- so we see all output so far in case of crash
   end Print_Tree;

end SAL.Poly.Binary_Trees.Sorted.Test;
