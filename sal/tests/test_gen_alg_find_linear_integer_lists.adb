--  Abstract :
--
--  Test SAL.Gen.Alg.Find_Linear, with lists of definite non-tagged
--  non-limited items
--
--  Copyright (C) 2000, 2007 Stephen Leake.  All Rights Reserved.
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
with SAL.Gen.Alg.Find_Linear;
with Test_Gen_Alg_Aux.Integers; use Test_Gen_Alg_Aux.Integers;
procedure Test_Gen_Alg_Find_Linear_Integer_Lists
is
   use Lists;
   package Algorithms_Find_Linear is new List_Algorithms.Find_Linear
     (Item_Type         => Integer,
      Key_Type          => Integer,
      Is_Equal_Node_Key => "=",
      Delete            => Lists.Delete,
      Insert_Before     => Insert_Before);

   function Find_Integer (Start : in Iterator_Type; Key : in Integer) return Iterator_Type
      renames Algorithms_Find_Linear.Find_Equal;

   List : List_Type;
begin
   Put_Line ("SAL.Gen.Alg.Find_Linear, SAL.Poly.Lists.Double,");
   Put_Line ("Definite non-tagged non-limited items (integer_lists)");
   Insert_Tail (List, 1);
   Insert_Tail (List, 3);
   Insert_Tail (List, 5);
   Print_List (List);

   Put_Line ("Finding");
   Put_Line ("5 => " & Integer'Image (Current (Find_Integer (First (List), 5))));
   Put_Line ("3 => " & Integer'Image (Current (Find_Integer (First (List), 3))));
   if Is_Null (Find_Integer (First (List), 0)) then
      Put_Line ("ok, didn't find 0");
   else
      Put_Line ("Oops, found 0");
   end if;

   --  IMPROVEME: test insert_before

end Test_Gen_Alg_Find_Linear_Integer_Lists;
