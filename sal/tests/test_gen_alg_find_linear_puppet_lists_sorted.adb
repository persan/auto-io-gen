--  Abstract :
--
--  Test SAL.Gen.Alg.Find_Linear.Sorted, with lists of indefinite
--  tagged limited items (puppets).
--
--  Full test of Sorted algorithms is done in Integer_Lists version;
--  this just verifies that the algorithm compiles with puppets.
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
with Test_Gen_Alg_Aux.Puppets; use Test_Gen_Alg_Aux.Puppets;
procedure Test_Gen_Alg_Find_Linear_Puppet_Lists_Sorted
is
   package Algorithms_Find_Linear is new List_Algorithms.Find_Linear
     (Item_Type         => Parameters_Type,
      Key_Type          => Parameters_Type,
      Is_Equal_Node_Key => Is_Equal_Puppet,
      Delete            => Lists.Delete,
      Insert_Before     => Lists.Insert_Before);

   package Algorithms_Sorted is new Algorithms_Find_Linear.Sorted
      (Is_Equal_Node_Item         => Is_Equal_Puppet,
       Is_Equal_Node_Node         => Is_Equal_Node,
       Is_Greater_Equal_Node_Item => Is_Greater_Equal_Param,
       Is_Greater_Equal_Node_Key  => Is_Greater_Equal_Param,
       Is_Greater_Equal_Node_Node => Is_Greater_Equal_Node,
       Prev_Function              => Lists.Prev,
       Prev_Procedure             => Lists.Prev,
       Splice_Before              => Lists.Splice_Before);
   use Algorithms_Sorted;

   Muppet_2_5 : constant Parameters_Type := (Muppet, 2, 5);
   Muppet_3_4 : constant Parameters_Type := (Muppet, 3, 4);
   Beanie_4   : constant Parameters_Type := (Beanie, 4);
   Beanie_0   : constant Parameters_Type := (Beanie, 0);

   List      : Lists.List_Type;
   Temp_List : Lists.List_Type;
   Iterator  : Lists.Iterator_Type;
begin
   Put_Line ("SAL.Gen.Alg.Find_Linear.Sorted, SAL.Poly.Lists.Double.");
   Put_Line ("Indefinite tagged limited items (puppets).");

   Add (List, Muppet_2_5, SAL.Backward);
   Add (List, Beanie_4);
   Add (List, Beanie_0, SAL.Backward);
   Print_List (List);

   New_Line;
   Put_Line ("Finding Keys");
   declare
      procedure Find_Key
         (List      : in Lists.List_Type;
          Key       : in Parameters_Type;
          Direction : in SAL.Direction_Type)
      is
         Found : Boolean;
      begin
         Find_Greater_Equal_Key (List, Key, Iterator, Found, Direction);
         Print (Key); Put (" (" & SAL.Direction_Type'Image (Direction) & ") => ");
         if Found then
            Print_Puppet (Lists.Current (Iterator)); New_Line;
         else
            if Lists.Is_Null (Iterator) then
               Put_Line ("Insert at tail");
            else
               Put ("Insert_Before "); Print_Puppet (Lists.Current (Iterator)); New_Line;
            end if;
         end if;
      end Find_Key;
   begin
      Find_Key (List, Muppet_2_5, SAL.Forward);
      Find_Key (List, Muppet_3_4, SAL.Backward);
      Find_Key (List, Beanie_0, SAL.Forward);
   end;

   New_Line;
   Put_Line ("Sorting");
   Lists.Finalize (List);
   Lists.Insert_Tail (List, Muppet_2_5);
   Lists.Insert_Tail (List, Beanie_4);
   Lists.Insert_Tail (List, Beanie_0);
   Print_List (List);
   Sort (List, Temp_List);
   Put_Line ("Final list => ");
   Print_List (List);

end Test_Gen_Alg_Find_Linear_Puppet_Lists_Sorted;
