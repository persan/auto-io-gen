--  Abstract :
--
--  Test SAL.Gen.Alg.Find_Linear.Sorted, with indefinite non-limited
--  items (symbols).
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
with Test_Gen_Alg_Aux.Symbols; use Test_Gen_Alg_Aux.Symbols;
procedure Test_Gen_Alg_Find_Linear_Symbol_Lists_Sorted
is
   package Algorithms_Find_Linear is new Algorithms.Find_Linear
     (Item_Type         => Symbol_Type'Class,
      Key_Type          => Symbol_Type'Class,
      Is_Equal_Node_Key => Is_Equal_Node_Class,
      Delete            => Lists.Delete,
      Insert_Before     => Lists.Insert_Before);

   package Algorithms_Sorted is new Algorithms_Find_Linear.Sorted
     (Is_Equal_Node_Item         => Is_Equal_Node_Class,
      Is_Equal_Node_Node         => Is_Equal_Node,
      Is_Greater_Equal_Node_Item => Is_Greater_Equal_Node_Class,
      Is_Greater_Equal_Node_Key  => Is_Greater_Equal_Node_Class,
      Is_Greater_Equal_Node_Node => Is_Greater_Equal_Node,
      Prev_Function              => Lists.Prev,
      Prev_Procedure             => Lists.Prev,
      Splice_Before              => Lists.Splice_Before);
   use Algorithms_Sorted;

   Float_5    : constant Floating_Point_Type := (Significant_Digits                     => 5);
   Float_4    : constant Floating_Point_Type := (Significant_Digits                     => 4);
   Discrete_4 : constant Symbol_Type'Class   := Discrete_Number_Type'(First             => -4, Last => +4);
   Float_0    : constant Symbol_Type'Class   := Floating_Point_Type'(Significant_Digits => 0);

   List      : Lists.List_Type;
   Temp_List : Lists.List_Type;
   Iterator  : Lists.Iterator_Type;
begin
   Put_Line ("SAL.Gen.Alg.Find_Linear.Sorted, SAL.Poly.Lists.Double.");
   Put_Line ("Indefinite non-limited items (symbols).");

   Add (List, Float_5, SAL.Backward);
   Add (List, Discrete_4);
   Add (List, Float_4, SAL.Backward);
   Print_List (List);

   New_Line;
   Put_Line ("Finding Keys");
   declare
      procedure Find_Key
         (List      : in Lists.List_Type;
          Key       : in Symbol_Type'class;
          Direction : in SAL.Direction_Type)
      is
         Found : Boolean;
      begin
         Find_Greater_Equal_Key (List, Key, Iterator, Found, Direction);
         Print (Key); Put (" (" & SAL.Direction_Type'Image (Direction) & ") => ");
         if Found then
            Print_Symbol (Lists.Current (Iterator)); New_Line;
         else
            if Lists.Is_Null (Iterator) then
               Put_Line ("Insert at tail");
            else
               Put ("Insert_Before "); Print_Symbol (Lists.Current (Iterator)); New_Line;
            end if;
         end if;
      end Find_Key;
   begin
      Find_Key (List, Float_5, SAL.Forward);
      Find_Key (List, Discrete_4, SAL.Backward);
      Find_Key (List, Float_0, SAL.Forward);
   end;

   New_Line;
   Put_Line ("Sorting");
   Lists.Finalize (List);
   Lists.Insert_Tail (List, Float_5);
   Lists.Insert_Tail (List, Discrete_4);
   Lists.Insert_Tail (List, Float_4);
   Print_List (List);
   Sort (List, Temp_List);
   Put_Line ("Final list => ");
   Print_List (List);

end Test_Gen_Alg_Find_Linear_Symbol_Lists_Sorted;
