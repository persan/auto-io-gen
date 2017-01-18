--  Abstract :
--
--  Test SAL.Gen.Alg.Find_Linear with indefinite non-limited items
--  (symbols).
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
with Ada.Tags;
with SAL.Gen.Alg.Find_Linear;
with Test_Gen_Alg_Aux.Symbols; use Test_Gen_Alg_Aux.Symbols;
procedure Test_Gen_Alg_Find_Linear_Symbol_Lists
is
   use Test_Gen_Alg_Aux.Symbols.Lists;

   package Algorithms_Find_Symbol_Linear is new Algorithms.Find_Linear
     (Item_Type         => Symbol_Type'Class,
      Key_Type          => Symbol_Type'Class,
      Is_Equal_Node_Key => Is_Equal_Node_Class,
      Delete            => Lists.Delete,
      Insert_Before     => Lists.Insert_Before);

   function Find_Symbol (Start : in Iterator_Type; Key : in Symbol_Type'class) return Iterator_Type
      renames Algorithms_Find_Symbol_Linear.Find_Equal;

   package Algorithms_Find_Tag_Linear is new Algorithms.Find_Linear
     (Item_Type         => Symbol_Type'Class,
      Key_Type          => Ada.Tags.Tag,
      Is_Equal_Node_Key => Is_Equal_Node_Tag,
      Delete            => Lists.Delete,
      Insert_Before     => Lists.Insert_Before);

   function Find_Type (Start : in Iterator_Type; Key : in Ada.Tags.Tag) return Iterator_Type
      renames Algorithms_Find_Tag_Linear.Find_Equal;

   List       : List_Type;
   Float_5    : constant Floating_Point_Type := (Significant_Digits                     => 5);
   Discrete_4 : constant Symbol_Type'Class   := Discrete_Number_Type'(First             => -4, Last => +4);
   Float_0    : constant Symbol_Type'Class   := Floating_Point_Type'(Significant_Digits => 0);
begin
   Put_Line ("SAL.Gen.Alg.Find_Linear, SAL.Poly.Lists.Double, Indefinite non-limited items (symbols)");
   Insert_Tail (List, Float_5);
   Insert_Tail (List, Discrete_4);
   Print_List (List);
   New_Line;

   Put_Line ("Finding exact symbols");
   Put ("Float_5    => "); Print_Symbol (Current (Find_Symbol (First (List), Float_5))); New_Line;
   Put ("Discrete_4 => "); Print_Symbol (Current (Find_Symbol (First (List), Discrete_4))); New_Line;
   if Is_Null (Find_Symbol (First (List), Float_0)) then
      Put_Line ("ok, didn't find Float_0");
   else
      Put_Line ("Oops, found Float_0");
   end if;

   Put_Line ("Finding types");
   Put ("Float    => "); Print_Symbol (Current (Find_Type (First (List), Float_0'Tag))); New_Line;
   Put ("Discrete => "); Print_Symbol (Current (Find_Type (First (List), Discrete_4'Tag))); New_Line;

   --  IMPROVEME: test insert_before, delete

end Test_Gen_Alg_Find_Linear_Symbol_Lists;
