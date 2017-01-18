--  Abstract :
--
--  Test SAL.Gen.Alg.Find_Linear with lists of indefinite tagged
--  limited items (puppets)
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
with Test_Gen_Alg_Aux.Puppets; use Test_Gen_Alg_Aux.Puppets;
procedure Test_Gen_Alg_Find_Linear_Puppet_Lists
is
   use Lists;

   package Algorithms_Find_Puppet_Linear is new List_Algorithms.Find_Linear
     (Item_Type         => Parameters_Type,
      Key_Type          => Parameters_Type,
      Is_Equal_Node_Key => Is_Equal_Puppet,
      Delete            => Lists.Delete,
      Insert_Before     => Lists.Insert_Before);

   function Find_Puppet (Start : in Iterator_Type; Key : in Parameters_Type) return Iterator_Type
      renames Algorithms_Find_Puppet_Linear.Find_Equal;

   package Algorithms_Find_Type_Linear is new List_Algorithms.Find_Linear
     (Item_Type         => Parameters_Type,
      Key_Type          => Label_Type,
      Is_Equal_Node_Key => Is_Equal_Label,
      Delete            => Lists.Delete,
      Insert_Before     => Lists.Insert_Before);

   function Find_Type (Start : in Iterator_Type; Key : in Label_Type) return Iterator_Type
      renames Algorithms_Find_Type_Linear.Find_Equal;

   List : List_Type;

   Muppet_2_5 : constant Parameters_Type := (Muppet, 2, 5);
   Beanie_4   : constant Parameters_Type := (Beanie, 4);
   Beanie_0   : constant Parameters_Type := (Beanie, 0);
   Found      : Iterator_Type;
begin
   Put_Line ("SAL.Gen.Alg.Find_Linear, SAL.Poly.Lists.Double,");
   Put_Line ("Indefinite tagged limited items (puppets)");
   Insert_Tail (List, Muppet_2_5);
   Insert_Tail (List, Beanie_4);
   Print_List (List);

   Put_Line ("Finding exact Puppets");
   Put ("Muppet_2_5 => "); Print_Puppet (Current (Find_Puppet (First (List), Muppet_2_5))); New_Line;
   Put ("Beanie_4   => "); Print_Puppet (Current (Find_Puppet (First (List), Beanie_4))); New_Line;

   Found := Find_Puppet (First (List), Beanie_0);
   if Is_Null (Found) then
      Put_Line ("ok, didn't find Beanie_0");
   else
      Put ("Oops, found Beanie_0 => "); Print_Puppet (Current (Found)); New_Line;
   end if;

   Put_Line ("Finding types");
   Put ("Muppet => "); Print_Puppet (Current (Find_Type (First (List), Muppet))); New_Line;
   Put ("Beanie => "); Print_Puppet (Current (Find_Type (First (List), Beanie))); New_Line;

   --  IMPROVEME: test insert_before

end Test_Gen_Alg_Find_Linear_Puppet_Lists;
