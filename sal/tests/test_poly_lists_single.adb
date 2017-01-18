--  Abstract:
--
--  Test Sal.Poly.Lists.Single, with various item types.
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

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Poly_Lists_Single_Aux;
with Test_Storage_Pools;
procedure Test_Poly_Lists_Single is
   Debug : Boolean := False;
begin
   if Ada.Command_Line.Argument_Count > 0 then
      Debug := True;
   end if;

   Test_Storage_Pools.Set_Debug (Test_Poly_Lists_Single_Aux.Node_Storage_Pool, Debug);

   Put_Line ("Definite non-tagged non-limited items (integer_lists)");
   Integer_Lists :
   declare
      use Test_Poly_Lists_Single_Aux.Integers;
      use Lists;
      use Iterators;

      List : aliased List_Type;
   begin
      Put_Line ("null List:");
      Print_List (List);
      Put_Line ("Is_Empty (List) => " & Boolean'Image (Is_Empty (List)));

      Put_Line ("Insert_Tail 1 3 5");
      Insert_Tail (List, 1);
      Insert_Tail (List, 3);
      Insert_Tail (List, 5);
      Print_List (List);
      Put_Line ("Is_Empty (List) => " & Boolean'Image (Is_Empty (List)));

      Put_Line ("Insert_Head 0");
      Insert_Head (List, 0);
      Print_List (List);

      Put_Line ("Delete_Head = " & Integer'Image (Head (List)));
      Delete_Head (List);
      Print_List (List);

      Put_Line ("Tail => " & Integer'Image (Tail (List)));

      --  Count tested by Print_List

   end Integer_Lists;
   Put_Line ("Integer list finalized");
   Test_Storage_Pools.Show_Storage (Test_Poly_Lists_Single_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Node_Storage_Pool);

   ------------

   New_Line;
   Put_Line ("Indefinite tagged non-limited items (symbols)");
   Symbol_Lists :
   declare
      use Test_Poly_Lists_Single_Aux.Symbols;
      use Lists;
      use Iterators;

      List       : aliased List_Type;
      Float_5    : constant Floating_Point_Type  := (Significant_Digits => 5);
      Discrete_4 : constant Discrete_Number_Type := (First              => -4, Last => +4);
   begin
      Put_Line ("Insert_Tail some symbols");
      Test_Storage_Pools.Set_Debug (Storage_Pool, Debug);
      Insert_Tail (List, Float_5);
      Insert_Tail (List, Discrete_4);
      Print_List (List);
      Test_Storage_Pools.Show_Storage (Storage_Pool);
   end Symbol_Lists;
   Put_Line ("Symbols list finalized");
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Symbols.Storage_Pool);

   -------------

   New_Line;
   Put_Line ("Indefinite tagged limited items (puppets)");
   Puppet_Lists :
   declare
      use Test_Poly_Lists_Single_Aux.Puppets;
      use Lists;
      use Iterators;

      List       : aliased List_Type;
      Muppet_2_5 : constant Parameters_Type := (Muppet, 2, 5);
      Beanie_4   : constant Parameters_Type := (Beanie, 4);
   begin
      Put_Line ("Insert_Tailing some puppets");
      Test_Storage_Pools.Set_Debug (Storage_Pool, Debug);
      Insert_Tail (List, Muppet_2_5);
      Insert_Tail (List, Beanie_4);
      Print_List (List);

      Test_Storage_Pools.Show_Storage (Storage_Pool);
   end Puppet_Lists;
   Put_Line ("Puppets list finalized");
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Show_Storage (Test_Poly_Lists_Single_Aux.Puppets.Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Poly_Lists_Single_Aux.Puppets.Storage_Pool);
end Test_Poly_Lists_Single;
