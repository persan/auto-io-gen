--  Abstract:
--
--  Test Sal.Gen.Stacks.Bounded_Limited and Bounded_Nonlimited.
--
--  Copyright (C) 1999, 2000, 2003 Stephen Leake.  All Rights Reserved.
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
with Ada.Command_Line;
with SAL.Gen.Stacks.Bounded_Limited;
with Test_Gen_Stacks_Bounded_Aux;
with Test_Storage_Pools;
procedure Test_Gen_Stacks_Bounded is
   Debug_Level : Integer;
begin

   case Ada.Command_Line.Argument_Count is
   when 0 =>
      --  use default
      Debug_Level := 0;
   when 1 =>
      Debug_Level := Integer'Value (Ada.Command_Line.Argument (1));
   when others =>
      Put_Line ("Usage : test_gen_stacks_bounded <debug_level>");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end case;

   case Debug_Level is
   when 0 =>
      Test_Storage_Pools.Set_Debug (Test_Gen_Stacks_Bounded_Aux.String_Storage_Pool, False);
      Test_Gen_Stacks_Bounded_Aux.Debug := False;
   when 1 =>
      Test_Storage_Pools.Set_Debug (Test_Gen_Stacks_Bounded_Aux.String_Storage_Pool, True);
      Test_Gen_Stacks_Bounded_Aux.Debug := False;
   when 2 =>
      Test_Storage_Pools.Set_Debug (Test_Gen_Stacks_Bounded_Aux.String_Storage_Pool, True);
      Test_Gen_Stacks_Bounded_Aux.Debug := True;
   when others =>
      Put_Line ("Debug_Level not in range 1 .. 2");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end case;

   Put_Line ("Testing Definite_Private_Item stack (Integers)");
   declare
      use Test_Gen_Stacks_Bounded_Aux.Integer_Stacks;

      procedure Put (Stack : in Stack_Type)
      is begin
         if Is_Empty (Stack) then
            Put_Line ("Empty");
         else
            Put_Line ("Depth => " & Integer'Image (Depth (Stack)));
            Flush;
            for I in 1 .. Depth (Stack) loop
               Put_Line (Integer'Image (I) & " => " & Integer'Image (Peek (Stack, I)));
               Flush;
            end loop;
            if Is_Full (Stack) then
               Put_Line ("Full");
            end if;
         end if;
      end Put;

      Stack : Stack_Type (5);
   begin
      Put_Line ("Empty Stack => ");
      Put (Stack);

      Put_Line ("Adding 5 items");
      Push (Stack, 5);
      Push (Stack, 4);
      Push (Stack, 3);
      Push (Stack, 2);
      Push (Stack, 1);

      Put (Stack);
   end;

   New_Line;
   Put_Line ("Testing Indefinite_Private_Item stack (Controlled Names)");
   declare
      use Test_Gen_Stacks_Bounded_Aux;
      package Name_Stacks is new SAL.Gen.Stacks.Bounded_Limited
         (Item_Type => String,
          Item_Node_Type => Name_Type,
          To_Item_Node => Create,
          Free_Item => Free_Name);
      use Name_Stacks;

      procedure Put (Stack : in Stack_Type)
      is begin
         if Is_Empty (Stack) then
            Put_Line ("Empty");
         else
            Put_Line ("Depth => " & Integer'Image (Depth (Stack)));
            Flush;
            for I in 1 .. Depth (Stack) loop
               --  ObjectAda raises Constraint_Error here
               Put_Line (Integer'Image (I) & " => " & Name (Peek (Stack, I)));
               Flush;
            end loop;
            if Is_Full (Stack) then
               Put_Line ("Full");
            end if;
         end if;
      end Put;

      Stack : Stack_Type (5);
   begin
      Put_Line ("Empty Stack => ");
      Put (Stack);

      Put_Line ("Adding 5 items");
      Push (Stack, "fifty_five");
      Push (Stack, "four");
      Push (Stack, "thirty_three");
      Push (Stack, "two");
      Push (Stack, "one");

      Test_Storage_Pools.Show_Storage (Test_Gen_Stacks_Bounded_Aux.String_Storage_Pool);

      Put (Stack);
   end;
   Put_Line ("String stack finalized");
   Test_Storage_Pools.Check_Deallocated (Test_Gen_Stacks_Bounded_Aux.String_Storage_Pool);
end Test_Gen_Stacks_Bounded;
