--  Abstract :
--
--  Like the name says
--
--  Copyright (C) 1999, 2000, 2003 Stephen Leake.  All Rights Reserved.
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
with SAL;
with Test_Poly_Stacks_Unbounded_Array_Aux;
with Test_Storage_Pools;
procedure Test_Poly_Stacks_Unbounded_Array is
   use Test_Poly_Stacks_Unbounded_Array_Aux.Unbounded_Integer_Stacks,
      Test_Poly_Stacks_Unbounded_Array_Aux.Integer_Stacks_Test;

   Stack : Unbounded_Stack_Type;

   procedure Ignore (Temp : in Integer)
   is
      pragma Unreferenced (Temp);
   begin
      null;
   end Ignore;

begin

   Put_Line ("Stack initialized");
   Test_Storage_Pools.Show_Storage (Test_Poly_Stacks_Unbounded_Array_Aux.Integer_Storage_Pool);
   Put (Stack);

   Put_Line ("Is_Empty (empty stack) => " & Boolean'Image (Is_Empty (Stack)));

   Put_Line ("Testing for exceptions");
   begin
      Ignore (Peek (Stack, 1));
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;

   begin
      Pop (Stack);
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;

   --  can't get Container_Full with Unbounded stacks.
   begin
      Ignore (Top (Stack));
      Put_Line ("oops, did NOT raise Container_Empty");
   exception
   when SAL.Container_Empty =>
      Put_Line ("ok, got Container_Empty");
   end;

   Put_Line ("Push, Top, Pop");
   Push (Stack, -101);
   Put_Line ("Top => " & Integer'Image (Top (Stack)));
   Put (Stack);
   Pop (Stack);
   Put (Stack);

   Put_Line ("Push, Peek");
   Push (Stack, 10);
   Push (Stack, 9);
   Push (Stack, 8);
   Push (Stack, 7);
   Push (Stack, 6);
   Push (Stack, 5);
   Push (Stack, 4);
   Push (Stack, 3);
   Push (Stack, 2);
   Push (Stack, 1);
   Put (Stack);

   Put_Line ("Peek (3) => " & Integer'Image (Peek (Stack, 3)));
   Put_Line ("Depth => " & Integer'Image (Depth (Stack)));

   Test_Storage_Pools.Show_Storage (Test_Poly_Stacks_Unbounded_Array_Aux.Integer_Storage_Pool);

   Put_Line ("Clearing");
   Clear (Stack);
   Test_Storage_Pools.Show_Storage (Test_Poly_Stacks_Unbounded_Array_Aux.Integer_Storage_Pool);

   Put_Line ("Using stack after Clearing");
   Push (Stack, 2);
   Push (Stack, 1);
   Put (Stack);
   Test_Storage_Pools.Show_Storage (Test_Poly_Stacks_Unbounded_Array_Aux.Integer_Storage_Pool);

   Put_Line ("Pop (item)");
   declare
      Item : Integer;
   begin
      Pop (Stack, Item);
      Put ("Item => " & Integer'Image (Item));
   end;

end Test_Poly_Stacks_Unbounded_Array;
