--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2000, 2007, 2009 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Test_Cases.Registration;
with SAL.AUnit;
with SAL.Gen.Alg.Find_Linear;
with Test_Poly_Unbounded_Arrays_Aux;
with Test_Storage_Pools;
package body Test_Poly_Unbounded_Arrays_Find_Linear is

   use Test_Poly_Unbounded_Arrays_Aux.Integers;

   package Algorithms is new SAL.Gen.Alg
      (Item_Node_Type => Integer,
       Container_Type => Arrays.Container_Type,
       Iterator_Type  => Arrays.Iterator_Type,
       Current        => Arrays.Current,
       First          => Arrays.First,
       Last           => Arrays.Last,
       None           => Arrays.None,
       Is_Null        => Arrays.Is_Null,
       Next_Procedure => Arrays.Next,
       Next_Function  => Arrays.Next);

   --  Iterators can only point to non-local objects
   Integer_Array : aliased Arrays.Array_Type;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.AUnit;
      use Test_Poly_Unbounded_Arrays_Aux.Integers.Arrays_Test;

      --  Instantiate Algorithms_Find_Linear here, to show that we can
      --  instantiate at other than library level.

      package Algorithms_Find_Linear is new Algorithms.Find_Linear
        (Item_Type         => Integer,
         Key_Type          => Integer,
         Is_Equal_Node_Key => "=",
         Delete            => Arrays.Delete,
         Insert_Before     => Arrays.Insert_Before);

      function Find (Start : in Arrays.Iterator_Type; Key : in Integer) return Arrays.Iterator_Type
        renames Algorithms_Find_Linear.Find_Equal;

      use Arrays;

      Container : Container_Type := Integer_Array'Access;

   begin
      Add_Last (Integer_Array, 1);
      Add_Last (Integer_Array, 2);
      Add_Last (Integer_Array, 3);
      Add_Last (Integer_Array, 4);

      Check
        ("init",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 4,
         First         => 1,
         Last          => 4,
         Base_First    => 1,
         Base_Last     => 4,
         Items         => (1 => 1, 2 => 2, 3 => 3, 4 => 4));

      Check
        ("Find 3",
         Index (Find (First (Integer_Array'Access), 3)),
         3);

      Check
        ("Find 0",
         Is_Null (Find (First (Integer_Array'Access), 0)),
         True);

      Algorithms_Find_Linear.Delete (Container, 2);

      Check
        ("delete",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 4,
         First         => 1,
         Last          => 3,
         Base_First    => 1,
         Base_Last     => 4,
         Items         => (1 => 1, 2 => 3, 3 => 4));

      Algorithms_Find_Linear.Insert_Before (Container, 3, 5, Copies => 2);

      Check
        ("Insert_Before",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 8,
         First         => 1,
         Last          => 5,
         Base_First    => 1,
         Base_Last     => 8,
         Items         => (1 => 1, 2 => 5, 3 => 5, 4 => 3, 5 => 4));

      Finalize (Integer_Array);
   end Nominal;

   ----------
   --  Public subprograms

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Poly_Unbounded_Arrays_Find_Linear");
   end Name;

   procedure Set_Up_Case (T : in out Test_Case)
   is begin
      SAL.AUnit.Check
        ("Allocated_Elements",
         Test_Storage_Pools.Allocated_Elements (Test_Poly_Unbounded_Arrays_Aux.Array_Storage_Pool),
         Expected => 8);
      --  Note that we are _not_ expecting 0 here, because Integer_Array is Initialized at elaboration time.

      Test_Storage_Pools.Set_Debug (Test_Poly_Unbounded_Arrays_Aux.Array_Storage_Pool, T.Debug_Level > 0);
      Test_Storage_Pools.Reset_Counts (Test_Poly_Unbounded_Arrays_Aux.Array_Storage_Pool);
   end Set_Up_Case;

   procedure Tear_Down (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run after each routine
      SAL.AUnit.Check
        ("Allocated_Elements",
         Test_Storage_Pools.Allocated_Elements (Test_Poly_Unbounded_Arrays_Aux.Array_Storage_Pool),
         0);
   end Tear_Down;

end Test_Poly_Unbounded_Arrays_Find_Linear;
