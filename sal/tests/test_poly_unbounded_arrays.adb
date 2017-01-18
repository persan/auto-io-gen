--  Abstract :
--
--  see spec
--
--  Copyright (C) 1999, 2002, 2005, 2007, 2009 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with SAL.AUnit;
with Test_Poly_Unbounded_Arrays_Aux;
with Test_Storage_Pools;
package body Test_Poly_Unbounded_Arrays is

   use Test_Poly_Unbounded_Arrays_Aux;
   use Test_Poly_Unbounded_Arrays_Aux.Integers.Arrays;
   use Test_Poly_Unbounded_Arrays_Aux.Integers.Arrays_Test;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      Integer_Array : Array_Type;
      Other_Array   : Array_Type;

   begin
      Create
        (Array_Obj => Integer_Array,
         Space     => 0,
         Growth    => Append,
         First     => 10);

      Check
        ("init",
         Integer_Array,
         Growth        => Append,
         Initial_Space =>  0,
         Max_Space     =>  0,
         First         => 10,
         Last          =>  9,
         Base_First    => 10,
         Base_Last     =>  9,
         Items         => (10 .. 9 => 0));

      --  Array_Storage_Pool contains just the array bounds at this point.

      begin
         Set (Integer_Array, 1, 100);
         Assert (False, "oops, did NOT raise Constraint_Error 1");
      exception
      when Constraint_Error =>
         null;
      end;

      begin
         Set (Integer_Array, 11, 100);
         Assert (False, "oops, did NOT raise Constraint_Error 2");
      exception
      when Constraint_Error =>
         null;
      end;

      begin
         Delete_First (Integer_Array);
         Assert (False, "oops, did NOT raise Constraint_Error 3");
      exception
      when Constraint_Error =>
         null;
      end;

      begin
         Delete_Last (Integer_Array);
         Assert (False, "oops, did NOT raise Constraint_Error 4");
      exception
      when Constraint_Error =>
         null;
      end;

      begin
         --  can't Add_First to 'append' array
         Add_First (Integer_Array, -101);
         Assert (False, "oops, did NOT raise Constraint_Error 5");
      exception
      when Constraint_Error =>
         null;
      end;

      Other_Array := Null_Array;
      Add_Last (Other_Array, 101);
      Check
        ("Add_Last null",
         Other_Array,
         Growth        => BOTH,
         Initial_Space => 1,
         Max_Space     => 1,
         First         => 1,
         Last          => 1,
         Base_First    => 1,
         Base_Last     => 1,
         Items         => (1 => 101));

      Add_Last (Integer_Array, 101);
      Check
        ("Add_Last 1",
         Integer_Array,
         Growth        => Append,
         Initial_Space => 0,
         Max_Space     => 1,
         First         => 10,
         Last          => 10,
         Base_First    => 10,
         Base_Last     => 10,
         Items         => (10 => 101));

      begin
         Set_Grow (Integer_Array, 8, -102);
         Assert (False, "oops, did NOT raise Constraint_Error Set_Grow 8");
      exception
      when Constraint_Error =>
         null;
      end;

      Set_Grow (Integer_Array, 11, 102);
      Check
        ("Set_Grow 11",
         Integer_Array,
         Growth        => Append,
         Initial_Space => 0,
         Max_Space     => 2,
         First         => 10,
         Last          => 11,
         Base_First    => 10,
         Base_Last     => 11,
         Items         => (10 => 101, 11 => 102));

      Other_Array := Null_Array;
      Set_Grow (Other_Array, 8, 100);
      Check
        ("Set_Grow null",
         Other_Array,
         Growth        => BOTH,
         Initial_Space => 1,
         Max_Space     => 1,
         First         => 8,
         Last          => 8,
         Base_First    => 8,
         Base_Last     => 8,
         Items         => (8 => 100));

      Finalize (Other_Array);

      Create (Other_Array, First => 1, Last => 0);
      Set_Grow (Other_Array, 8, 100);
      Check
        ("Set_Grow null created",
         Other_Array,
         Growth        => BOTH,
         Initial_Space => 0,
         Max_Space     => 8,
         First         => 8,
         Last          => 8,
         Base_First    => 1,
         Base_Last     => 8,
         Items         => (8 => 100));
      Finalize (Other_Array);

      Delete_First (Integer_Array);
      Check
        ("Delete first",
         Integer_Array,
         Growth        => Append,
         Initial_Space => 0,
         Max_Space     => 2,
         First         => 11,
         Last          => 11,
         Base_First    => 10,
         Base_Last     => 11,
         Items         => (11 => 102));

      Delete_Last (Integer_Array);
      Check
        ("Delete last",
         Integer_Array,
         Growth        => Append,
         Initial_Space => 0,
         Max_Space     => 2,
         First         => 11,
         Last          => 10,
         Base_First    => 10,
         Base_Last     => 10,
         Items         => (11 .. 10 => 0));

      --  shrink on delete further tested below

      --  Let block exit finalize Integer_Array

   end Nominal;

   procedure Prepend_Array (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      Integer_Array : Array_Type;

   begin
      Create
        (Array_Obj => Integer_Array,
         Space     => 0,
         Growth    => Prepend,
         Last      => 10);

      Add_First (Integer_Array, -101);
      begin
         --  can't Add_Last to 'prepend' array
         Add_Last (Integer_Array, 101);
         Assert (False, "oops, did NOT raise Constraint_Error");
      exception
      when Constraint_Error =>
         null;
      end;
      Check
        ("Add first",
         Integer_Array,
         Growth        => Prepend,
         Initial_Space => 0,
         Max_Space     => 1,
         First         => 10,
         Last          => 10,
         Base_First    => 10,
         Base_Last     => 10,
         Items         => (10 => -101));

      --  set item 2 less than current First, to check resize
      Set_Grow (Integer_Array, 8, -103);
      Set_Grow (Integer_Array, 9, -102);
      begin
         Set_Grow (Integer_Array, 11, 102);
         Assert (False, "oops, did NOT raise Constraint_Error");
      exception
      when Constraint_Error =>
         null;
      end;
      Check
        ("Set_Grow",
         Integer_Array,
         Growth        => Prepend,
         Initial_Space => 0,
         Max_Space     => 4,
         First         => 8,
         Last          => 10,
         Base_First    => 7,
         Base_Last     => 10,
         Items         => (8 => -103, 9 => -102, 10 => -101));

      --  add enough to grow, then delete enough to shrink
      for I in reverse 0 .. First (Integer_Array) - 1 loop
         Add_First (Integer_Array, Get (Integer_Array, I + 1) - 1);
      end loop;
      Check
        ("Add Grow",
         Integer_Array,
         Growth        => Prepend,
         Initial_Space => 0,
         Max_Space     => 16,
         First         => 0,
         Last          => 10,
         Base_First    => -5,
         Base_Last     => 10,
         Items         =>
           (0 => -111,
            1 => -110,
            2 => -109,
            3 => -108,
            4 => -107,
            5 => -106,
            6 => -105,
            7 => -104,
            8 => -103,
            9 => -102,
            10 => -101));

      for I in 0 .. 9 loop
         Delete_First (Integer_Array);
      end loop;
      Check
        ("Delete Shrink",
         Integer_Array,
         Growth        => Prepend,
         Initial_Space => 0,
         Max_Space     => 16,
         First         => 10,
         Last          => 10,
         Base_First    =>  7,
         Base_Last     => 10,
         Items         => (10 => -101));

      --  Let block exit finalize Integer_Array
   end Prepend_Array;

   procedure Double_Ended_Array (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      Integer_Array : Array_Type;

   begin
      Create
        (Array_Obj => Integer_Array,
         Space     => 0,
         Growth    => Both,
         First     => 10);

      Add_First (Integer_Array, -101);
      Add_Last (Integer_Array, 101);
      Check
        ("Adding First, Last",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 2,
         First         =>  9,
         Last          => 10,
         Base_First    =>  9,
         Base_Last     => 10,
         Items         => (9 => -101, 10 => 101));

      Set_Grow (Integer_Array, 8, -102);
      Set_Grow (Integer_Array, 11, 102);
      Check
        ("Set_Grow",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 8,
         First         =>  8,
         Last          => 11,
         Base_First    =>  7,
         Base_Last     => 14,
         Items         => (8 => -102, 9 => -101, 10 => 101, 11 => 102));

      for I in 0 .. First (Integer_Array) - 1 loop
         Set_Grow (Integer_Array, I, -110 + I);
      end loop;
      Check
        ("Set_Grow a lot",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 16,
         First         =>  0,
         Last          => 11,
         Base_First    =>  -1,
         Base_Last     => 14,
         Items         =>
           (0         => -110,
            1          => -109,
            2          => -108,
            3          => -107,
            4          => -106,
            5          => -105,
            6          => -104,
            7          => -103,
            8          => -102,
            9          => -101,
            10         => 101,
            11         => 102));
      for I in 0 .. 9 loop
         Delete_Last (Integer_Array);
      end loop;
      Check
        ("Delete_Last a lot",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 16,
         First         =>  0,
         Last          => 1,
         Base_First    =>  -1,
         Base_Last     => 6,
         Items         =>
           (0          => -110,
            1          => -109));

      for I in Last (Integer_Array) + 1 .. Last (Integer_Array) + 10 loop
         Add_Last (Integer_Array, Get (Integer_Array, I - 1) + 1);
      end loop;
      Check
        ("Add_Last a lot",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 16,
         First         =>  0,
         Last          => 11,
         Base_First    =>  -1,
         Base_Last     => 14,
         Items         =>
           (0          => -110,
            1          => -109,
            2          => -108,
            3          => -107,
            4          => -106,
            5          => -105,
            6          => -104,
            7          => -103,
            8          => -102,
            9          => -101,
            10         => -100,
            11         => -99));
      for I in 0 .. 9 loop
         Delete_First (Integer_Array);
      end loop;
      Check
        ("Delete_First a lot",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 16,
         First         => 10,
         Last          => 11,
         Base_First    =>  7,
         Base_Last     => 14,
         Items         =>
           (10         => -100,
            11         => -99));

      --  Let block exit finalize Integer_Array

   end Double_Ended_Array;

   procedure Test_Insert_Before (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Integer_Array : Array_Type;

   begin
      Create
        (Array_Obj => Integer_Array,
         Space     => 0,
         Growth    => Prepend,
         First     => 0);

      begin
         Insert_Before
           (Integer_Array,
            Before => 3,
            Item   => 10);
         Assert (False, "oops, did NOT raise Constraint_Error (growth = prepend)");
      exception
      when Constraint_Error =>
         null;
      end;

      Create
        (Array_Obj => Integer_Array,
         Space     => 0,
         Growth    => Append,
         First     => 0);

      begin
         Insert_Before
           (Integer_Array,
            Before => 3,
            Item   => 10);
         Assert (False, "oops, did NOT raise Constraint_Error (empty)");
      exception
      when Constraint_Error =>
         null;
      end;

      Add_Last (Integer_Array, 1);
      Add_Last (Integer_Array, 2);
      Add_Last (Integer_Array, 3);
      Add_Last (Integer_Array, 4);

      Insert_Before
        (Integer_Array,
         Before => 2,
         Item   => 10);

      Check
        ("10",
         Integer_Array,
         Growth        => Append,
         Initial_Space => 0,
         Max_Space     => 8,
         First         => 0,
         Last          => 4,
         Base_First    => 0,
         Base_Last     => 7,
         Items         => (0 => 1, 1 => 2, 2 => 10, 3 => 3, 4 => 4));

      Finalize (Integer_Array);

      --  Test insert_before when there is only one item in the array
      Initialize (Integer_Array);
      Add_Last (Integer_Array, 2);
      Check
        ("1 a",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 1,
         First         => 1,
         Last          => 1,
         Base_First    => 1,
         Base_Last     => 1,
         Items         => (1 => 2));

      Insert_Before
        (Integer_Array,
         Before => 1, -- index of '2'
         Item   => 1);

      Check
        ("1 b",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 2,
         First         => 1,
         Last          => 2,
         Base_First    => 1,
         Base_Last     => 2,
         Items         => (1 => 1, 2 => 2));

      Insert_Before
        (Integer_Array,
         Before => 2, -- index of '2'
         Item   => 3,
         Copies => 2);

      Check
        ("1 b",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 4,
         First         => 1,
         Last          => 4,
         Base_First    => 1,
         Base_Last     => 4,
         Items         => (1 => 1, 2 => 3, 3 => 3, 4 => 2));

      Insert_Before
        (Integer_Array,
         Before => 1, -- first
         Item   => 4,
         Copies => 2);

      Check
        ("1 b",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 8,
         First         => 1,
         Last          => 6,
         Base_First    => 1,
         Base_Last     => 8,
         Items         => (1 => 4, 2 => 4, 3 => 1, 4 => 3, 5 => 3, 6 => 2));

   end Test_Insert_Before;

   procedure Test_Delete (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Integer_Array : Array_Type;

   begin
      begin
         Delete
           (Integer_Array,
            Index => 3);
         Assert (False, "oops, did NOT raise Constraint_Error 1");
      exception
      when Constraint_Error =>
         null;
      end;

      Add_Last (Integer_Array, 1);
      Add_Last (Integer_Array, 2);
      Add_Last (Integer_Array, 3);
      Add_Last (Integer_Array, 4);

      Delete
        (Integer_Array,
         Index => 2);

      Check
        ("1",
         Integer_Array,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 4,
         First         => 1,
         Last          => 3,
         Base_First    => 1,
         Base_Last     => 4,
         Items         => (1 => 1, 2 => 3, 3 => 4));

   end Test_Delete;

   procedure Test_Iterators (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use SAL.AUnit;

      Container : Container_Type := new Array_Type;
      I         : Iterator_Type (Container);

   begin
      Add_Last (Container.all, 1);
      Add_Last (Container.all, 2);
      Add_Last (Container.all, 3);
      Add_Last (Container.all, 4);
      Add_Last (Container.all, 5);

      Check ("first", Index (First (Container)), 1);
      Check ("last", Index (Last (Container)), 5);
      Check ("none", Is_Null (None (Container)), True);
      Check
        ("middle",
         Index
           (Middle
              (Next (Next (First (Container))),
               Last (Container)
              )
           ), 4);

      Check ("middle", Index (Middle (First (Container), Last (Container))), 3);

      Check ("null", Is_Null (I), True);

      I := First (Container);

      Check ("current", Current (I), 1);

      Check ("next 1", Index (Next (I)), 2);

      Next (I);

      Check ("next 2", Index (I), 2);

      Check ("prev 1", Index (Prev (I)), 1);

      Prev (I);

      Check ("prev 2", Index (I), 1);

      Delete (Container, I);

      Check ("i post delete", Index (I), 1);

      Check
        ("delete",
         Container.all,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 8,
         First         => 1,
         Last          => 4,
         Base_First    => 1,
         Base_Last     => 8,
         Items         => (1 => 2, 2 => 3, 3 => 4, 4 => 5));

      Insert_Before (Container, I, 6, Copies => 2);

      Check
        ("insert_before",
         Container.all,
         Growth        => Both,
         Initial_Space => 0,
         Max_Space     => 8,
         First         => 1,
         Last          => 6,
         Base_First    => 1,
         Base_Last     => 8,
         Items         => (1 => 6, 2 => 6, 3 => 2, 4 => 3, 5 => 4, 6 => 5));

      Test_Poly_Unbounded_Arrays_Aux.Integers.Free (Container);
   end Test_Iterators;

   ----------
   --  Public subprograms

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Prepend_Array'Access, "Prepend_Array");
      Register_Routine (T, Double_Ended_Array'Access, "Double_Ended_Array");
      Register_Routine (T, Test_Insert_Before'Access, "Test_Insert_Before");
      Register_Routine (T, Test_Delete'Access, "Test_Delete");
      Register_Routine (T, Test_Iterators'Access, "Test_Iterators");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Poly_Unbouned_Arrays");
   end Name;

   procedure Set_Up_Case (T : in out Test_Case)
   is begin
      SAL.AUnit.Check ("Allocated_Elements", Test_Storage_Pools.Allocated_Elements (Array_Storage_Pool), 0);
      Test_Storage_Pools.Reset_Counts (Array_Storage_Pool);
      Test_Storage_Pools.Set_Debug (Array_Storage_Pool, T.Debug_Level > 0);
   end Set_Up_Case;

   procedure Tear_Down (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run after each routine
      SAL.AUnit.Check ("Allocated_Elements", Test_Storage_Pools.Allocated_Elements (Array_Storage_Pool), 0);
   end Tear_Down;

end Test_Poly_Unbounded_Arrays;
