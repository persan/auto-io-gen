--  Abstract :
--
--  Test SAL.Gen.Alg.Find_Binary, with definite non-tagged non-limited
--  items.
--
--  Copyright (C) 2000, 2003 Stephen Leake.  All Rights Reserved.
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
with SAL.Gen.Alg.Find_Binary;
with Test_Gen_Alg_Aux.Integers; use Test_Gen_Alg_Aux.Integers;
procedure Test_Gen_Alg_Find_Binary_Integer_Arrays
is
   package Algorithms_Find_Binary is new Array_Algorithms.Find_Binary
      (Key_Type                  => Integer,
       Is_Less_Equal_Node_Key    => "<=",
       Is_Equal_Node_Key         => "=",
       Is_Greater_Equal_Node_Key => ">=",
       Middle                    => Integer_Array_Iterators.Middle,
       Prev                      => Integer_Array_Iterators.Prev);

   use Integer_Array_Iterators;

   List : aliased Integer_Array_Type :=
      (1 => 1,
       2 => 3,
       3 => 4,
       4 => 5,
       5 => 8,
       6 => 10);
   --  Need explicit initialization, rather than constrained bounds,
   --  to allow 'access to match an access to unchecked array type.

   Container : constant Integer_Container_Type := List'Unchecked_Access;
   --  unchecked because List is not at library level.
begin
   Put_Line ("SAL.Gen.Alg.Find_Binary, SAL.Aux.Enum_Iterators.");
   Put_Line ("Definite non-tagged non-limited items (integer_arrays)");

   Print_Array (Container);

   Put_Line ("Finding");
   Put_Line ("5 => " & Integer'Image (Current (Algorithms_Find_Binary.Find_Less_Equal (Container, 5))));
   Put_Line ("3 => " & Integer'Image (Current (Algorithms_Find_Binary.Find_Greater_Equal (Container, 3))));
   if Is_Null (Algorithms_Find_Binary.Find_Less_Equal (Container, 0)) then
      Put_Line ("ok, didn't find 0");
   else
      Put_Line ("Oops, found 0");
   end if;

end Test_Gen_Alg_Find_Binary_Integer_Arrays;
