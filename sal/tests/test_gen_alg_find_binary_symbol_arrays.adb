--  Abstract :
--
--  Test SAL.Gen.Alg.Find_Binary indefinite non-limited items
--  (symbols).
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
with SAL.Gen.Alg.Find_Binary;
with Test_Gen_Alg_Aux.Symbols; use Test_Gen_Alg_Aux.Symbols;
procedure Test_Gen_Alg_Find_Binary_Symbol_Arrays
is
   package Algorithms_Find_Binary is new Array_Algorithms.Find_Binary
      (Key_Type                  => Symbol_Type'Class,
       Is_Less_Equal_Node_Key    => Is_Less_Equal_Node_Class,
       Is_Equal_Node_Key         => Is_Equal_Node_Class,
       Is_Greater_Equal_Node_Key => Is_Greater_Equal_Node_Class,
       Middle                    => Symbol_Array_Iterators.Middle,
       Prev                      => Symbol_Array_Iterators.Prev);

   use Symbol_Array_Iterators;

   --  These can't be 'constant' because Symbol_Access_Type isn't.
   Float_5    : aliased Floating_Point_Type := (Significant_Digits                     => 5);
   Discrete_4 : aliased Symbol_Type'Class   := Discrete_Number_Type'(First             => -4, Last => +4);

   pragma Warnings (Off); -- Discrete_3 is not modified, but we can't make it constant either.
   Discrete_3 : aliased Symbol_Type'Class   := Discrete_Number_Type'(First             => -3, Last => +3);
   pragma Warnings (On);

   Float_0    : aliased Symbol_Type'Class   := Floating_Point_Type'(Significant_Digits => 0);

   List : aliased Symbol_Array_Type :=
      (1 => Discrete_4'Unchecked_Access,
       2 => Float_0'Unchecked_Access,
       3 => Float_5'Unchecked_Access);
   --  Need explicit initialization, rather than constrained bounds,
   --  to allow 'access to match an access to unconstrained array
   --  type.

   Container : constant Symbol_Container_Type := List'Unchecked_Access;
   --  Unchecked because List is not at library level.

   Iterator : Symbol_Array_Iterators.Iterator_Type := Symbol_Array_Iterators.None (Container);
begin
   Put_Line ("SAL.Gen.Alg.Find_Binary, SAL.Aux.Enum_Iterators.");
   Put_Line ("Indefinite non-limited items (symbols)");

   Print_Array (Container);

   Put_Line ("Finding");
   Print (Float_5);
   Put (" find less equal => ");
   Print_Symbol (Current (Algorithms_Find_Binary.Find_Less_Equal (Container, Float_5)));
   New_Line;

   Print (Discrete_4);
   Put (" find greater equal => ");
   Print_Symbol (Current (Algorithms_Find_Binary.Find_Greater_Equal (Container, Discrete_4)));
   New_Line;

   Iterator := Algorithms_Find_Binary.Find_Less_Equal (Container, Discrete_3);
   Print (Discrete_3);
   Put (" find less equal => ");
   if Is_Null (Iterator) then
      Put_Line ("Insert at head");
   else
      Put ("Insert_After "); Print_Symbol (Current (Iterator));
   end if;

end Test_Gen_Alg_Find_Binary_Symbol_Arrays;
