--  Abstract :
--
--  Test SAL.Gen.Alg.Find_Binary indefinite tagged limited items
--  (puppets).
--
--  Copyright (C) 2000, 2003 Stephen Leake.  All Rights Reserved.
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
with SAL.Gen.Alg.Find_Binary;
with Test_Gen_Alg_Aux.Puppets; use Test_Gen_Alg_Aux.Puppets;
procedure Test_Gen_Alg_Find_Binary_Puppet_Arrays
is
   package Algorithms_Find_Binary is new Array_Algorithms.Find_Binary
      (Key_Type                  => Parameters_Type,
       Is_Less_Equal_Node_Key    => Is_Less_Equal_Param,
       Is_Equal_Node_Key         => Is_Equal_Puppet,
       Is_Greater_Equal_Node_Key => Is_Greater_Equal_Param,
       Middle                    => Puppet_Array_Iterators.Middle,
       Prev                      => Puppet_Array_Iterators.Prev);

   use Puppet_Array_Iterators;

   Muppet_2_5 : constant Parameters_Type := (Muppet, 2, 5);
   Muppet_3_4 : constant Parameters_Type := (Muppet, 3, 4);
   Beanie_4   : constant Parameters_Type := (Beanie, 4);
   Beanie_0   : constant Parameters_Type := (Beanie, 0);

   List : aliased Puppet_Array_Type :=
      (1 => Allocate (Muppet_2_5),
       2 => Allocate (Beanie_0),
       3 => Allocate (Beanie_4));
   --  Need explicit initialization, rather than constrained bounds,
   --  to allow 'access to match an access to unchecked array type.

   Container : constant Puppet_Container_Type := List'Unchecked_Access;
   --  unchecked because List is not at library level.

   Iterator : Puppet_Array_Iterators.Iterator_Type := Puppet_Array_Iterators.None (Container);
begin
   Put_Line ("SAL.Gen.Alg.Find_Binary, SAL.Aux.Enum_Iterators.");
   Put_Line ("Indefinite tagged limited items (puppets)");

   Print_Array (Container);

   Put_Line ("Finding");
   Print (Muppet_2_5);
   Put (" find less equal => ");
   Print_Puppet (Current (Algorithms_Find_Binary.Find_Less_Equal (Container, Muppet_2_5)));
   New_Line;

   Print (Beanie_0);
   Put (" find greater equal => ");
   Print_Puppet (Current (Algorithms_Find_Binary.Find_Greater_Equal (Container, Beanie_0)));
   New_Line;

   Iterator := Algorithms_Find_Binary.Find_Less_Equal (Container, Muppet_3_4);
   Print (Muppet_3_4);
   Put (" find less equal => ");
   if Is_Null (Iterator) then
      Put_Line ("Insert at head");
   else
      Put ("Insert_After "); Print_Puppet (Current (Iterator));
   end if;

   --  We would use a Controlled container in a real application, but
   --  this just shows we can use plain array containers if we want
   --  to.
   for I in List'Range loop
      Free_Puppet (List (I));
   end loop;
end Test_Gen_Alg_Find_Binary_Puppet_Arrays;
