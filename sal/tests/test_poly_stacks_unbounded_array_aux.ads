--  Abstract
--
--  Declarations to help test SAL.Poly.Stacks.Unbounded_Arrays.
--
--  Copyright (C) 1999, 2000, 2002 Stephen Leake.  All Rights Reserved.
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

with SAL.Poly.Stacks.Test;
with SAL.Poly.Stacks.Unbounded_Array;
with Test_Storage_Pools;
with System.Storage_Pools;
package Test_Poly_Stacks_Unbounded_Array_Aux is
--   pragma Elaborate_Body; -- Test_Storage_Pools is, but we have no body

   Integer_Storage_Pool_Name : aliased constant String := "Integer_Storage_Pool";
   Integer_Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Integer_Storage_Pool_Name'Access);

   package Integer_Stacks is new SAL.Poly.Stacks (Integer);
   package Integer_Stacks_Test is new Integer_Stacks.Test (Integer'Image);
   package Unbounded_Integer_Stacks is new Integer_Stacks.Unbounded_Array
      (Initial_Stack_Size => 0,
       Array_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Integer_Storage_Pool));

end Test_Poly_Stacks_Unbounded_Array_Aux;

