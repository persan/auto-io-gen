--  Abstract
--
--  Declarations to help test SAL.Poly.Unbounded_Arrays.
--
--  Copyright (C) 1999, 2000, 2002, 2003, 2007, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Unchecked_Deallocation;
with SAL.AUnit;
with SAL.Aux.Definite_Private_Items;
with SAL.Poly.Unbounded_Arrays.Test;
with System.Storage_Pools;
with Test_Storage_Pools;
package Test_Poly_Unbounded_Arrays_Aux is
   --  pragma Elaborate_Body; -- storage pools are library level controlled objects, and we have no body.

   -----------
   --  General utilities

   Integer_Storage_Pool_Name : aliased constant String := "Integer_Storage_Pool";
   Integer_Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Integer_Storage_Pool_Name'Access);

   Array_Storage_Pool_Name : aliased constant String := "Array_Storage_Pool";
   Array_Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Array_Storage_Pool_Name'Access);

   ------------
   --  Definite non-tagged non-limited item type.

   package Integers is
      package Arrays_Aux is new SAL.Aux.Definite_Private_Items (Integer);
      package Arrays is new SAL.Poly.Unbounded_Arrays
        (Index_Type         => Integer,
         Item_Type          => Integer,
         Item_Node_Type     => Integer,
         To_Item_Node       => Arrays_Aux.To_Item_Node,
         Free_Item          => Arrays_Aux.Free_Item,
         Copy_Item_Node     => Arrays_Aux.Copy,
         Array_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Array_Storage_Pool));

      package Arrays_Test is new Arrays.Test
        (Image           => Integer'Image,
         Check_Item_Node => SAL.AUnit.Check,
         Check_Index     => SAL.AUnit.Check);

      procedure Free is new Ada.Unchecked_Deallocation (Arrays.Array_Type, Arrays.Container_Type);
   end Integers;

end Test_Poly_Unbounded_Arrays_Aux;
