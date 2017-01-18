--  Abstract
--
--  Declarations to help test SAL.Poly.Binary_Trees.Sorted.
--
--  Copyright (C) 1997, 1998, 2001, 2002 Stephen Leake.  All Rights Reserved.
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

with System.Storage_Pools;
with SAL.Aux.Indefinite_Private_Items.Comparable;
with SAL.Poly.Binary_Trees.Sorted.Test;
with SAL.Poly.Binary_Trees.Sorted.Iterators;
with Test_Storage_Pools;
package Test_Poly_Binary_Trees_Sorted_Aux is
   pragma Elaborate_Body; -- Body depends on Ada.Text_IO.

   String_Storage_Pool_Name : aliased constant String := "String_Storage_Pool";
   String_Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, String_Storage_Pool_Name'Access);

   Node_Storage_Pool_Name : aliased constant String := "Node_Storage_Pool";
   Node_Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Node_Storage_Pool_Name'Access);

   Iterator_Stack_Storage_Pool_Name : aliased constant String := "Iterator_Stack_Storage_Pool";
   Iterator_Stack_Storage_Pool : Test_Storage_Pools.Storage_Pool_Type
      (1000, Iterator_Stack_Storage_Pool_Name'Access);

   ------------
   --  Indefinite, non-tagged, non-limited Item and Key types

   type String_Access_Type is access String;
   for String_Access_Type'Storage_Pool use String_Storage_Pool;

   package String_Items_Aux is new SAL.Aux.Indefinite_Private_Items
      (Item_Type                 => String,
       Item_Access_Type          => String_Access_Type);
   package String_Items_Comparable_Aux is new String_Items_Aux.Comparable;

   package String_Trees is new SAL.Poly.Binary_Trees.Sorted
      (Item_Type      => String,
       Item_Node_Type => String_Access_Type,
       To_Item_Node   => String_Items_Aux.New_Item,
       Free_Item      => String_Items_Aux.Free_Item,
       Key_Type       => String,
       To_Key         => String_Items_Aux.To_Item,
       Is_Greater           => ">",
       Is_Equal             => "=",
       Duplicate_Key_Action => SAL.Allow,
       Node_Storage_Pool    => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));

   package String_Trees_Iterators is new String_Trees.Iterators
      (Stack_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Iterator_Stack_Storage_Pool));

   procedure Put_String (Item : in String);

   procedure Put_String_Access (Item : in String_Access_Type);

   package String_Trees_Test is new String_Trees.Test (Put_String, Put_String_Access);

end Test_Poly_Binary_Trees_Sorted_Aux;
