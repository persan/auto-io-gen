--  Abstract
--
--  Declarations to help test SAL.Gen.Alg.*, with a definite
--  non-limited item type.
--
--  All SAL containers that are compatible with SAL.Gen.Alg are
--  instantiated here, with Integer Item_Types. This ensures
--  that they stay compatible as SAL.Gen.Alg changes.
--
--  The actual algorithms are instantiated in the various test
--  procedures test_gen_alg_*.adb. That way, the output files don't
--  change when a new algorithm is added; only when a new container is
--  added.
--
--  Copyright (C) 2000, 2002, 2003 Stephen Leake.  All Rights Reserved.
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

with System.Storage_Pools;
with SAL.Aux.Definite_Private_Items;
with SAL.Aux.Enum_Iterators;
with SAL.Gen.Alg.Count;
with SAL.Gen.Alg.Process_All_Constant;
with SAL.Poly.Lists.Double;
package Test_Gen_Alg_Aux.Integers is
   pragma Elaborate_Body; -- Body depends on Ada.Text_IO.

   -------------
   --  Double linked list container

   package Lists_Aux is new SAL.Aux.Definite_Private_Items (Integer);
   package Lists is new SAL.Poly.Lists.Double
      (Item_Type         => Integer,
       Item_Node_Type    => Integer,
       To_Item_Node      => Lists_Aux.To_Item_Node,
       Free_Item         => Lists_Aux.Free_Item,
       Copy              => Lists_Aux.Copy,
       Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));

   package List_Algorithms is new SAL.Gen.Alg
      (Item_Node_Type => Integer,
       Container_Type => Lists.List_Type,
       Iterator_Type  => Lists.Iterator_Type,
       Current        => Lists.Current,
       First          => Lists.First,
       Last           => Lists.Last,
       None           => Lists.None,
       Is_Null        => Lists.Is_Null,
       Next_Procedure => Lists.Next,
       Next_Function  => Lists.Next);

   function Count is new List_Algorithms.Count;

   procedure Print_Integer
      (Item  : in Integer;
       First : in Boolean);

   procedure Print_Header (List : in Lists.List_Type);
   procedure Print_Trailer (List : in Lists.List_Type);

   procedure Print_List is new List_Algorithms.Process_All_Constant
      (Pre_Process_Container  => Print_Header,
       Process_Item           => Print_Integer,
       Post_Process_Container => Print_Trailer);

   --------------
   --  Unconstrained array container

   type Integer_Array_Type is array (Integer range <>) of Integer;
   type Integer_Container_Type is access constant Integer_Array_Type;

   package Integer_Array_Iterators is new SAL.Aux.Enum_Iterators
      (Item_Node_Type => Integer,
       Index_Type     => Integer,
       Array_Type     => Integer_Array_Type,
       Container_Type => Integer_Container_Type);

   package Array_Algorithms is new SAL.Gen.Alg
      (Item_Node_Type => Integer,
       Container_Type => Integer_Container_Type,
       Iterator_Type  => Integer_Array_Iterators.Iterator_Type,
       Current        => Integer_Array_Iterators.Current,
       First          => Integer_Array_Iterators.First,
       Last           => Integer_Array_Iterators.Last,
       None           => Integer_Array_Iterators.None,
       Is_Null        => Integer_Array_Iterators.Is_Null,
       Next_Procedure => Integer_Array_Iterators.Next,
       Next_Function  => Integer_Array_Iterators.Next);

   function Count is new Array_Algorithms.Count;

   procedure Print_Header (List : in Integer_Container_Type);
   procedure Print_Trailer (List : in Integer_Container_Type);

   procedure Print_Array is new Array_Algorithms.Process_All_Constant
      (Pre_Process_Container  => Print_Header,
       Process_Item           => Print_Integer,
       Post_Process_Container => Print_Trailer);

end Test_Gen_Alg_Aux.Integers;
