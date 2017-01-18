--  Abstract
--
--  Declarations to help test SAL.Gen.Alg.*, with an indefinite tagged
--  non-limited type.
--
--  All SAL containers that are compatible with SAL.Gen.Alg are
--  instantiated here. This ensures that they stay compatible as
--  SAL.Gen.Alg changes.
--
--  The actual algorithms are instantiated in the various test
--  separate procedures test_gen_alg_*-symbols.adb. That way, the
--  output files don't change when a new algorithm is added; only when
--  a new container is added.
--
--  Copyright (C) 2000, 2002, 2003, 2007 Stephen Leake.  All Rights Reserved.
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

with Ada.Tags;
with Ada.Unchecked_Deallocation;
with System.Storage_Pools;
with SAL.Aux.Enum_Iterators;
with SAL.Aux.Indefinite_Private_Items;
with SAL.Gen.Alg.Count;
with SAL.Gen.Alg.Process_All_Constant;
with SAL.Poly.Lists.Double;
with Test_Storage_Pools;
package Test_Gen_Alg_Aux.Symbols is
   pragma Elaborate_Body; -- Body depends on Ada.Text_IO.

   type Symbol_Type is abstract tagged null record;

   type Symbol_Access_Type is access all Symbol_Type'Class;
   --  Symbol_Access_Type can't be 'access constant' because the
   --  containers require access to variable.

   Storage_Pool_Name : aliased constant String := "Symbol_Storage_Pool";
   Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Storage_Pool_Name'Access);
   for Symbol_Access_Type'Storage_Pool use Storage_Pool;

   procedure Print (Item : in Symbol_Type) is abstract;

   function Is_Equal (Left, Right : in Symbol_Type) return Boolean is abstract;
   function Is_Greater_Equal (Left, Right : in Symbol_Type) return Boolean is abstract;
   function Is_Less_Equal (Left, Right : in Symbol_Type) return Boolean is abstract;

   ------------
   --  Container support

   package Container_Aux is new SAL.Aux.Indefinite_Private_Items
      (Item_Type        => Symbol_Type'Class,
       Item_Access_Type => Symbol_Access_Type);

   procedure Free_Symbol is new Ada.Unchecked_Deallocation (Symbol_Type'Class, Symbol_Access_Type);

   -------------
   --  Operations for all algorithms

   procedure Print_Symbol
      (Item  : in Symbol_Access_Type;
       First : in Boolean := True);

   --  For Find_Linear.Is_Equal (Left : in Item_Node_Type; Right : in Key_Type) return Boolean;
   function Is_Equal_Node_Class (Left : in Symbol_Access_Type; Right : in Symbol_Type'Class) return Boolean;
   function Is_Equal_Node_Tag (Left : in Symbol_Access_Type; Right : in Ada.Tags.Tag) return Boolean;

   --  For Find_Linear.Sorted.Is_Equal_*
   function Is_Equal_Node (Left, Right : in Symbol_Access_Type) return Boolean;

   --  For Find_Linear.Sorted.Is_Greater_Equal_*
   function Is_Greater_Equal_Node_Class (Left : in Symbol_Access_Type; Right : in Symbol_Type'Class) return Boolean;
   function Is_Greater_Equal_Node (Left, Right : in Symbol_Access_Type) return Boolean;

   --  For Find_Binary.Is_Less_Equal_*
   function Is_Less_Equal_Node_Class (Left : in Symbol_Access_Type; Right : in Symbol_Type'Class) return Boolean;

   --------------
   --  List container and algorithms

   package Lists is new SAL.Poly.Lists.Double
      (Item_Type         => Symbol_Type'Class,
       Item_Node_Type    => Symbol_Access_Type,
       To_Item_Node      => Container_Aux.New_Item,
       Free_Item         => Free_Symbol,
       Copy              => Container_Aux.Copy,
       Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));
   use Lists;

   package Algorithms is new SAL.Gen.Alg
      (Item_Node_Type => Symbol_Access_Type,
       Container_Type => Lists.List_Type,
       Iterator_Type  => Lists.Iterator_Type,
       Next_Function  => Lists.Next,
       Next_Procedure => Lists.Next);

   function Count is new Algorithms.Count;

   procedure Print_Header (List : in Lists.List_Type);
   procedure Print_Trailer (List : in Lists.List_Type);

   procedure Print_List is new Algorithms.Process_All_Constant
      (Pre_Process_Container  => Print_Header,
       Process_Item           => Print_Symbol,
       Post_Process_Container => Print_Trailer);

   -----------
   --  Array container and algorithms

   type Symbol_Array_Type is array (Integer range <>) of Symbol_Access_Type;
   type Symbol_Container_Type is access constant Symbol_Array_Type;

   package Symbol_Array_Iterators is new SAL.Aux.Enum_Iterators
      (Item_Node_Type => Symbol_Access_Type,
       Index_Type     => Integer,
       Array_Type     => Symbol_Array_Type,
       Container_Type => Symbol_Container_Type);

   package Array_Algorithms is new SAL.Gen.Alg
      (Item_Node_Type => Symbol_Access_Type,
       Container_Type => Symbol_Container_Type,
       Iterator_Type  => Symbol_Array_Iterators.Iterator_Type,
       Current        => Symbol_Array_Iterators.Current,
       First          => Symbol_Array_Iterators.First,
       Last           => Symbol_Array_Iterators.Last,
       None           => Symbol_Array_Iterators.None,
       Is_Null        => Symbol_Array_Iterators.Is_Null,
       Next_Procedure => Symbol_Array_Iterators.Next,
       Next_Function  => Symbol_Array_Iterators.Next);

   function Count is new Array_Algorithms.Count;

   procedure Print_Header (List : in Symbol_Container_Type);
   procedure Print_Trailer (List : in Symbol_Container_Type);

   procedure Print_Array is new Array_Algorithms.Process_All_Constant
      (Pre_Process_Container  => Print_Header,
       Process_Item           => Print_Symbol,
       Post_Process_Container => Print_Trailer);

   ------------
   --  Floating_Point derived item type

   type Floating_Point_Type is new Symbol_Type with record
      Significant_Digits : Natural;
   end record;

   procedure Print (Item : in Floating_Point_Type);

   function Is_Equal (Left, Right : in Floating_Point_Type) return Boolean;
   function Is_Greater_Equal (Left, Right : in Floating_Point_Type) return Boolean;
   function Is_Less_Equal (Left, Right : in Floating_Point_Type) return Boolean;

   ------------
   --  Discrete derived item type

   type Discrete_Number_Type is new Symbol_Type with record
      First : Integer;
      Last : Integer;
   end record;

   procedure Print (Item : in Discrete_Number_Type);

   function Is_Equal (Left, Right : in Discrete_Number_Type) return Boolean;
   function Is_Greater_Equal (Left, Right : in Discrete_Number_Type) return Boolean;
   function Is_Less_Equal (Left, Right : in Discrete_Number_Type) return Boolean;

end Test_Gen_Alg_Aux.Symbols;
