--  Abstract
--
--  Declarations to help test SAL.Gen.Alg.*, with an indefinite tagged
--  limited item type.
--
--  All SAL containers that are compatible with SAL.Gen.Alg are
--  instantiated here. This ensures that they stay compatible as
--  SAL.Gen.Alg changes.
--
--  The actual algorithms are instantiated in the various test
--  procedures test_gen_alg_*-puppets.adb. That way, the output files
--  don't change when a new algorithm is added; only when a new
--  container is added.
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

with Ada.Unchecked_Deallocation;
with System.Storage_Pools;
with SAL.Aux.Enum_Iterators;
with SAL.Aux.Indefinite_Limited_Items;
with SAL.Gen.Alg.Count;
with SAL.Gen.Alg.Process_All_Constant;
with SAL.Poly.Lists.Double;
with Test_Storage_Pools;
package Test_Gen_Alg_Aux.Puppets is
   pragma Elaborate_Body; -- Body depends on Ada.Text_IO.

   type Label_Type is (Muppet, Beanie);

   type Parameters_Type (Label : Label_Type) is record
      case Label is
      when Muppet =>
         Arms    : Integer;
         Fingers : Integer;
      when Beanie =>
         Legs : Integer;
      end case;
   end record;

   procedure Print (Item : in Parameters_Type);

   --  Base type
   type Puppet_Type is abstract tagged limited null record;
   type Puppet_Access_Type is access all Puppet_Type'Class;
   type Puppet_Access_Constant_Type is access constant Puppet_Type'Class;
   --  Need 'all' so we can convert from Muppet_Access_Type in Allocate.

   Storage_Pool_Name : aliased constant String := "Puppet_Storage_Pool";
   Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Storage_Pool_Name'Access);
   for Puppet_Access_Type'Storage_Pool use Storage_Pool;

   procedure Print (Item : in Puppet_Type) is abstract;

   procedure Initialize (Item : in out Puppet_Type; Parameters : in Parameters_Type) is abstract;

   function Is_Equal (Left, Right : in Puppet_Type) return Boolean is abstract;
   function Is_Equal (Left : in Puppet_Type; Right : in Parameters_Type) return Boolean is abstract;
   function Is_Equal (Left : in Puppet_Type; Right : in Label_Type) return Boolean is abstract;
   function Is_Greater_Equal (Left : in Puppet_Type; Right : in Parameters_Type) return Boolean is abstract;
   function Is_Greater_Equal (Left, Right : in Puppet_Type) return Boolean is abstract;
   function Is_Less_Equal (Left : in Puppet_Type; Right : in Parameters_Type) return Boolean is abstract;

   ------------
   --  Container support

   function Allocate (Parameters : in Parameters_Type) return Puppet_Access_Type;

   procedure Init_Class (Item : in out Puppet_Type'class; Parameters : in Parameters_Type);
   --  Dispatches to appropriate Initialize

   package Container_Aux is new SAL.Aux.Indefinite_Limited_Items
      (Create_Parameters_Type => Parameters_Type,
       Limited_Type           => Puppet_Type'Class,
       Item_Access_Type       => Puppet_Access_Type,
       Allocate_Item          => Allocate,
       Initialize_Item        => Init_Class);

   procedure Free_Puppet is new Ada.Unchecked_Deallocation (Puppet_Type'Class, Puppet_Access_Type);

   -----------
   --  Operations for all algorithms

   procedure Print_Puppet (Item  : in Puppet_Access_Type; First : in Boolean := True);

   --  for Find_Linear.Is_Equal (Left : in Item_Node_Type; Right : in Key_Type) return Boolean;
   function Is_Equal_Puppet (Left : in Puppet_Access_Type; Right : in Parameters_Type) return Boolean;
   function Is_Equal_Label (Left : in Puppet_Access_Type; Right : in Label_Type) return Boolean;

   --  for Find_Linear.Sorted.Is_Equal_*
   function Is_Equal_Node (Left, Right : in Puppet_Access_Type) return Boolean;

   --  for Find_Linear.Sorted.Is_Greater_Equal_*
   function Is_Greater_Equal_Param (Left : in Puppet_Access_Type; Right : in Parameters_Type) return Boolean;
   function Is_Greater_Equal_Node (Left, Right : in Puppet_Access_Type) return Boolean;

   --  for Find_Binary.Is_Less_Equal_Node_Key
   function Is_Less_Equal_Param (Left : in Puppet_Access_Type; Right : in Parameters_Type) return Boolean;

   --------------
   --  List container and algorithms

   package Lists is new SAL.Poly.Lists.Double
      (Item_Type         => Parameters_Type,
       Item_Node_Type    => Puppet_Access_Type,
       To_Item_Node      => Container_Aux.New_Item,
       Free_Item         => Free_Puppet,
       Copy              => Container_Aux.No_Copy,
       Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));

   package List_Algorithms is new SAL.Gen.Alg
      (Item_Node_Type => Puppet_Access_Type,
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

   procedure Print_Header (List : in Lists.List_Type);
   procedure Print_Trailer (List : in Lists.List_Type);

   procedure Print_List is new List_Algorithms.Process_All_Constant
      (Pre_Process_Container  => Print_Header,
       Process_Item           => Print_Puppet,
       Post_Process_Container => Print_Trailer);

   -----------
   --  Array container and algorithms

   type Puppet_Array_Type is array (Integer range <>) of Puppet_Access_Type;
   type Puppet_Container_Type is access constant Puppet_Array_Type;

   package Puppet_Array_Iterators is new SAL.Aux.Enum_Iterators
      (Item_Node_Type => Puppet_Access_Type,
       Index_Type     => Integer,
       Array_Type     => Puppet_Array_Type,
       Container_Type => Puppet_Container_Type);

   package Array_Algorithms is new SAL.Gen.Alg
      (Item_Node_Type => Puppet_Access_Type,
       Container_Type => Puppet_Container_Type,
       Iterator_Type  => Puppet_Array_Iterators.Iterator_Type,
       Current        => Puppet_Array_Iterators.Current,
       First          => Puppet_Array_Iterators.First,
       Last           => Puppet_Array_Iterators.Last,
       None           => Puppet_Array_Iterators.None,
       Is_Null        => Puppet_Array_Iterators.Is_Null,
       Next_Procedure => Puppet_Array_Iterators.Next,
       Next_Function  => Puppet_Array_Iterators.Next);

   function Count is new Array_Algorithms.Count;

   procedure Print_Header (List : in Puppet_Container_Type);
   procedure Print_Trailer (List : in Puppet_Container_Type);

   procedure Print_Array is new Array_Algorithms.Process_All_Constant
      (Pre_Process_Container  => Print_Header,
       Process_Item           => Print_Puppet,
       Post_Process_Container => Print_Trailer);

   -----------
   --  Muppet derived type

   type Muppet_Type (Arms : Integer) is new Puppet_Type with record
      Fingers : Integer;
   end record;

   procedure Print (Item : in Muppet_Type);

   procedure Initialize (Item : in out Muppet_Type; Parameters : in Parameters_Type);

   function Is_Equal (Left, Right : in Muppet_Type) return Boolean;
   function Is_Equal (Left : in Muppet_Type; Right : in Parameters_Type) return Boolean;
   function Is_Equal (Left : in Muppet_Type; Right : in Label_Type) return Boolean;
   function Is_Greater_Equal (Left : in Muppet_Type; Right : in Parameters_Type) return Boolean;
   function Is_Greater_Equal (Left, Right : in Muppet_Type) return Boolean;
   function Is_Less_Equal (Left : in Muppet_Type; Right : in Parameters_Type) return Boolean;

   -----------
   --  Beanie derived type

   type Beanie_Type is new Puppet_Type with record
      Legs : Integer;
   end record;

   procedure Print (Item : in Beanie_Type);

   procedure Initialize (Item : in out Beanie_Type; Parameters : in Parameters_Type);

   function Is_Equal (Left, Right : in Beanie_Type) return Boolean;
   function Is_Equal (Left : in Beanie_Type; Right : in Parameters_Type) return Boolean;
   function Is_Equal (Left : in Beanie_Type; Right : in Label_Type) return Boolean;
   function Is_Greater_Equal (Left : in Beanie_Type; Right : in Parameters_Type) return Boolean;
   function Is_Greater_Equal (Left, Right : in Beanie_Type) return Boolean;
   function Is_Less_Equal (Left : in Beanie_Type; Right : in Parameters_Type) return Boolean;

end Test_Gen_Alg_Aux.Puppets;
