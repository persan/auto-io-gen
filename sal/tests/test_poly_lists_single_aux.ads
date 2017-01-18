--  Abstract
--
--  Declarations to help test SAL.Poly.Lists.Single, and algorithms
--  instantiated for it.
--
--  Copyright (C) 1999, 2000, 2002, 2003, 2005 Stephen Leake.  All Rights Reserved.
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

with Ada.Unchecked_Deallocation;
with System.Storage_Pools;

with SAL.Gen.Alg.Count;
with SAL.Gen.Alg.Process_All_Constant;
with SAL.Poly.Lists.Single.Iterators;
with SAL.Aux.Definite_Private_Items;
with SAL.Aux.Indefinite_Private_Items;
with SAL.Aux.Indefinite_Limited_Items;
with Test_Storage_Pools;
package Test_Poly_Lists_Single_Aux is
   pragma Elaborate_Body; -- Body depends on Ada.Text_IO.

   -----------
   --  General utilities

   Node_Storage_Pool_Name : aliased constant String := "Node_Storage_Pool";
   Node_Storage_Pool      : Test_Storage_Pools.Storage_Pool_Type (1000, Node_Storage_Pool_Name'Access);

   ------------
   --  Definite non-tagged non-limited item type.

   package Integers is
      package Lists_Aux is new SAL.Aux.Definite_Private_Items (Integer);
      package Lists is new SAL.Poly.Lists.Single
         (Item_Type         => Integer,
          Item_Node_Type    => Integer,
          To_Item_Node      => Lists_Aux.To_Item_Node,
          Free_Item         => Lists_Aux.Free_Item,
          Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));

      package Iterators is new Lists.Iterators;
      use Iterators;

      package Algorithms is new SAL.Gen.Alg
         (Item_Node_Type => Integer,
          Container_Type => Lists.List_Type,
          Iterator_Type  => Iterators.Iterator_Type,
          Next_Procedure => Next,
          Next_Function  => Next);

      function Count is new Algorithms.Count;

      procedure Print_Integer
         (Item  : in Integer;
          First : in Boolean);

      procedure Print_Header (List : in Lists.List_Type);
      procedure Print_Trailer (List : in Lists.List_Type);

      procedure Print_List is new Algorithms.Process_All_Constant
         (Pre_Process_Container  => Print_Header,
          Process_Item           => Print_Integer,
          Post_Process_Container => Print_Trailer);

   end Integers;

   ------------
   --  Indefinite abstract tagged non-limited type.

   package Symbols is
      type Symbol_Type is abstract tagged null record;

      type Symbol_Access_Type is access Symbol_Type'Class;

      Storage_Pool_Name : aliased constant String := "Symbol_Storage_Pool";
      Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Storage_Pool_Name'Access);
      for Symbol_Access_Type'Storage_Pool use Storage_Pool;

      procedure Print (Item : in Symbol_Type) is abstract;
      procedure Print_Symbol
         (Item  : in Symbol_Access_Type;
          First : in Boolean := True);

      package Lists_Aux is new SAL.Aux.Indefinite_Private_Items
         (Item_Type        => Symbol_Type'Class,
          Item_Access_Type => Symbol_Access_Type);

      procedure Free_Symbol is new Ada.Unchecked_Deallocation (Symbol_Type'Class, Symbol_Access_Type);

      package Lists is new SAL.Poly.Lists.Single
         (Item_Type         => Symbol_Type'Class,
          Item_Node_Type    => Symbol_Access_Type,
          To_Item_Node      => Lists_Aux.New_Item,
          Free_Item         => Free_Symbol,
          Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));

      package Iterators is new Lists.Iterators;
      use Iterators;

      package Algorithms is new SAL.Gen.Alg
         (Item_Node_Type => Symbol_Access_Type,
          Container_Type => Lists.List_Type,
          Iterator_Type  => Iterators.Iterator_Type,
          Next_Procedure => Next,
          Next_Function  => Next);

      function Count is new Algorithms.Count;

      type Floating_Point_Type is new Symbol_Type with record
         Significant_Digits : Natural;
      end record;

      procedure Print (Item : in Floating_Point_Type);

      type Discrete_Number_Type is new Symbol_Type with record
         First : Integer;
         Last : Integer;
      end record;

      procedure Print (Item : in Discrete_Number_Type);

      procedure Print_Header (List : in Lists.List_Type);
      procedure Print_Trailer (List : in Lists.List_Type);

      procedure Print_List is new Algorithms.Process_All_Constant
         (Pre_Process_Container  => Print_Header,
          Process_Item           => Print_Symbol,
          Post_Process_Container => Print_Trailer);

   end Symbols;

   ------------
   --  Indefinite tagged limited type.

   package Puppets is
      type Label_Type is (Muppet, Beanie);

      type Parameters_Type (Label : Label_Type) is record
         case Label is
         when Muppet =>
            Arms : Integer;
         Fingers : Integer;
         when Beanie =>
            Legs : Integer;
         end case;
      end record;

      type Puppet_Type is abstract tagged limited null record;
      type Puppet_Access_Type is access Puppet_Type'Class;
      type Puppet_Access_Constant_Type is access constant Puppet_Type'Class;

      Storage_Pool_Name : aliased constant String := "Puppet_Storage_Pool";
      Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Storage_Pool_Name'Access);
      for Puppet_Access_Type'Storage_Pool use Storage_Pool;

      procedure Print (Item : in Puppet_Type) is abstract;
      procedure Print_Puppet
         (Item  : in Puppet_Access_Type;
          First : in Boolean := True);

      procedure Initialize (Item : in out Puppet_Type; Parameters : in Parameters_Type) is abstract;

      function Is_Equal (Left : in Parameters_Type; Right : in Puppet_Type) return Boolean is abstract;

      type Muppet_Type (Arms : Integer) is new Puppet_Type with record
         Fingers : Integer;
      end record;

      procedure Print (Item : in Muppet_Type);

      procedure Initialize (Item : in out Muppet_Type; Parameters : in Parameters_Type);

      function Is_Equal (Left : in Parameters_Type; Right : in Muppet_Type) return Boolean;

      type Beanie_Type is new Puppet_Type with record
         Legs : Integer;
      end record;

      procedure Print (Item : in Beanie_Type);

      procedure Initialize (Item : in out Beanie_Type; Parameters : in Parameters_Type);

      function Is_Equal (Left : in Parameters_Type; Right : in Beanie_Type) return Boolean;

      function Allocate (Parameters : in Parameters_Type) return Puppet_Access_Type;

      procedure Init_Class (Item : in out Puppet_Type'class; Parameters : in Parameters_Type);

      package Lists_Aux is new SAL.Aux.Indefinite_Limited_Items
         (Create_Parameters_Type => Parameters_Type,
          Limited_Type           => Puppet_Type'Class,
          Item_Access_Type       => Puppet_Access_Type,
          Allocate_Item          => Allocate,
          Initialize_Item        => Init_Class);

      procedure Free_Puppet is new Ada.Unchecked_Deallocation (Puppet_Type'Class, Puppet_Access_Type);

      package Lists is new SAL.Poly.Lists.Single
         (Item_Type         => Parameters_Type,
          Item_Node_Type    => Puppet_Access_Type,
          To_Item_Node      => Lists_Aux.New_Item,
          Free_Item         => Free_Puppet,
          Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));

      package Iterators is new Lists.Iterators;
      use Iterators;

      package Algorithms is new SAL.Gen.Alg
         (Item_Node_Type => Puppet_Access_Type,
          Container_Type => Lists.List_Type,
          Iterator_Type  => Iterators.Iterator_Type,
          Next_Procedure => Next,
          Next_Function  => Next);

      function Count is new Algorithms.Count;

      procedure Print_Header (List : in Lists.List_Type);
      procedure Print_Trailer (List : in Lists.List_Type);

      procedure Print_List is new Algorithms.Process_All_Constant
         (Pre_Process_Container  => Print_Header,
          Process_Item           => Print_Puppet,
          Post_Process_Container => Print_Trailer);

   end Puppets;

end Test_Poly_Lists_Single_Aux;
