--  Abstract
--
--  Declarations to help test SAL.Poly.Lists.Double.
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

with Ada.Unchecked_Deallocation;
with System.Storage_Pools;
with SAL.Gen.Alg.Count;
with SAL.Gen.Alg.Process_All_Constant;
with SAL.Poly.Lists.Double.Validate;
with SAL.Aux.Definite_Private_Items;
with SAL.Aux.Indefinite_Private_Items;
with SAL.Aux.Indefinite_Limited_Items;
with Test_Storage_Pools;
package Test_Poly_Lists_Double_Aux is
   pragma Elaborate_Body; -- Body depends on Ada.Text_IO.

   -----------
   --  General utilities

   Node_Storage_Pool_Name : aliased constant String := "Node_Storage_Pool";
   Node_Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Node_Storage_Pool_Name'Access);

   ------------
   --  Definite non-limited item type.

   package Integers is
      package Lists_Aux is new SAL.Aux.Definite_Private_Items (Integer);
      package Lists is new SAL.Poly.Lists.Double
         (Item_Type         => Integer,
          Item_Node_Type    => Integer,
          To_Item_Node      => Lists_Aux.To_Item_Node,
          Free_Item         => Lists_Aux.Free_Item,
          Copy              => Lists_Aux.Copy,
          Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));

      package Algorithms is new SAL.Gen.Alg
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

      function Count_Integers is new Algorithms.Count;

      procedure Print_Integer
         (Item  : in Integer;
          First : in Boolean);

      procedure Print_Header (List : in Lists.List_Type);
      procedure Print_Trailer (List : in Lists.List_Type);

      procedure Print_List is new Algorithms.Process_All_Constant
         (Pre_Process_Container  => Print_Header,
          Process_Item           => Print_Integer,
          Post_Process_Container => Print_Trailer);

      package Validate is new Lists.Validate;

      procedure Check_Print (List : in Lists.List_Type; Label : in String);

   end Integers;

   ------------
   --  Indefinite non-limited type.

   package Symbols is
      type Symbol_Label_Type is (Floating_Point, Discrete);

      type Symbol_Type (Label : Symbol_Label_Type) is record
         case Label is
         when Floating_Point =>
            Significant_Digits : Natural;
         when Discrete =>
            First : Integer;
         Last : Integer;
         end case;
      end record;

      type Symbol_Access_Type is access Symbol_Type;

      Storage_Pool_Name : aliased constant String := "Symbol_Storage_Pool";
      Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Storage_Pool_Name'Access);
      for Symbol_Access_Type'Storage_Pool use Storage_Pool;

      procedure Print (Item : in Symbol_Type);
      procedure Print_Symbol
         (Item  : in Symbol_Access_Type;
          First : in Boolean := True);

      package Lists_Aux is new SAL.Aux.Indefinite_Private_Items
         (Item_Type        => Symbol_Type,
          Item_Access_Type => Symbol_Access_Type);

      procedure Free_Symbol is new Ada.Unchecked_Deallocation (Symbol_Type, Symbol_Access_Type);

      package Lists is new SAL.Poly.Lists.Double
         (Item_Type         => Symbol_Type,
          Item_Node_Type    => Symbol_Access_Type,
          To_Item_Node      => Lists_Aux.New_Item,
          Free_Item         => Free_Symbol,
          Copy              => Lists_Aux.Copy,
          Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));

      procedure Print_Header (List : in Lists.List_Type);
      procedure Print_Trailer (List : in Lists.List_Type);

      package Algorithms is new SAL.Gen.Alg
         (Item_Node_Type => Symbol_Access_Type,
          Container_Type => Lists.List_Type,
          Iterator_Type  => Lists.Iterator_Type,
          Current        => Lists.Current,
          None           => Lists.None,
          Is_Null        => Lists.Is_Null,
          Next_Procedure => Lists.Next,
          Next_Function  => Lists.Next,
          First          => Lists.First,
          Last           => Lists.Last);

      function Count_Symbols is new Algorithms.Count;

      procedure Print_List is new Algorithms.Process_All_Constant
         (Pre_Process_Container  => Print_Header,
          Process_Item           => Print_Symbol,
          Post_Process_Container => Print_Trailer);

      package Validate is new Lists.Validate;

      procedure Check_Print (List : in Lists.List_Type; Label : in String);

   end Symbols;

   ------------
   --  Indefinite limited type.

   package Puppets is
      type Puppet_Label_Type is (Muppet, Beanie);

      subtype Puppet_Parameters_Type is String;

      type Puppet_Type (Label : Puppet_Label_Type) is limited record
         case Label is
         when Muppet =>
            Arms : Integer;
            Fingers : Integer;
         when Beanie =>
            Legs : Integer;
         end case;
      end record;

      type Puppet_Access_Type is access Puppet_Type;

      Storage_Pool_Name : aliased constant String := "Puppet_Storage_Pool";
      Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Storage_Pool_Name'Access);
      for Puppet_Access_Type'Storage_Pool use Storage_Pool;

      procedure Print (Item : in Puppet_Type);

      procedure Print_Puppet
         (Item  : in Puppet_Access_Type;
          First : in Boolean);

      procedure Initialize (Item : in out Puppet_Type; Parameters : in Puppet_Parameters_Type);

      function Allocate (Parameters : in Puppet_Parameters_Type) return Puppet_Access_Type;

      function Copy_Puppet (Item : in Puppet_Access_Type) return Puppet_Access_Type;

      package Lists_Aux is new SAL.Aux.Indefinite_Limited_Items
         (Create_Parameters_Type => Puppet_Parameters_Type,
          Limited_Type           => Puppet_Type,
          Item_Access_Type       => Puppet_Access_Type,
          Allocate_Item          => Allocate,
          Initialize_Item        => Initialize);

      procedure Free_Puppet is new Ada.Unchecked_Deallocation (Puppet_Type, Puppet_Access_Type);

      package Lists is new SAL.Poly.Lists.Double
         (Item_Type         => Puppet_Parameters_Type,
          Item_Node_Type    => Puppet_Access_Type,
          To_Item_Node      => Lists_Aux.New_Item,
          Free_Item         => Free_Puppet,
          Copy              => Copy_Puppet,
          Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class (Node_Storage_Pool));

      package Algorithms is new SAL.Gen.Alg
         (Item_Node_Type => Puppet_Access_Type,
          Container_Type => Lists.List_Type,
          Iterator_Type  => Lists.Iterator_Type,
          Current        => Lists.Current,
          None           => Lists.None,
          Is_Null        => Lists.Is_Null,
          First          => Lists.First,
          Last           => Lists.Last,
          Next_Procedure => Lists.Next,
          Next_Function  => Lists.Next);

      function Count_Puppets is new Algorithms.Count;

      procedure Print_Header (List : in Lists.List_Type);
      procedure Print_Trailer (List : in Lists.List_Type);

      procedure Print_List is new Algorithms.Process_All_Constant
         (Pre_Process_Container  => Print_Header,
          Process_Item           => Print_Puppet,
          Post_Process_Container => Print_Trailer);

      package Validate is new Lists.Validate;

      procedure Check_Print (List : in Lists.List_Type; Label : in String);

   end Puppets;
end Test_Poly_Lists_Double_Aux;
