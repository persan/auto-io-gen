--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2001, 2002, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Asis.Aux;
with Asis.Declarations;
package body Auto_Io_Gen.Lists is

   function To_Item_Node (Item : in Asis.Element) return Variant_Access_Type
   is begin
      return new Variant_Type'(Item, Component_Lists.Null_List);
   end To_Item_Node;

   function Null_Copy (Item : in Variant_Access_Type) return Variant_Access_Type
   is begin
      raise Program_Error;
      return null;
   end Null_Copy;

   function Type_Name (Item : in Type_Descriptor_Type) return String
   is begin
      return Ada.Characters.Handling.To_String (Asis.Declarations.Defining_Name_Image (Item.Type_Name));
   end Type_Name;

   package body Type_Iterator_Aux is

      function To_Item (Item : in Type_Descriptor_Lists.Iterator_Type) return Type_Descriptor_Lists.Iterator_Type
      is begin
         return Item;
      end To_Item;

      function To_Key  (Item : in Type_Descriptor_Lists.Iterator_Type) return String
      is begin
         return Type_Name (Type_Descriptor_Lists.Current (Item).all);
      end To_Key;

      procedure Free_Item (Item : in out Type_Descriptor_Lists.Iterator_Type)
      is
         pragma Unreferenced (Item);
      begin
         null;
      end Free_Item;

   end Type_Iterator_Aux;

   package body Context_Aux is

      function Copy    (Item : in Context_Type) return Context_Type
      is begin
         return (new String'(Item.Name.all), Item.Need_Use);
      end Copy;

      function To_Item (Item : in Context_Type) return Context_Type
      is begin
         return Item;
      end To_Item;

      function To_Key  (Item : in Context_Type) return String
      is begin
         return Item.Name.all;
      end To_Key;

      procedure Free_Item (Item : in out Context_Type)
      is
      begin
         Free_String (Item.Name);
      end Free_Item;

   end Context_Aux;

   package body Name_Aux is

      function Copy (Item : in String) return String
      is begin
         return Item;
      end Copy;

      function To_Item_Node (Item : in String) return String_Access_Type
      is begin
         return new String'(Item);
      end To_Item_Node;

      function To_Key (Item : in String_Access_Type) return String
      is begin
         return Item.all;
      end To_Key;

   end Name_Aux;

   function To_Item_Node (Item : in Asis.Element) return Formal_Package_Access_Type
   is begin
      return new Formal_Package_Type'(Item, Element_Lists.Null_List);
   end To_Item_Node;

   function Null_Copy (Item : in Formal_Package_Access_Type) return Formal_Package_Access_Type
   is begin
      raise Program_Error;
      return Item; --  keep compiler happy
   end Null_Copy;

   function Find_Package (List : in Formal_Package_Lists.List_Type; Name : in String) return Asis.Element
   is
      use Formal_Package_Lists;
      I : Iterator_Type := First (List);
   begin
      loop
         if Is_Null (I) then
            return Asis.Nil_Element;
         elsif Name = Asis.Aux.Name (Current (I).Package_Declaration) then
            return Current (I).Package_Declaration;
         end if;

         Next (I);
      end loop;
   end Find_Package;

   function Is_Present (List : in Formal_Package_Lists.List_Type; Name : in String) return Boolean
   is
      use Formal_Package_Lists;
      I : Iterator_Type := First (List);
   begin
      loop
         if Is_Null (I) then
            return False;
         elsif Name = Asis.Aux.Name (Current (I).Package_Declaration) then
            return True;
         end if;

         Next (I);
      end loop;
   end Is_Present;

end Auto_Io_Gen.Lists;
