--  Abstract
--
--  See spec.
--
--  Copyright (C) 2000, 2002 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO; use Ada.Text_IO;
package body Test_Gen_Alg_Aux.Puppets is

   --  Need library level specific access types for Allocate.
   type Muppet_Access_Type is access Muppet_Type;
   for Muppet_Access_Type'Storage_Pool use Puppet_Access_Type'Storage_Pool;

   type Beanie_Access_Type is access Beanie_Type;
   for Beanie_Access_Type'Storage_Pool use Puppet_Access_Type'Storage_Pool;

   procedure Print (Item : in Parameters_Type)
   is begin
      Put ("(" & Label_Type'Image (Item.Label) & ", ");
      case Item.Label is
      when Muppet =>
         Put ("Arms => " & Integer'Image (Item.Arms) & ", Fingers => " & Integer'Image (Item.Fingers));
      when Beanie =>
         Put ("Legs => " & Integer'Image (Item.Legs));
      end case;
      Put (")");
   end Print;

   ------------
   --  Container support

   function Allocate (Parameters : in Parameters_Type) return Puppet_Access_Type
   is
   begin
      case Parameters.Label is
      when Muppet =>
         declare
            Item : constant Muppet_Access_Type := new Muppet_Type (Parameters.Arms);
         begin
            Item.Fingers := Parameters.Fingers;
            return Puppet_Access_Type (Item);
         end;

      when Beanie =>
         declare
            Item : constant Beanie_Access_Type := new Beanie_Type;
         begin
            Item.Legs := Parameters.Legs;
            return Puppet_Access_Type (Item);
         end;

      end case;
   end Allocate;

   procedure Init_Class
      (Item       : in out Puppet_Type'class;
       Parameters : in     Parameters_Type)
   is
   begin
      Initialize (Item, Parameters);
   end Init_Class;

   -----------
   --  Operations for all algorithms

   procedure Print_Puppet
     (Item  : in Puppet_Access_Type;
      First : in Boolean := True)
   is begin
      if not First then
         Put_Line (",");
         Put (" ");
      end if;
      Print (Item.all);
   end Print_Puppet;

   function Is_Equal_Puppet (Left : in Puppet_Access_Type; Right : in Parameters_Type) return Boolean
   is begin
      return Is_Equal (Left.all, Right);
   end Is_Equal_Puppet;

   function Is_Equal_Label (Left : in Puppet_Access_Type; Right : in Label_Type) return Boolean
   is begin
      return Is_Equal (Left.all, Right);
   end Is_Equal_Label;

   function Is_Equal_Node (Left, Right : in Puppet_Access_Type) return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      if Left.all'Tag = Right.all'Tag then
         return Is_Equal (Left.all, Right.all);
      else
         return False;
      end if;
   end Is_Equal_Node;

   function Is_Greater_Equal_Param (Left : in Puppet_Access_Type; Right : in Parameters_Type) return Boolean
   is begin
      return Is_Greater_Equal (Left.all, Right);
   end Is_Greater_Equal_Param;

   function Is_Greater_Equal_Node (Left, Right : in Puppet_Access_Type) return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      if Right.all'Tag = Left.all'Tag then
         return Is_Greater_Equal (Left.all, Right.all);

      else
         return Left.all in Beanie_Type; -- Beanies are greater than Muppets

      end if;
   end Is_Greater_Equal_Node;

   function Is_Less_Equal_Param (Left : in Puppet_Access_Type; Right : in Parameters_Type) return Boolean
   is begin
      return Is_Less_Equal (Left.all, Right);
   end Is_Less_Equal_Param;

   ----------------
   --  List Container

   procedure Print_Header (List : in Lists.List_Type)
   is
      Item_Count : constant Natural := Count (List);
   begin
      Put (" (Count => " & Integer'Image (Item_Count) & ") => ");
      if Item_Count = 0 then
         Put ("(");
      else
         New_Line;
         Put ("(");
      end if;
   end Print_Header;

   procedure Print_Trailer (List : in Lists.List_Type)
   is
      pragma Unreferenced (List);
   begin
      Put_Line (")");
   end Print_Trailer;

   ----------------
   --  Array Container

   procedure Print_Header (List : in Puppet_Container_Type)
   is
      Item_Count : constant Natural := Count (List);
   begin
      Put (" (Count => " & Integer'Image (Item_Count) & ") => ");
      if Item_Count = 0 then
         Put ("(");
      else
         New_Line;
         Put ("(");
      end if;
   end Print_Header;

   procedure Print_Trailer (List : in Puppet_Container_Type)
   is
      pragma Unreferenced (List);
   begin
      Put_Line (")");
   end Print_Trailer;

   --------------
   --  Muppets

   procedure Print (Item : in Muppet_Type) is
   begin
      Put ("(Muppet) (Arms => " & Integer'Image (Item.Arms) & ", Fingers => " & Integer'Image (Item.Fingers) & ")");
   end Print;

   procedure Initialize
     (Item : in out Muppet_Type;
      Parameters : in Parameters_Type)
   is begin
      Item.Fingers := Parameters.Fingers;
   end Initialize;

   function Is_Equal (Left, Right : in Muppet_Type) return Boolean
   is begin
      return Left.Arms = Right.Arms and Left.Fingers = Right.Fingers;
   end Is_Equal;

   function Is_Equal (Left : in Muppet_Type; Right : in Parameters_Type) return Boolean
   is begin
      return Muppet = Right.Label and then
         (Left.Arms = Right.Arms and
          Left.Fingers = Right.Fingers);
   end Is_Equal;

   function Is_Equal (Left : in Muppet_Type; Right : in Label_Type) return Boolean
   is
      pragma Unreferenced (Left);
   begin
      return Right = Muppet;
   end Is_Equal;

   function Is_Greater_Equal (Left : in Muppet_Type; Right : in Parameters_Type) return Boolean
   is begin
      case Right.Label is
      when Muppet =>
         if Left.Arms = Right.Arms then
            return  Left.Fingers >= Right.Fingers;
         else
            return Left.Arms >= Right.Arms;
         end if;

      when Beanie =>
         return False; -- Beanies are greater than Muppets

      end case;
   end Is_Greater_Equal;

   function Is_Greater_Equal (Left, Right : in Muppet_Type) return Boolean
   is begin
      if Left.Arms = Right.Arms then
         return  Left.Fingers >= Right.Fingers;
      else
         return Left.Arms >= Right.Arms;
      end if;
   end Is_Greater_Equal;

   function Is_Less_Equal (Left : in Muppet_Type; Right : in Parameters_Type) return Boolean
   is begin
      case Right.Label is
      when Muppet =>
         if Left.Arms = Right.Arms then
            return  Left.Fingers <= Right.Fingers;
         else
            return Left.Arms <= Right.Arms;
         end if;

      when Beanie =>
         return False; -- Beanies are greater than Muppets

      end case;
   end Is_Less_Equal;

   ------------
   --  Beanies

   procedure Print (Item : in Beanie_Type) is
   begin
      Put ("(Beanie) (Legs => " & Integer'Image (Item.Legs) & ")");
   end Print;

   procedure Initialize
     (Item : in out Beanie_Type;
      Parameters : in Parameters_Type)
   is begin
      Item.Legs := Parameters.Legs;
   end Initialize;

   function Is_Equal (Left, Right : in Beanie_Type) return Boolean
   is begin
      return Left.Legs = Right.Legs;
   end Is_Equal;

   function Is_Equal (Left : in Beanie_Type; Right : in Parameters_Type) return Boolean
   is begin
      return Right.Label = Beanie and then Left.Legs = Right.Legs;
   end Is_Equal;

   function Is_Equal (Left : in Beanie_Type; Right : in Label_Type) return Boolean
   is
      pragma Unreferenced (Left);
   begin
      return Right = Beanie;
   end Is_Equal;

   function Is_Greater_Equal (Left : in Beanie_Type; Right : in Parameters_Type) return Boolean
   is begin
      case Right.Label is
      when Muppet =>
         return True; -- Beanies are greater than Muppets

      when Beanie =>
         return Left.Legs >= Right.Legs;

      end case;
   end Is_Greater_Equal;

   function Is_Greater_Equal (Left, Right : in Beanie_Type) return Boolean
   is begin
      return Left.Legs >= Right.Legs;
   end Is_Greater_Equal;

   function Is_Less_Equal (Left : in Beanie_Type; Right : in Parameters_Type) return Boolean
   is begin
      case Right.Label is
      when Muppet =>
         return False; -- Beanies are greater than Muppets

      when Beanie =>
         return Left.Legs <= Right.Legs;

      end case;
   end Is_Less_Equal;

end Test_Gen_Alg_Aux.Puppets;
