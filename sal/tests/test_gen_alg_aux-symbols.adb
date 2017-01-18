--  Abstract
--
--  See spec.
--
--  Copyright (C) 2000, 2002, 2006 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
package body Test_Gen_Alg_Aux.Symbols is

   -------------
   --  Operations for all algorithms

   procedure Print_Symbol
     (Item  : in Symbol_Access_Type;
      First : in Boolean := True)
   is begin
      if not First then
         Put_Line (",");
         Put (" ");
      end if;
      Print (Item.all);
   end Print_Symbol;

   function Is_Equal_Node_Class
      (Left  : in Symbol_Access_Type;
       Right : in Symbol_Type'Class)
      return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      if Left.all'Tag = Right'Tag then
         return Is_Equal (Left.all, Right);
      else
         return False;
      end if;
   end Is_Equal_Node_Class;

   function Is_Equal_Node_Tag
      (Left  : in Symbol_Access_Type;
       Right : in Ada.Tags.Tag)
      return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      return Left.all'Tag = Right;
   end Is_Equal_Node_Tag;

   function Is_Equal_Node (Left, Right : in Symbol_Access_Type) return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      if Left.all'Tag = Right.all'Tag then
         return Is_Equal (Left.all, Right.all);
      else
         return False;
      end if;
   end Is_Equal_Node;

   function Is_Greater_Equal_Node_Class (Left : in Symbol_Access_Type; Right : in Symbol_Type'Class) return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      if Left.all'Tag = Right'Tag then
         return Is_Greater_Equal (Left.all, Right);
      else
         return Left.all in Floating_Point_Type; -- Float is greater than Discrete
      end if;
   end Is_Greater_Equal_Node_Class;

   function Is_Greater_Equal_Node (Left, Right : in Symbol_Access_Type) return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      if Left.all'Tag = Right.all'Tag then
         return Is_Greater_Equal (Left.all, Right.all);
      else
         return Left.all in Floating_Point_Type; -- Float is greater than Discrete
      end if;
   end Is_Greater_Equal_Node;

   function Is_Less_Equal_Node_Class (Left : in Symbol_Access_Type; Right : in Symbol_Type'Class) return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      if Left.all'Tag = Right'Tag then
         return Is_Less_Equal (Left.all, Right);
      else
         return not (Left.all in Floating_Point_Type); -- Float is greater than Discrete
      end if;
   end Is_Less_Equal_Node_Class;

   --------------
   --  List container and algorithms

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

   -----------
   --  Array container and algorithms

   procedure Print_Header (List : in Symbol_Container_Type)
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

   procedure Print_Trailer (List : in Symbol_Container_Type)
   is
      pragma Unreferenced (List);
   begin
      Put_Line (")");
   end Print_Trailer;

   ------------
   --  Floating_Point derived item type

   procedure Print (Item : in Floating_Point_Type) is
   begin
      Put ("(Significant_Digits =>" & Natural'Image (Item.Significant_Digits) & ")");
   end Print;

   function Is_Equal (Left, Right : in Floating_Point_Type) return Boolean
   is begin
      return Left.Significant_Digits = Right.Significant_Digits;
   end Is_Equal;

   function Is_Greater_Equal (Left, Right : in Floating_Point_Type) return Boolean
   is begin
      return Left.Significant_Digits >= Right.Significant_Digits;
   end Is_Greater_Equal;

   function Is_Less_Equal (Left, Right : in Floating_Point_Type) return Boolean
   is begin
      return Left.Significant_Digits <= Right.Significant_Digits;
   end Is_Less_Equal;

   ------------
   --  Discrete derived item type

   procedure Print (Item : in Discrete_Number_Type) is
   begin
      Put ("(First =>" & Natural'Image (Item.First) & ", Last =>" & Natural'Image (Item.Last) & ")");
   end Print;

   function Is_Equal (Left, Right : in Discrete_Number_Type) return Boolean
   is begin
      return Left.Last = Right.Last;
   end Is_Equal;

   function Is_Greater_Equal (Left, Right : in Discrete_Number_Type) return Boolean
   is begin
      return Left.Last >= Right.Last;
   end Is_Greater_Equal;

   function Is_Less_Equal (Left, Right : in Discrete_Number_Type) return Boolean
   is begin
      return Left.Last <= Right.Last;
   end Is_Less_Equal;

end Test_Gen_Alg_Aux.Symbols;
