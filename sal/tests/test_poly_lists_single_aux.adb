--  Abstract :
--
--  see spec
--
--  Copyright (C) 1999, 2002 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
package body Test_Poly_Lists_Single_Aux is

   package body Integers is
      procedure Print_Integer
         (Item  : in Integer;
          First : in Boolean)
      is begin
         if First then
            Put ("Item => ");
         else
            Put_Line (",");
            Put (" Item => ");
         end if;
         Put (Integer'Image (Item));
      end Print_Integer;

      procedure Print_Header (List : in Lists.List_Type)
      is
         Item_Count : constant Natural := Count (List);
      begin
         Put ("List (Count => " & Integer'Image (Item_Count) & ") => ");
         if Item_Count /= 0 then
            New_Line;
         end if;
         Put ("(");
      end Print_Header;

      procedure Print_Trailer (List : in Lists.List_Type)
      is
         pragma Unreferenced (List);
      begin
         Put_Line (")");
      end Print_Trailer;
   end Integers;

   ------------

   package body Symbols is

      procedure Print_Symbol
         (Item  : in Symbol_Access_Type;
          First : in Boolean := True)
      is begin
         if First then
            Put ("Item => ");
         else
            Put_Line (",");
            Put (" Item => ");
         end if;
         Print (Item.all);
      end Print_Symbol;

      procedure Print (Item : in Floating_Point_Type)
      is begin
         Put ("Significant_Digits =>" & Natural'Image (Item.Significant_Digits));
      end Print;

      procedure Print (Item : in Discrete_Number_Type)
      is begin
         Put ("First =>" & Natural'Image (Item.First) & " Last =>" & Natural'Image (Item.Last));
      end Print;

      procedure Print_Header (List : in Lists.List_Type)
      is begin
         Put_Line ("List (Count => " & Integer'Image (Lists.Count (List)) & ")");
         Put ("(");
      end Print_Header;

      procedure Print_Trailer (List : in Lists.List_Type)
      is
         pragma Unreferenced (List);
      begin
         Put_Line (")");
      end Print_Trailer;

   end Symbols;

   ------------

   package body Puppets is

      procedure Print_Puppet
         (Item  : in Puppet_Access_Type;
          First : in Boolean := True)
      is begin
         if First then
            Put ("Item => ");
         else
            Put_Line (",");
            Put (" Item => ");
         end if;
         Print (Item.all);
      end Print_Puppet;

      procedure Print (Item : in Muppet_Type)
      is begin
         Put (Label_Type'Image (Muppet) & Integer'Image (Item.Arms) & Integer'Image (Item.Fingers));
      end Print;

      procedure Initialize (Item : in out Muppet_Type; Parameters : in Parameters_Type)
      is begin
         Item.Fingers := Parameters.Fingers;
      end Initialize;

      function Is_Equal (Left : in Parameters_Type; Right : in Muppet_Type) return Boolean
      is begin
         return Left.Label = Muppet and then
            (Left.Arms = Right.Arms and
             Left.Fingers = Right.Fingers);
      end Is_Equal;

      procedure Print (Item : in Beanie_Type)
      is begin
         Put (Label_Type'Image (Beanie) & Integer'Image (Item.Legs));
      end Print;

      procedure Initialize (Item : in out Beanie_Type; Parameters : in Parameters_Type)
      is begin
         Item.Legs := Parameters.Legs;
      end Initialize;

      function Is_Equal (Left : in Parameters_Type; Right : in Beanie_Type) return Boolean
      is begin
         return Left.Label = Beanie and then (Left.Legs = Right.Legs);
      end Is_Equal;

      function Allocate (Parameters : in Parameters_Type) return Puppet_Access_Type
      is begin
         case Parameters.Label is
         when Muppet =>
            return new Muppet_Type (Parameters.Arms);
         when Beanie =>
            return new Beanie_Type;
         end case;
      end Allocate;

      procedure Init_Class (Item : in out Puppet_Type'class; Parameters : in Parameters_Type)
      is begin
         Initialize (Item, Parameters);
      end Init_Class;

      procedure Print_Header (List : in Lists.List_Type)
      is begin
         Put_Line ("List (Count => " & Integer'Image (Count (List)) & ")");
         Put ("(");
      end Print_Header;

      procedure Print_Trailer (List : in Lists.List_Type)
      is
         pragma Unreferenced (List);
      begin
         Put_Line (")");
      end Print_Trailer;

   end Puppets;
end Test_Poly_Lists_Single_Aux;
