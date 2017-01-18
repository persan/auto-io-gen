--  Abstract :
--
--  See spec
--
--  Copyright (C) 1999, 2002, 2003 Stephen Leake.  All Rights Reserved.
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
with Ada.Strings.Fixed;
package body Test_Gen_Lists_Double_Aux is

   package body Integers is

      procedure Print_Integer
         (Item  : in Integer;
          First : in Boolean)
      is begin
         if not First then
            Put_Line (",");
            Put (" ");
         end if;
         Put (Integer'Image (Item));
      end Print_Integer;

      procedure Print_Header (List : in Lists.List_Type)
      is
         Item_Count : constant Natural := Count_Integers (List);
      begin
         --  Check_Print below prints list name
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

      procedure Check_Print (List : in Lists.List_Type; Label : in String)
      is begin
         Put (Label);
         Validate.Validate (List);
         Print_List (List);
      end Check_Print;

   end Integers;

   -----------
   --  Symbol_Lists

   package body Symbols is

      procedure Print (Item : in Symbol_Type)
      is begin
         Put (Symbol_Label_Type'Image (Item.Label) & " ");
         case Item.Label is
         when Floating_Point =>
            Put ("Significant_Digits =>" & Natural'Image (Item.Significant_Digits));
         when Discrete =>
            Put ("First =>" & Natural'Image (Item.First) & " Last =>" & Natural'Image (Item.Last));
         end case;
      end Print;

      procedure Print_Symbol
         (Item  : in Symbol_Access_Type;
          First : in Boolean            := True)
      is begin
         if not First then
            Put_Line (",");
            Put (" ");
         end if;
         Print (Item.all);
      end Print_Symbol;

      procedure Print_Header (List : in Lists.List_Type)
      is
         Item_Count : constant Natural := Count_Symbols (List);
      begin
         --  Check_Print below prints list name
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

      procedure Check_Print (List : in Lists.List_Type; Label : in String)
      is begin
         Put (Label);
         Validate.Validate (List);
         Print_List (List);
      end Check_Print;

   end Symbols;

   ------------
   --  Indefinite limited type.

   package body Puppets is

      procedure Print (Item : in Puppet_Type)
      is begin
         Put (Puppet_Label_Type'Image (Item.Label));
         case Item.Label is
         when Muppet =>
            Put (Integer'Image (Item.Arms) & Integer'Image (Item.Fingers));
         when Beanie =>
            Put (Integer'Image (Item.Legs));
         end case;
      end Print;

      procedure Print_Puppet
         (Item  : in Puppet_Access_Type;
          First : in Boolean)
      is begin
         if not First then
            Put_Line (",");
            Put (" ");
         end if;
         Print (Item.all);
      end Print_Puppet;

      procedure Initialize (Item : in out Puppet_Type; Parameters : in Puppet_Parameters_Type)
      is
         First : Natural := Parameters'First;
         Last : Natural;
         use Ada.Strings.Fixed;
      begin
         Last := Index (Parameters, " ");
         if Item.Label /= Puppet_Label_Type'Value (Parameters (First .. Last - 1)) then
            raise Constraint_Error;
         else
            case Item.Label is
            when Muppet =>
               First := Last + 1;
               Last := Index (Parameters (First .. Parameters'Last), " ");
               Item.Arms := Integer'Value (Parameters (First .. Last));
               Item.Fingers := Integer'Value (Parameters (Last .. Parameters'Last));
            when Beanie =>
               Item.Legs := Integer'Value (Parameters (Last .. Parameters'Last));
            end case;
         end if;
      end Initialize;

      function Allocate (Parameters : in Puppet_Parameters_Type) return Puppet_Access_Type
      is
         First : constant Natural := Parameters'First;
         Last  : Natural;
         use Ada.Strings.Fixed;
      begin
         Last := Index (Parameters, " ");
         case Puppet_Label_Type'Value (Parameters (First .. Last - 1)) is
         when Muppet =>
            return new Puppet_Type (Muppet);
         when Beanie =>
            return new Puppet_Type (Beanie);
         end case;
      end Allocate;

      function Copy_Puppet (Item : in Puppet_Access_Type) return Puppet_Access_Type
      is
         Result : constant Puppet_Access_Type := new Puppet_Type (Item.Label);
      begin
         case Item.Label is
         when Muppet =>
            Result.Arms := Item.Arms;
            Result.Fingers := Item.Fingers;
         when Beanie =>
            Result.Legs := Item.Legs;
         end case;
         return Result;
      end Copy_Puppet;

      procedure Print_Header (List : in Lists.List_Type)
      is
         Item_Count : constant Natural := Count_Puppets (List);
      begin
         --  Check_Print below prints list name
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

      procedure Check_Print (List : in Lists.List_Type; Label : in String)
      is begin
         Put (Label);
         Validate.Validate (List);
         Print_List (List);
      end Check_Print;

   end Puppets;

end Test_Gen_Lists_Double_Aux;
