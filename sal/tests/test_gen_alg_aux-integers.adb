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

with Ada.Text_IO; use Ada.Text_IO;
package body Test_Gen_Alg_Aux.Integers is

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

   procedure Print_Integer
     (Item  : in Integer;
      First : in Boolean)
   is
   begin
      if not First then
         Put_Line (",");
         Put (" ");
      end if;
      Put (Integer'Image (Item));
   end Print_Integer;

   procedure Print_Trailer (List : in Lists.List_Type)
   is
      pragma Unreferenced (List);
   begin
      Put_Line (")");
   end Print_Trailer;

   --------------
   --  Unconstrained array container

   procedure Print_Header (List : in Integer_Container_Type)
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

   procedure Print_Trailer (List : in Integer_Container_Type)
   is
      pragma Unreferenced (List);
   begin
      Put_Line (")");
   end Print_Trailer;

end Test_Gen_Alg_Aux.Integers;
