--  Abstract :
--
--  See spec
--
--  Copyright (C) 1999, 2000 Stephen Leake.  All Rights Reserved.
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
package body SAL.Memory_Streams.Bounded.Test
is
   type Point_Type is record
      X : Integer;
      Y : Integer;
   end record;

   Point : Point_Type := (0, 0);

   Memory_Buffer : aliased Stream_Type (10);

   procedure Put (Item : in Point_Type)
   is begin
      Put ("(" & Integer'Image (Item.X) & ", ");
      Put (Integer'Image (Item.Y) & ")");
   end Put;

   procedure Put (Item : in Stream_Element_Array)
   is begin
      Put ("(");
      for I in Item'First .. Item'Last - 1 loop
         Put (Stream_Element'Image (Item (I)) & ", ");
      end loop;
      Put (Stream_Element'Image (Item (Item'Last)) & ")");
   end Put;

   procedure Run_Test
   is begin
      Put_Line ("testing SAL.Memory_Streams.Bounded");

      Put_Line ("write => (1, 2)");
      Create (Memory_Buffer);
      Point_Type'Write (Memory_Buffer'Access, Point_Type'(1, 2));
      Put ("Got => "); Put (Memory_Buffer.Raw (1 .. Memory_Buffer.Last));
      New_Line (2);

      Put_Line ("read => ");
      Create (Memory_Buffer, Memory_Buffer.Raw (1 .. Memory_Buffer.Last));
      Point_Type'Read (Memory_Buffer'Access, Point);
      Put (Point);
      New_Line (2);

      Put_Line ("done");
   end Run_Test;

end SAL.Memory_Streams.Bounded.Test;
