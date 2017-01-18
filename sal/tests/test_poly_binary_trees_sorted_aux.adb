--  Abstract :
--
--  See spec
--
--  Copyright (C) 1999 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
package body Test_Poly_Binary_Trees_Sorted_Aux is

   procedure Put_String (Item : in String)
   is begin
      Ada.Text_IO.Put (Item);
   end Put_String;

   procedure Put_String_Access (Item : in String_Access_Type)
   is begin
      Ada.Text_IO.Put (Item.all);
   end Put_String_Access;

end Test_Poly_Binary_Trees_Sorted_Aux;
