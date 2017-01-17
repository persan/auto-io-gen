--  Abstract :
--
--  Image functions for types in Ada.Streams
--
--  Copyright (C) 2007 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

package body SAL.Streams_More.Images is

   function Hex_Image (Item : in Ada.Streams.Stream_Element_Array) return String
   is
      use SAL.Streams_More.Images;
      use type Ada.Streams.Stream_Element_Offset;
      Result       : String (1 .. Item'Length * 3) := (others => ' ');
      Result_First : Integer                       := Result'First;
   begin
      for I in Item'Range loop
         Result (Result_First .. Result_First + 1) := Hex_Image (Item (I));
         Result_First := Result_First + 3; --  leave a space between
      end loop;
      return Result;
   end Hex_Image;

end SAL.Streams_More.Images;
