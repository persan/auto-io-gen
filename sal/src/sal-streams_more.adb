--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2005 Stephen Leake.  All Rights Reserved.
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

package body SAL.Streams_More is

   procedure To_Stream
     (Item      : in     String;
      Data      : in out Ada.Streams.Stream_Element_Array;
      Data_Last : in out Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;
   begin
      for I in Item'Range loop
         Data_Last := Data_Last + 1;
         Data (Data_Last) := Stream_Element (Character'Pos (Item (I)));
      end loop;
   end To_Stream;

   function To_Stream (Item : in String) return Ada.Streams.Stream_Element_Array
   is
      use Ada.Streams;
      Data      : Stream_Element_Array (Stream_Element_Offset (Item'First) .. Stream_Element_Offset (Item'Last));
      Data_Last : Stream_Element_Offset := Data'First - 1;
   begin
      To_Stream (Item, Data, Data_Last);
      return Data;
   end To_Stream;

   procedure From_Stream
     (Data      : in     Ada.Streams.Stream_Element_Array;
      Item      : in out String;
      Item_Last : in out Natural)
   is begin
      for I in Data'Range loop
         Item_Last        := Item_Last + 1;
         Item (Item_Last) := Character'Val (Integer (Data (I)));
      end loop;
   end From_Stream;

   function To_String (Item : in Ada.Streams.Stream_Element_Array) return String
   is
      Result      : String (1 .. Integer (Item'Length));
      Result_Last : Integer := Result'First - 1;
   begin
      for I in Item'Range loop
         Result_Last := Result_Last + 1;
         Result (Result_Last) := Character'Val (Integer (Item (I)));
      end loop;
      return Result;
   end To_String;

end SAL.Streams_More;
