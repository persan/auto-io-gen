--  Abstract :
--
--  Utilities useful with Ada.Streams
--
--  Copyright (C) 2004, 2005, 2007 Stephen Leake.  All Rights Reserved.
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

pragma License (Modified_GPL);

with Ada.Streams;
package SAL.Streams_More is
   pragma Pure;

   procedure To_Stream
     (Item      : in     String;
      Data      : in out Ada.Streams.Stream_Element_Array;
      Data_Last : in out Ada.Streams.Stream_Element_Offset);
   --  Write Item to Data (Data_Last + 1 ...), update Data_Last to
   --  point to last element written.

   function To_Stream (Item : in String) return Ada.Streams.Stream_Element_Array;

   procedure From_Stream
     (Data      : in     Ada.Streams.Stream_Element_Array;
      Item      : in out String;
      Item_Last : in out Natural);
   --  Read Item (Item_Last + 1 .. ) from Data; update Item_Last to
   --  last character written.

   function To_String (Item : in Ada.Streams.Stream_Element_Array) return String;

end SAL.Streams_More;
