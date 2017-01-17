--  Abstract:
--
--  A memory stream type, for obtaining raw byte images of types.
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
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

with System;
package SAL.Memory_Streams.Bounded is
   pragma Preelaborate; -- SAL.Memory_Streams is.

   type Stream_Type (Max_Length : Stream_Element_Count)
   is new Root_Stream_Type with private;

   procedure Create (Stream : in out Stream_Type);
   --  create an empty Stream with direction Out_Stream, for writing.

   procedure Create
      (Stream : in out Stream_Type;
       Data : in Stream_Element_Array);
   --  create a Stream with data, with direction In_Stream, for reading.
   --  raises Constraint_Error if Data overflows Stream

   procedure Create
      (Stream : in out Stream_Type;
       Address : in System.Address);
   --  create a Stream with data from Address, copying
   --  Stream.Max_Length bytes, with direction In_Stream, for reading.

   function Length (Stream : in Stream_Type) return Stream_Element_Count;
   --  for an In_Stream, the amount of data left to be read.
   --  for an Out_Stream, the amount of data written.

   function Address (Stream : in Stream_Type) return System.Address;
   --  for an In_Stream, raises Status_Error.
   --  for an Out_Stream, the address of the first element of the raw
   --  Stream, for passing to system routines.

   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   --  for an In_Stream, reads elements from Stream, storing them in
   --  Item. Stops when Item'Last or end of Stream is reached, setting
   --  Last to last element of Item written.
   --
   --  for an Out_Stream, raises Status_Error.

   procedure Write
     (Stream : in out Stream_Type;
      Item   : in Stream_Element_Array);
   --  for an In_Stream, raises Status_Error.
   --
   --  for an Out_Stream, writes elements from Item to the Stream,
   --  stopping when Item'last is reached. Raises End_Error if attempt
   --  to write past end of Stream.

private
   type Stream_Type (Max_Length : Stream_Element_Count)
   is new Ada.Streams.Root_Stream_Type with
   record
      --  Direction is not a discriminant, because we anticipate
      --  changing direction on some streams.
      Direction : Direction_Type;
      Last : Stream_Element_Offset := 0; -- last element of Raw that has been read/written
      Raw : Stream_Element_Array (1 .. Max_Length);
   end record;

end SAL.Memory_Streams.Bounded;
