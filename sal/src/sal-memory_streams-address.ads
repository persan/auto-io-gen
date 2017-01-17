--  Abstract:
--
--  A memory stream type that reads from an address, for processing data
--  returned by system calls.
--
--  These are input streams only, used when a system routine returns the
--  address of a buffer whose size is not known.
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
package SAL.Memory_Streams.Address is
   pragma Preelaborate; -- SAL.Memory_Streams is.

   type Stream_Type is new Root_Stream_Type with private;

   procedure Create
      (Stream  : in out Stream_Type;
       Address : in     System.Address);
   --  Create a Stream with data at Address, with direction In_Stream,
   --  for reading. The stream has no end. Data at Address is NOT
   --  copied.

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   : out    Stream_Element_Array;
      Last   : out    Stream_Element_Offset);
   --  reads elements from Stream, storing them in Item. Stops when
   --  Item'Last is reached, setting Last to Item'Last.

   overriding procedure Write
     (Stream : in out Stream_Type;
      Item   : in     Stream_Element_Array);
   --  raises Status_Error.

   --  The following are here because they involve peeking ahead in
   --  the stream, which is not supported for
   --  Ada.Streams.Root_Stream_Type

   function Null_Terminated_String_Length
      (Stream : in Stream_Type)
      return Natural;
   --  return number of characters to first 0 in Stream.
   --  Stream pointer is NOT advanced

   function Null_Terminated_Wide_String_Length
      (Stream : in Stream_Type)
      return Natural;
   --  return number of wide characters to first wide 0 in Stream.
   --  Stream pointer is NOT advanced

private
   type Stream_Type is new Root_Stream_Type with record
      --  Address is next Storage_Element to read; incremented with each Read.
      Address : System.Address;
   end record;

end SAL.Memory_Streams.Address;
