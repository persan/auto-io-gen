--  Abstract:
--
--  see spec
--
--  We assume a byte-addressable machine
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

with System.Address_To_Access_Conversions;
with System.Storage_Elements;
package body SAL.Memory_Streams.Address is

   procedure Create
      (Stream : in out Stream_Type;
       Address : in System.Address)
   is begin
      Stream.Address := Address;
   end Create;

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      use type System.Storage_Elements.Storage_Offset;
   begin
      for I in Item'Range loop
         Item (I) := Stream_Element_Address_Conversions.To_Pointer (Stream.Address).all;
         Stream.Address := Stream.Address + 1;
      end loop;
      Last := Item'Last;
   end Read;

   overriding procedure Write
     (Stream : in out Stream_Type;
      Item   : in Stream_Element_Array)
   is begin
      raise Status_Error;
   end Write;

   package Character_Address_Conversions is new System.Address_To_Access_Conversions (Character);

   function Null_Terminated_String_Length
      (Stream : in Stream_Type)
      return Natural
   is
      use type System.Storage_Elements.Storage_Offset;
      Count : System.Storage_Elements.Storage_Offset := 0;
   begin
      while ASCII.NUL /= Character_Address_Conversions.To_Pointer (Stream.Address + Count).all
      loop
         Count := Count + 1;
      end loop;
      return Natural (Count);
   end Null_Terminated_String_Length;

   package Wide_Character_Address_Conversions is new System.Address_To_Access_Conversions (Wide_Character);

   function Null_Terminated_Wide_String_Length
      (Stream : in Stream_Type)
      return Natural
   is
      use type System.Storage_Elements.Storage_Offset;
      Count : System.Storage_Elements.Storage_Offset := 0;
   begin
      while Wide_Character'Val (0) /=
         Wide_Character_Address_Conversions.To_Pointer (Stream.Address + 2 * Count).all
      loop
         Count := Count + 1;
      end loop;
      return Natural (Count);
   end Null_Terminated_Wide_String_Length;

end SAL.Memory_Streams.Address;
