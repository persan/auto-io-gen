--  Abstract:
--
--  see spec
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
package body SAL.Memory_Streams.Bounded is

   procedure Create (Stream : in out Stream_Type)
   is begin
      Stream.Last := 0;
      Stream.Direction := Out_Stream;
   end Create;

   procedure Create
      (Stream : in out Stream_Type;
       Data : in Stream_Element_Array)
   is begin
      Stream.Raw (1 .. Data'Length) := Data;
      Stream.Last := 0;
      Stream.Direction := In_Stream;
   end Create;

   package Stream_Element_Address_Conversions is new System.Address_To_Access_Conversions (Stream_Element);

   procedure Create
      (Stream : in out Stream_Type;
       Address : in System.Address)
   is
      function "+" (Left : System.Address; Right : System.Storage_Elements.Storage_Offset)
                    return System.Address renames System.Storage_Elements."+";

      Temp : System.Address := Address;
   begin
      for I in Stream.Raw'Range loop
         Stream.Raw (I) := Stream_Element_Address_Conversions.To_Pointer (Address).all;
         Temp := Temp + 1;
      end loop;
      Stream.Direction := In_Stream;
      Stream.Last := 0;
   end Create;

   function Length (Stream : in Stream_Type) return Stream_Element_Count
   is begin
      case Stream.Direction is
      when In_Stream =>
         return Stream.Raw'Last - Stream.Last;
      when Out_Stream =>
         return Stream.Last;
      end case;
   end Length;

   function Address (Stream : in Stream_Type) return System.Address
   is begin
      case Stream.Direction is
      when In_Stream =>
         raise Status_Error;
      when Out_Stream =>
         return Stream.Raw (1)'Address;
      end case;
   end Address;

   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is begin
      case Stream.Direction is
      when In_Stream =>
         declare
            Remaining : constant Stream_Element_Offset := Stream.Raw'Last - Stream.Last;
         begin
            if Remaining >= Item'Length then
               Item := Stream.Raw (Stream.Last + 1 .. Stream.Last + Item'Length);
               Stream.Last := Stream.Last + Item'Length;
               Last := Item'Last;
            else
               Last := Item'First + Remaining - 1;
               Item (Item'First .. Last) := Stream.Raw (Stream.Last + 1 .. Stream.Raw'Last);
               Stream.Last := Stream.Raw'Last;
            end if;
         end;
      when Out_Stream =>
         raise Status_Error;
      end case;
   end Read;

   procedure Write
     (Stream : in out Stream_Type;
      Item   : in Stream_Element_Array)
   is begin
      case Stream.Direction is
      when In_Stream =>
         raise Status_Error;
      when Out_Stream =>
         declare
            Remaining : constant Stream_Element_Offset := Stream.Raw'Last - Stream.Last;
         begin
            if Remaining >= Item'Length then
               Stream.Raw (Stream.Last + 1 .. Stream.Last + Item'Length) := Item;
               Stream.Last := Stream.Last + Item'Length;
            else
               raise End_Error;
            end if;
         end;
      end case;
   end Write;

end SAL.Memory_Streams.Bounded;
