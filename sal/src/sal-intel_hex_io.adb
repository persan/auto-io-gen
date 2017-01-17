--  Abstract:
--
--  See spec.
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

with SAL.Generic_Hex_Image;
package body SAL.Intel_Hex_IO is

   Max_16_Bit_Address : constant Interfaces.Unsigned_32 := 16#FFFF#;
   Max_20_Bit_Address : constant Interfaces.Unsigned_32 := 16#F_FFFF#;

   Record_Overhead : constant := 11;

   procedure Initialize (File : in out File_Type)
   is begin
      File.Current_Segment := 0;
      File.Current_Offset  := 0;
      File.Buffer_Last     := 0;
   end Initialize;

   function Get_Segment_20 (Address : in Interfaces.Unsigned_32) return Interfaces.Unsigned_16
   is
      use Interfaces;
   begin
      return Unsigned_16 (Address / 16) and 16#F000#;
   end Get_Segment_20;

   function Get_Segment_32 (Address : in Interfaces.Unsigned_32) return Interfaces.Unsigned_16
   is
      use Interfaces;
   begin
      return Unsigned_16 (Address / 16#1_0000#);
   end Get_Segment_32;

   function Get_Offset (File : in File_Type; Address : in Interfaces.Unsigned_32) return Interfaces.Unsigned_16
   is
      use Interfaces;
   begin
      case File.Address_Mode is
      when Bits_16 =>
         return Unsigned_16 (Address);
      when Bits_20 =>
         return Unsigned_16 (Address - Unsigned_32 (File.Current_Segment) * 16);
      when Bits_32 =>
         return Unsigned_16 (Address mod 16#1_0000#);
      end case;
   end Get_Offset;

   function Get_Address (File : in File_Type) return Interfaces.Unsigned_32
   is
      use Interfaces;
   begin
      case File.Address_Mode is
      when Bits_16 =>
         return Unsigned_32 (File.Current_Offset) + Unsigned_32 (File.Buffer_Last) - 1;
      when Bits_20 =>
         return
           Unsigned_32 (File.Current_Segment) * 16 +
           Unsigned_32 (File.Current_Offset) +
           Unsigned_32 (File.Buffer_Last) - 1;
      when Bits_32 =>
         return
           Unsigned_32 (File.Current_Segment) * 16#1_0000# +
           Unsigned_32 (File.Current_Offset) +
           Unsigned_32 (File.Buffer_Last) - 1;
      end case;
   end Get_Address;

   function Hex_Image is new SAL.Generic_Hex_Image (Width => 2, Number_Type => Interfaces.Unsigned_8);
   function Hex_Image is new SAL.Generic_Hex_Image (Width => 4, Number_Type => Interfaces.Unsigned_16);

   function Hex_Value (Data : in Character) return Interfaces.Unsigned_8
   is
      use Interfaces;
   begin
      if Data > '9' then
         return Unsigned_8 (Character'Pos (Data) - Character'Pos ('A') + 10);
      else
         return Unsigned_8 (Character'Pos (Data) - Character'Pos ('0'));
      end if;
   end Hex_Value;

   function Hex_Value (Data : in String) return Interfaces.Unsigned_8
   is
      use Interfaces;
   begin
      return Hex_Value (Data (Data'First)) * 16 + Hex_Value (Data (Data'Last));
   end Hex_Value;

   function Hex_Value (Data : in String) return Interfaces.Unsigned_16
   is
      use Interfaces;
   begin
      return Unsigned_16 (Unsigned_8'(Hex_Value (Data (Data'First .. Data'First + 1)))) * 256 +
        Unsigned_16 (Unsigned_8'(Hex_Value (Data (Data'Last - 1 .. Data'Last))));
   end Hex_Value;

   function Checksum_16 (Item : Interfaces.Unsigned_16) return Interfaces.Unsigned_8
   is
      use Interfaces;
   begin
      return Unsigned_8 (Item / 256) + Unsigned_8 (Item mod 256);
   end Checksum_16;

   procedure Write_Data (File : in out File_Type)
   is
      use Ada.Text_IO, Interfaces;
      Checksum : Unsigned_8 := File.Buffer_Last + Checksum_16 (File.Current_Offset);
   begin
      if File.Buffer_Last = File.Buffer'First - 1 then
         --  Nothing to write
         return;
      end if;

      Put (File.File, ":" & Hex_Image (File.Buffer_Last) & Hex_Image (File.Current_Offset) & "00");
      for I in File.Buffer'First .. File.Buffer_Last loop
         Checksum := Checksum + File.Buffer (I);
         Put (File.File, Hex_Image (File.Buffer (I)));
      end loop;
      Put_Line (File.File, Hex_Image (-Checksum));
      File.Buffer_Last := File.Buffer'First - 1;
   end Write_Data;

   procedure Read_Data (File : in out File_Type)
   is
      use Ada.Text_IO, Interfaces;
      Checksum    : Unsigned_8;
      Temp_Offset : Unsigned_16;
      Line        : String (1 .. Max_Bytes_Per_Line * 2 + Record_Overhead + 1); --  +1 for DOS CR LF on Unix
      Line_Last   : Integer;
      Data_Count  : Unsigned_8;
      I           : Integer;
      Record_Type : Unsigned_8;
   begin
      Get_Line (File.File, Line, Line_Last);

      if Line (Line_Last) = ASCII.CR then
         --  Reading a DOS format file on a Unix system; ignore the CR
         Line_Last := Line_Last - 1;
      end if;

      Data_Count := Unsigned_8 ((Line_Last - Record_Overhead) / 2);

      if Line_Last < Record_Overhead then
         raise Invalid_Format;
      elsif Line (1) /= ':' then
         raise Invalid_Format;
      elsif Hex_Value (Line (2 .. 3)) /= Data_Count then
         raise Invalid_Format;
      end if;

      Checksum := Data_Count;

      Temp_Offset := Hex_Value (Line (4 .. 7));
      Checksum    := Checksum + Checksum_16 (Temp_Offset);

      Record_Type := Hex_Value (Line (8 .. 9));
      Checksum    := Checksum + Record_Type;

      case Record_Type is
      when 0 =>
         --  Data record
         I                   := 10;
         File.Buffer_Last    := File.Buffer'First - 1;
         File.Current_Offset := Temp_Offset;
         File.Bytes_Per_Line := Data_Count;

         loop
            exit when I > Line_Last - 2;
            File.Buffer_Last               := File.Buffer_Last + 1;
            File.Buffer (File.Buffer_Last) := Hex_Value (Line (I .. I + 1));
            Checksum                       := Checksum + File.Buffer (File.Buffer_Last);
            I                              := I + 2;
         end loop;
         if Checksum + Hex_Value (Line (Line_Last - 1 .. Line_Last)) /= 0 then
            raise Invalid_Format;
         end if;

      when 1 =>
         --  End of file record
         if Line (1 .. Line_Last) /= ":00000001FF" then
            raise Invalid_Format;
         end if;

      when 2 =>
         --  Extended segment address record
         File.Address_Mode    := Bits_20;
         File.Current_Segment := Hex_Value (Line (10 .. 13));
         Checksum             := Checksum + Checksum_16 (File.Current_Segment);
         if Checksum + Hex_Value (Line (Line_Last - 1 .. Line_Last)) /= 0 then
            raise Invalid_Format;
         end if;

      when 3 =>
         --  Start segment address record
         raise SAL.Not_Implemented;

      when 4 =>
         --  Extended linear address record
         File.Address_Mode    := Bits_32;
         File.Current_Segment := Hex_Value (Line (10 .. 13));
         Checksum             := Checksum + Checksum_16 (File.Current_Segment);
         if Checksum + Hex_Value (Line (Line_Last - 1 .. Line_Last)) /= 0 then
            raise Invalid_Format;
         end if;

      when 5 =>
         --  Start linear address record
         raise SAL.Not_Implemented;

      when others =>
         raise Invalid_Format;
      end case;
   end Read_Data;

   procedure Write_Segment_20 (File : in out File_Type; Segment : in Interfaces.Unsigned_16)
   is
      use Ada.Text_IO, Interfaces;
      Checksum : constant Unsigned_8 := 4 + Checksum_16 (Segment);
   begin
      Put_Line (File.File, ":02000002" & Hex_Image (Segment) & Hex_Image (-Checksum));
      File.Current_Segment := Segment;
   end Write_Segment_20;

   procedure Write_Extended_Linear_Address (File : in out File_Type; Segment : in Interfaces.Unsigned_16)
   is
      use Ada.Text_IO, Interfaces;
      Checksum : constant Unsigned_8 := 6 + Checksum_16 (Segment);
   begin
      Put_Line (File.File, ":02000004" & Hex_Image (Segment) & Hex_Image (-Checksum));
      File.Current_Segment := Segment;
   end Write_Extended_Linear_Address;

   procedure Add_Byte (File : in out File_Type; Address : in Interfaces.Unsigned_32; Byte : in Interfaces.Unsigned_8)
   is
      use type Interfaces.Unsigned_8;
   begin
      if File.Buffer_Last = File.Bytes_Per_Line then
         Write_Data (File);
      end if;

      File.Buffer_Last               := File.Buffer_Last + 1;
      File.Buffer (File.Buffer_Last) := Byte;

      if File.Buffer_Last = File.Buffer'First then
         File.Current_Offset := Get_Offset (File, Address);
      end if;
   end Add_Byte;

   ----------
   --  Public subprograms

   procedure Create
     (File           :    out File_Type;
      Name           : in     String;
      Mode           : in     Mode_Type;
      Address_Mode   : in     Address_Mode_Type     := Bits_16;
      Bytes_Per_Line : in     Interfaces.Unsigned_8 := 128)
   is begin
      Ada.Text_IO.Create (File.File, Mode, Name);
      File.Address_Mode   := Address_Mode;
      File.Bytes_Per_Line := Bytes_Per_Line;
      Initialize (File);
   end Create;

   procedure Open
     (File           :    out File_Type;
      Name           : in     String;
      Mode           : in     Mode_Type;
      Address_Mode   : in     Address_Mode_Type     := Bits_16;
      Bytes_Per_Line : in     Interfaces.Unsigned_8 := 128)
   is begin
      Ada.Text_IO.Open (File.File, Mode, Name);
      File.Address_Mode   := Address_Mode;
      File.Bytes_Per_Line := Bytes_Per_Line;
      Initialize (File);
   end Open;

   procedure Reset (File : in out File_Type; Mode : in Mode_Type)
   is begin
      Ada.Text_IO.Reset (File.File, Mode);
      Initialize (File);
   end Reset;

   procedure Close (File : in out File_Type)
   is
      use Ada.Text_IO;
   begin
      if not Is_Open (File.File) then
         return;
      end if;

      if Mode (File.File) = Out_File then
         Write_Data (File);
         Ada.Text_IO.Put_Line (File.File, ":00000001FF");
      end if;
      Ada.Text_IO.Close (File.File);
      Initialize (File);
   end Close;

   procedure Put
     (File    : in out File_Type;
      Byte    : in     Interfaces.Unsigned_8;
      Address : in     Interfaces.Unsigned_32)
   is
      use type Interfaces.Unsigned_16;
      use type Interfaces.Unsigned_32;
      Temp_Segment : Interfaces.Unsigned_16;
   begin
      case File.Address_Mode is
      when Bits_16 =>
         if Address > Max_16_Bit_Address then
            raise Address_Range_Error;
         end if;

      when Bits_20 =>
         if Address > Max_20_Bit_Address then
            raise Address_Range_Error;
         end if;

         Temp_Segment := Get_Segment_20 (Address);
         if Temp_Segment /= File.Current_Segment then
            --  Change segments
            Write_Data (File);
            Write_Segment_20 (File, Temp_Segment);
         end if;

      when Bits_32 =>
         Temp_Segment := Get_Segment_32 (Address);
         if Temp_Segment /= File.Current_Segment then
            --  Change segments
            Write_Data (File);
            Write_Extended_Linear_Address (File, Temp_Segment);
         end if;
      end case;

      if Address /= Get_Address (File) + 1 then
         --  Not contiguous; start new record
         Write_Data (File);
      end if;

      Add_Byte (File, Address, Byte);
   end Put;

   procedure Get
     (File    : in out File_Type;
      Byte    :    out Interfaces.Unsigned_8;
      Address :    out Interfaces.Unsigned_32)
   is
      use type Interfaces.Unsigned_8;
   begin
      if File.Buffer_Last = File.Buffer'First - 1 or File.Buffer_Last >= File.Bytes_Per_Line then
         File.Buffer_Last := File.Buffer'First - 1;
         loop
            --  Read actual data, not just address lines
            Read_Data (File);
            exit when File.Buffer_Last > File.Buffer'First;
         end loop;
         File.Buffer_Last := File.Buffer'First - 1;
      end if;

      File.Buffer_Last := File.Buffer_Last + 1;
      Byte             := File.Buffer (File.Buffer_Last);
      Address          := Get_Address (File);
   end Get;

end SAL.Intel_Hex_IO;
