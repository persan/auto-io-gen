--  Abstract :
--
--  Input/Output for Intex Hex format files. Start address not yet
--  supported.
--
--  References :
--
--  [1] http://www.interlog.com/~speff/usefulinfo/Hexfrmt.pdf
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

with Ada.IO_Exceptions;
with Ada.Text_IO;
with Interfaces;
package SAL.Intel_Hex_IO is

   Max_Bytes_Per_Line : constant := 255;

   type Address_Mode_Type is (Bits_16, Bits_20, Bits_32);
   --  16 bit uses standard Intel Hex addressing.
   --  20 bit uses segment extended addressing.
   --  32 bit uses linear extended addressing.

   for Address_Mode_Type use
      (Bits_16 => 0,
       Bits_20 => 1,
       Bits_32 => 2);

   Name_Error          : exception renames Ada.IO_Exceptions.Name_Error;
   End_Error           : exception renames Ada.IO_Exceptions.End_Error;
   Address_Range_Error : exception; --  an address is too large for current mode.
   Invalid_Format      : exception; --  input file has invalid format

   type File_Type is limited private;

   subtype Mode_Type is Ada.Text_IO.File_Mode range Ada.Text_IO.In_File .. Ada.Text_IO.Out_File;

   procedure Create
     (File           :    out File_Type;
      Name           : in     String;
      Mode           : in     Mode_Type;
      Address_Mode   : in     Address_Mode_Type     := Bits_16;
      Bytes_Per_Line : in     Interfaces.Unsigned_8 := 128);
   --  Create a new file.

   procedure Open
     (File           :    out File_Type;
      Name           : in     String;
      Mode           : in     Mode_Type;
      Address_Mode   : in     Address_Mode_Type     := Bits_16;
      Bytes_Per_Line : in     Interfaces.Unsigned_8 := 128);
   --  Open existing file. New data written to File will use
   --  Address_Mode, Bytes_Per_Line.

   procedure Reset (File : in out File_Type; Mode : in Mode_Type);
   --  Reset file to beginning.

   procedure Close (File : in out File_Type);

   procedure Put
     (File    : in out File_Type;
      Byte    : in     Interfaces.Unsigned_8;
      Address : in     Interfaces.Unsigned_32);

   procedure Get
     (File    : in out File_Type;
      Byte    :    out Interfaces.Unsigned_8;
      Address :    out Interfaces.Unsigned_32);

private

   type Byte_Array_Type is array (1 .. Interfaces.Unsigned_8 (Max_Bytes_Per_Line)) of Interfaces.Unsigned_8;

   type File_Type is record
      File          : Ada.Text_IO.File_Type;
      Address_Mode  : Address_Mode_Type;

      --  Current address in different formats; address of first byte
      --  of data on last line read or written.
      Current_Segment : Interfaces.Unsigned_16;
      Current_Offset  : Interfaces.Unsigned_16;

      --  'segment' is either the 20 bit segment, or the upper word of
      --  the linear address.

      Bytes_Per_Line : Interfaces.Unsigned_8;
      --  Data length for output, bytes read on input

      Buffer         : Byte_Array_Type;
      Buffer_Last    : Interfaces.Unsigned_8;
      --  Index of last byte in Buffer read or written by user
   end record;
end SAL.Intel_Hex_IO;
