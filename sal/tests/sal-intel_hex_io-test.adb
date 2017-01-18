--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Test_Cases.Registration;
with SAL.AUnit;
with SAL.Interfaces_More.AUnit;
package body SAL.Intel_Hex_IO.Test is

   type Expected_File_Type is record
      Current_Segment : Interfaces.Unsigned_16;
      Current_Offset  : Interfaces.Unsigned_16;
      Buffer_Last     : Interfaces.Unsigned_8;
   end record;

   procedure Check (Label : in String; File : in SAL.Intel_Hex_IO.File_Type; Expected : in Expected_File_Type)
   is
      use Interfaces_More.AUnit;
   begin
      Check (Label & ".Current_Segment", File.Current_Segment, Expected.Current_Segment);
      Check (Label & ".Current_Offset", File.Current_Offset, Expected.Current_Offset);
      Check (Label & ".Buffer_Last", File.Buffer_Last, Expected.Buffer_Last);
   end Check;

   procedure Check (Label : in String; File : in out Ada.Text_IO.File_Type; Expected : in String)
   is
      use Ada.Text_IO;

      Line      : String (1 .. 100); -- longer than needed to check correct length.
      Line_Last : Natural;
   begin
      Get_Line (File, Line, Line_Last);
      SAL.AUnit.Check (Label, Line (1 .. Line_Last), Expected);
   end Check;

   ----------
   --  Test procedures

   procedure Test_16 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Interfaces_More.AUnit, Interfaces;

      Hex_File_Name : constant String := "sal-intel_hex_io-test.hex";
      Hex_File      : SAL.Intel_Hex_IO.File_Type;
      Text_File     : Ada.Text_IO.File_Type;

      Byte    : Unsigned_8;
      Address : Unsigned_32;

   begin
      Create
        (Hex_File, Mode => Ada.Text_IO.Out_File, Name => Hex_File_Name, Bytes_Per_Line => 10, Address_Mode => Bits_16);
      for I in 1 .. 20 loop
         Put (Hex_File, Address => Unsigned_32 (I) - 1, Byte => Unsigned_8 (I));
      end loop;

      Check ("create 1", Hex_File, Expected => (Current_Segment => 0, Current_Offset => 10, Buffer_Last => 10));

      for I in 30 .. 49 loop
         Put (Hex_File, Address => Unsigned_32 (I), Byte => Unsigned_8 (I));
      end loop;

      Check ("create 2", Hex_File, Expected => (Current_Segment => 0, Current_Offset => 40, Buffer_Last => 10));

      Close (Hex_File);

      Ada.Text_IO.Open (Text_File, Ada.Text_IO.In_File, Hex_File_Name);
      Check ("Text 1", Text_File, ":0A0000000102030405060708090ABF");
      Check ("Text 2", Text_File, ":0A000A000B0C0D0E0F101112131451");
      Check ("Text 3", Text_File, ":0A001E001E1F20212223242526277F");
      Check ("Text 4", Text_File, ":0A00280028292A2B2C2D2E2F303111");
      Check ("Text 5", Text_File, ":00000001FF");
      Ada.Text_IO.Close (Text_File);

      Open (Hex_File, Mode => Ada.Text_IO.In_File, Name => Hex_File_Name);
      for I in 1 .. 20 loop
         Get (Hex_File, Byte, Address);
         Check ("Hex 1 .. 20" & Integer'Image (I) & ".byte", Byte, Unsigned_8 (I));
         Check ("Hex 1 .. 20" & Integer'Image (I) & ".address", Address, Unsigned_32 (I - 1));
      end loop;
      for I in 30 .. 49 loop
         Get (Hex_File, Byte, Address);
         Check ("Hex 30 .. 49" & Integer'Image (I) & ".byte", Byte, Unsigned_8 (I));
         Check ("Hex 30 .. 49" & Integer'Image (I) & ".address", Address, Unsigned_32 (I));
      end loop;
      Close (Hex_File);
   end Test_16;

   procedure Test_20 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Interfaces_More.AUnit, Interfaces;

      Hex_File_Name : constant String := "sal-intel_hex_io-test.hex";
      Hex_File      : SAL.Intel_Hex_IO.File_Type;
      Text_File     : Ada.Text_IO.File_Type;

      Byte    : Unsigned_8;
      Address : Unsigned_32;

   begin
      Create
        (Hex_File, Mode => Ada.Text_IO.Out_File, Name => Hex_File_Name, Bytes_Per_Line => 10, Address_Mode => Bits_20);

      for I in 1 .. 20 loop
         Put (Hex_File, Address => Unsigned_32 (I) - 1 + 16#2_0000#, Byte => Unsigned_8 (I));
      end loop;

      Check ("create 1", Hex_File, Expected => (Current_Segment => 16#2000#, Current_Offset => 10, Buffer_Last => 10));

      for I in 30 .. 49 loop
         Put (Hex_File, Address => Unsigned_32 (I) + 16#3_0000#, Byte => Unsigned_8 (I));
      end loop;

      Check ("create 2", Hex_File, Expected => (Current_Segment => 16#3000#, Current_Offset => 40, Buffer_Last => 10));

      Close (Hex_File);

      Ada.Text_IO.Open (Text_File, Ada.Text_IO.In_File, Hex_File_Name);
      Check ("Text 1", Text_File, ":020000022000DC");
      Check ("Text 2", Text_File, ":0A0000000102030405060708090ABF");
      Check ("Text 3", Text_File, ":0A000A000B0C0D0E0F101112131451");
      Check ("Text 4", Text_File, ":020000023000CC");
      Check ("Text 5", Text_File, ":0A001E001E1F20212223242526277F");
      Check ("Text 6", Text_File, ":0A00280028292A2B2C2D2E2F303111");
      Check ("Text 7", Text_File, ":00000001FF");
      Ada.Text_IO.Close (Text_File);

      Open (Hex_File, Mode => Ada.Text_IO.In_File, Name => Hex_File_Name);
      for I in 1 .. 20 loop
         Get (Hex_File, Byte, Address);
         Check ("Hex 1 .. 20" & Integer'Image (I) & ".byte", Byte, Unsigned_8 (I));
         Check ("Hex 1 .. 20" & Integer'Image (I) & ".address", Address, Unsigned_32 (I - 1) + 16#2_0000#);
      end loop;
      for I in 30 .. 49 loop
         Get (Hex_File, Byte, Address);
         Check ("Hex 30 .. 49" & Integer'Image (I) & ".byte", Byte, Unsigned_8 (I));
         Check ("Hex 30 .. 49" & Integer'Image (I) & ".address", Address, Unsigned_32 (I) + 16#3_0000#);
      end loop;
      Close (Hex_File);
   end Test_20;

   procedure Test_32 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Interfaces_More.AUnit, Interfaces;

      Hex_File_Name : constant String := "sal-intel_hex_io-test.hex";
      Hex_File      : SAL.Intel_Hex_IO.File_Type;
      Text_File     : Ada.Text_IO.File_Type;

      Byte    : Unsigned_8;
      Address : Unsigned_32;

   begin
      Create
        (Hex_File, Mode => Ada.Text_IO.Out_File, Name => Hex_File_Name, Bytes_Per_Line => 10, Address_Mode => Bits_32);

      for I in 1 .. 20 loop
         Put (Hex_File, Address => Unsigned_32 (I) - 1 + 16#2_0000#, Byte => Unsigned_8 (I));
      end loop;

      Check ("create 1", Hex_File, Expected => (Current_Segment => 16#2#, Current_Offset => 10, Buffer_Last => 10));

      for I in 30 .. 49 loop
         Put (Hex_File, Address => Unsigned_32 (I) + 16#3_0000#, Byte => Unsigned_8 (I));
      end loop;

      Check ("create 2", Hex_File, Expected => (Current_Segment => 16#3#, Current_Offset => 40, Buffer_Last => 10));

      Close (Hex_File);

      Ada.Text_IO.Open (Text_File, Ada.Text_IO.In_File, Hex_File_Name);
      Check ("Text 1", Text_File, ":020000040002F8");
      Check ("Text 2", Text_File, ":0A0000000102030405060708090ABF");
      Check ("Text 3", Text_File, ":0A000A000B0C0D0E0F101112131451");
      Check ("Text 4", Text_File, ":020000040003F7");
      Check ("Text 5", Text_File, ":0A001E001E1F20212223242526277F");
      Check ("Text 6", Text_File, ":0A00280028292A2B2C2D2E2F303111");
      Check ("Text 7", Text_File, ":00000001FF");
      Ada.Text_IO.Close (Text_File);

      Open (Hex_File, Mode => Ada.Text_IO.In_File, Name => Hex_File_Name);
      for I in 1 .. 20 loop
         Get (Hex_File, Byte, Address);
         Check ("Hex 1 .. 20" & Integer'Image (I) & ".byte", Byte, Unsigned_8 (I));
         Check ("Hex 1 .. 20" & Integer'Image (I) & ".address", Address, Unsigned_32 (I - 1) + 16#2_0000#);
      end loop;
      for I in 30 .. 49 loop
         Get (Hex_File, Byte, Address);
         Check ("Hex 30 .. 49" & Integer'Image (I) & ".byte", Byte, Unsigned_8 (I));
         Check ("Hex 30 .. 49" & Integer'Image (I) & ".address", Address, Unsigned_32 (I) + 16#3_0000#);
      end loop;
      Close (Hex_File);
   end Test_32;

   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("SAL.Intel_Hex_IO.Test");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_16'Access, "Test_16");
      Register_Routine (T, Test_20'Access, "Test_20");
      Register_Routine (T, Test_32'Access, "Test_32");
   end Register_Tests;

end SAL.Intel_Hex_IO.Test;
