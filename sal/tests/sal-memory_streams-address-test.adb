--  Abstract :
--
--  see spec
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

with Ada.Text_IO;
with Ada.Wide_Text_IO;
package body SAL.Memory_Streams.Address.Test
is
   Stream : aliased Stream_Type;

   procedure Run_Test
   is begin
      Ada.Text_IO.Put_Line ("testing SAL.Memory_Streams.Address");

      Test_Read :
      declare
         Raw_Memory : constant String := "ABCDEFGH";
         Read_String : String (1 .. 2);
      begin
         Ada.Text_IO.Put_Line ("Raw_Memory => " & Raw_Memory);
         Create (Stream, Raw_Memory (Raw_Memory'First)'Address);
         Ada.Text_IO.Put_Line ("read => ");
         for I in 1 .. 4 loop
            String'Read (Stream'Access, Read_String);
            Ada.Text_IO.Put ("""" & Read_String & """ ");
         end loop;
         Ada.Text_IO.New_Line (2);
      end Test_Read;

      Test_Terminated_String :
      declare
         Raw_Memory : constant String := "IJKLMNO" & ASCII.NUL;
      begin
         --  If we output the Nul, diff thinks this is a binary file
         Ada.Text_IO.Put_Line ("Raw_Memory => " & Raw_Memory (1 .. Raw_Memory'Last - 1));
         Create (Stream, Raw_Memory (Raw_Memory'First)'Address);
         declare
            Read_String : String (1 .. Null_Terminated_String_Length (Stream));
         begin
            Ada.Text_IO.Put_Line ("Length => " & Natural'Image (Read_String'Length));
            String'Read (Stream'Access, Read_String);
            Ada.Text_IO.Put ("Read => """ & Read_String & """ ");
         end;
         Ada.Text_IO.New_Line (2);
      end Test_Terminated_String;

      Test_Terminated_Wide_String :
      declare
         Raw_Memory : constant Wide_String := "IJKLMNO" & Wide_Character'Val (0);
      begin
         Ada.Wide_Text_IO.Put_Line ("Raw_Memory => " & Raw_Memory (1 .. Raw_Memory'Last - 1));
         Create (Stream, Raw_Memory (Raw_Memory'First)'Address);
         declare
            Read_String : Wide_String (1 .. Null_Terminated_Wide_String_Length (Stream));
         begin
            Ada.Text_IO.Put_Line ("Length => " & Natural'Image (Read_String'Length));
            Wide_String'Read (Stream'Access, Read_String);
            Ada.Wide_Text_IO.Put ("Read => """ & Read_String & """ ");
         end;
         Ada.Text_IO.New_Line (2);
      end Test_Terminated_Wide_String;

      Ada.Text_IO.Put_Line ("done");
   end Run_Test;

end SAL.Memory_Streams.Address.Test;
