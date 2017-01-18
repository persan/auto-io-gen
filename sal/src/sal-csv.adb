--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
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

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
package body SAL.CSV is

   procedure Free is new Ada.Unchecked_Deallocation (String, Ada.Strings.Unbounded.String_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Positive_Array_Integer_Type, Positive_Array_Integer_Access_Type);

   overriding procedure Finalize (File : in out File_Type)
   is
      use type Ada.Strings.Unbounded.String_Access;
   begin
      if File.Line /= null then
         Ada.Text_IO.Close (File.File);
         Free (File.Line);
         Free (File.Commas);
      end if;
   end Finalize;

   procedure Open
     (File         : in out File_Type;
      Name         : in     String;
      Max_Row_Size : in     Integer;
      Columns      : in     Integer   := 0)
   is
      use Ada.Text_IO;
      Comma_Index : Integer := 0;
      Comma_Count : Integer := 0;
   begin
      Open (File.File, In_File, Name);
      File.Line := new String (1 .. Max_Row_Size + 1); -- +1 so we can check for longer lines.

      if Columns = 0 then
         --  Read first line to determine how many columns there are.
         Get_Line (File.File, File.Line.all, File.Last);

         loop
            Comma_Index := Ada.Strings.Fixed.Index (Source => File.Line (Comma_Index + 1 .. File.Last), Pattern => ",");
            exit when Comma_Index = 0;

            Comma_Count := Comma_Count + 1;
         end loop;

         Reset (File.File);

      else
         Comma_Count := Columns - 1;
      end if;

      File.Commas := new Positive_Array_Integer_Type (1 .. Comma_Count);

      Next_Row (File);
   end Open;

   function Columns (File : in File_Type) return Integer
   is begin
      return File.Commas'Last + 1;
   end Columns;

   function End_Of_File (File : in File_Type) return Boolean
   is begin
      return Ada.Text_IO.End_Of_File (File.File);
   end End_Of_File;

   procedure Next_Row (File : in out File_Type)
   is
      use Ada.Text_IO;
      Comma_Index : Integer := 0;
   begin
      if End_Of_File (File.File) then
         raise End_Error;
      end if;

      Get_Line (File.File, File.Line.all, File.Last);

      if File.Line'Last = File.Last then
         raise Initialization_Error with "row" & Count'Image (Line (File.File)) & "longer than Max_Row_Size";
      end if;

      for I in File.Commas'Range loop
         Comma_Index     := Ada.Strings.Fixed.Index
           (Source => File.Line (Comma_Index + 1 .. File.Last), Pattern => ",");
         File.Commas (I) := Comma_Index;
      end loop;
   end Next_Row;

   function Read (File : in File_Type; Column : in Integer) return String
   is
      First : Integer;
      Last  : Integer;
   begin
      if Column < 1 or Column > File.Commas'Last + 1 then
         raise Ada.Text_IO.Use_Error with "invalid column" & Integer'Image (Column);
      end if;

      if Column = 1 then
         First := File.Line'First;
         Last  := File.Commas (1) - 1;
      elsif Column = File.Commas'Last + 1 then
         First := File.Commas (File.Commas'Last) + 1;
         Last  := File.Last;
      else
         First := File.Commas (Column - 1) + 1;
         Last  := File.Commas (Column) - 1;
      end if;

      return File.Line (First .. Last);
   end Read;

end SAL.CSV;
