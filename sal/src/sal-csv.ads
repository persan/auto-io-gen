--  Abstract :
--
--  Support for reading CSV (comma separated value) files.
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

with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Text_IO;
package SAL.CSV is
   pragma Elaborate_Body;
   --  Almost any package that does file IO will not be preelaborable.

   type File_Type is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (File : in out File_Type);
   --  close file, free data structures

   procedure Open
     (File         : in out File_Type;
      Name         : in     String;
      Max_Row_Size : in     Integer;
      Columns      : in     Integer   := 0);
   --  If Columns is 0, reads first row of the file to determine how
   --  many columns there are.
   --
   --  Raises Initialization_Error if first row is longer than
   --  Max_Line_Size.

   function Columns (File : in File_Type) return Integer;

   function End_Of_File (File : in File_Type) return Boolean;
   --  True if internal file is at end of file; last row has been read
   --  by Next_Row.

   procedure Next_Row (File : in out File_Type);
   --  After Open, read will read first row (often containing the row
   --  header names). Next_Row advances internal data so read will
   --  read the next row.
   --
   --  Raises Ada.Text_IO.End_Error if there is no next row.
   --
   --  Raises Initialization_Error if new row is longer than
   --  Max_Line_Size specified in Open.

   function Read (File : in File_Type; Column : in Integer) return String;
   --  Return the contents of Column as a string.
   --
   --  Raises Use_Error if Column is not in File.

private
   type Positive_Array_Integer_Type is array (Positive range <>) of Integer;
   type Positive_Array_Integer_Access_Type is access Positive_Array_Integer_Type;

   type File_Type is new Ada.Finalization.Limited_Controlled with record
      File   : Ada.Text_IO.File_Type;
      Line   : Ada.Strings.Unbounded.String_Access;
      Last   : Integer;
      Commas : Positive_Array_Integer_Access_Type;
   end record;

end SAL.CSV;
