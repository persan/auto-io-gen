--  Abstract :
--
--  Utilities for AUnit tests with Ada.Text_IO files
--
--  Separate from parent to allow parent to be Preelaborated.
--
--  Copyright (C) 2004 - 2008 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
package SAL.AUnit.Text_IO is
   pragma Elaborate_Body; --  Ada.Text_IO

   procedure Check (File : in Ada.Text_IO.File_Type; Expected : in String);
   --  Read a line from File, compare to Expected. Failure message
   --  label is file name and line number.
   --
   --  File must be Open.

   procedure Check_End (File : in Ada.Text_IO.File_Type);
   --  Check that End_Of_File (File) is True.

   type Line_Number_Array_Type is array (Positive range <>) of Ada.Text_IO.Count;

   procedure Check_Files
     (Label         : in String;
      Computed_Name : in String;
      Expected_Name : in String;
      Skip          : in Line_Number_Array_Type := (1 .. 0 => 1));
   --  Compare files named Computed_Name and Expected_Name
   --
   --  Skip lines in Skip; this allows for lines with time stamps that
   --  are not repeatable.

end SAL.AUnit.Text_IO;
