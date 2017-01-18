--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004 - 2008 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with Ada.Exceptions;
with Ada.Strings.Fixed;
package body SAL.AUnit.Text_IO is

   procedure Check (File : in Ada.Text_IO.File_Type; Expected : in String)
   is
      Read_Line : String (1 .. 400);
      Last      : Natural;
   begin
      Ada.Text_IO.Get_Line (File, Read_Line, Last);
      Check
        (Label    => Ada.Text_IO.Name (File) & Ada.Text_IO.Count'Image (Ada.Text_IO.Line (File)),
         Computed => Read_Line (1 .. Last),
         Expected => Expected);
   exception
   when Ada.Text_IO.End_Error =>
      Standard.AUnit.Assertions.Assert (False, "got End_Error; expecting " & Expected);
   end Check;

   procedure Check_End (File : in Ada.Text_IO.File_Type)
   is begin
      Standard.AUnit.Assertions.Assert (Ada.Text_IO.End_Of_File (File), Ada.Text_IO.Name (File) & " not at end");
   end Check_End;

   function Not_In (Line : in Ada.Text_IO.Count; Skip : in Line_Number_Array_Type) return Boolean
   is
      use type Ada.Text_IO.Count;
   begin
      for I in Skip'Range loop
         if Line = Skip (I) then
            return False;
         end if;
      end loop;
      return True;
   end Not_In;

   procedure Check_Files
     (Label         : in String;
      Computed_Name : in String;
      Expected_Name : in String;
      Skip          : in Line_Number_Array_Type := (1 .. 0 => 1))
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      use Standard.AUnit.Assertions;
      Computed : File_Type;
      Expected : File_Type;
   begin
      begin
         Open (Computed, In_File, Computed_Name);
      exception
      when E : others =>
         Assert
           (False,
            Label & " file '" & Computed_Name & "' cannot be opened: " & Ada.Exceptions.Exception_Name (E));
      end;

      begin
         Open (Expected, In_File, Expected_Name);
      exception
      when E : others =>
         Assert
           (False,
            Label & " file '" & Expected_Name & "' cannot be opened: " & Ada.Exceptions.Exception_Name (E));
      end;

      begin
         while not End_Of_File (Expected) loop
            declare
               Computed_Line : constant String := Get_Line (Computed);
               Expected_Line : constant String := Get_Line (Expected);
            begin
               --  Get_Line advances the line counter beyond the line of interest
               if Not_In (Line (Computed) - 1, Skip) then
                  Check (Computed_Name & ":" & Trim (Positive_Count'Image (Line (Expected)), Both),
                         Computed_Line,
                         Expected_Line);
               end if;
            end;
         end loop;
         Assert (End_Of_File (Computed), Label & " Computed file longer than Expected file");

         Close (Computed);
         Close (Expected);
      exception
      when Standard.AUnit.Assertions.Assertion_Error =>
         Close (Computed);
         Close (Expected);
         raise;
      end;
   end Check_Files;

end SAL.AUnit.Text_IO;
