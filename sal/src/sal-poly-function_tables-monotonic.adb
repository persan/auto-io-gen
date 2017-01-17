--  Abstract:
--
--  see spec
--
--  Copyright (C) 1999 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.

package body SAL.Poly.Function_Tables.Monotonic is

   procedure Initialize (Function_Table : in out Function_Table_Type)
   is
      Table : Table_Type renames Function_Table.Table.all;
   begin
      Function_Tables.Initialize (Function_Tables.Function_Table_Type (Function_Table));

      Function_Table.Increasing_Range :=
         Table (Table'First).Range_Value < Table (Table'First + 1).Range_Value;

      --  search for max, min
      Function_Table.Range_Max := Range_Type'First;
      Function_Table.Range_Min := Range_Type'Last;

      for I in Table'Range loop
         if I > Table'First + 1 then
            if Function_Table.Increasing_Range then
               if Table (I).Range_Value <= Table (I - 1).Range_Value then
                  raise Initialization_Error;
               end if;
            else
               if Table (I).Range_Value >= Table (I - 1).Range_Value  then
                  raise Initialization_Error;
               end if;
            end if;
         end if;

         if Table (I).Range_Value > Function_Table.Range_Max then
            Function_Table.Range_Max := Table (I).Range_Value;
         end if;
         if Table (I).Range_Value < Function_Table.Range_Min then
            Function_Table.Range_Min := Table (I).Range_Value;
         end if;
      end loop;
   end Initialize;

end SAL.Poly.Function_Tables.Monotonic;
