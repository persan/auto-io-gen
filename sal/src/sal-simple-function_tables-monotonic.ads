--  Abstract:
--
--  Root of monotonic (and thus invertable) tables.
--
--  See child packages for different orders of interpolation.
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

generic
package SAL.Simple.Function_Tables.Monotonic is

   type Function_Table_Type is abstract new Function_Tables.Function_Table_Type with private;

   ---------------
   --  Override Function_Table_Type operations

   procedure Initialize (Function_Table : in out Function_Table_Type);
   --  Add restriction on Range_Values.
   --
   --  Raises Initialization_Error if Range_Values are not
   --  monotonically increasing or decreasing.

   ------------
   --  New operations

   function Compute_Inverse
      (Function_Table : in Function_Table_Type;
       Range_Value    : in Range_Type)
      return Domain_Type
      is abstract;
   --  Compute inverse function result.
   --
   --  Raises Range_Error if Range_Value not in table range.
   --  Raises Initialization_Error if Function_Table not Initialized.
private

   type Function_Table_Type is abstract new Function_Tables.Function_Table_Type with record
      Increasing_Range : Boolean;
      Range_Max        : Range_Type;
      Range_Min        : Range_Type;
   end record;

end SAL.Simple.Function_Tables.Monotonic;
