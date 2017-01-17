--  Abstract:
--
--  Root of generic utilities for encoding functions as tables.
--
--  Monotonically increasing Domain_Values allows binary search in Compute.
--
--  See child packages for various compute options.
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

with Ada.Finalization;
generic
   type Domain_Type is digits <>;
   type Range_Type is digits <>;
package SAL.Simple.Function_Tables is

   type Function_Point_Type is record
      Domain_Value : Domain_Type;
      Range_Value  : Range_Type;
      --  could be just 'domain' and 'range', but 'range' is a reserved word.
   end record;

   type Table_Type is array (Integer range <>) of Function_Point_Type;

   type Table_Access_Type is access constant Table_Type;

   type Function_Table_Type (Table : Table_Access_Type) is abstract new Ada.Finalization.Limited_Controlled
      with private;

   -----------
   --  Dispatching operations on Function_Table_Type.

   procedure Initialize (Function_Table : in out Function_Table_Type);
   --
   --  Raises Initialization_Error if Domain_Values are not
   --  monotonically increasing.

   function Compute
      (Function_Table : in Function_Table_Type;
       Domain_Value   : in Domain_Type)
      return Range_Type
      is abstract;
   --  Compute function result. See child packages for specific lookup
   --  methods.
   --
   --  Raises Domain_Error if Domain_Value not in table domain.

private
   type Function_Table_Type (Table : Table_Access_Type) is
      abstract new Ada.Finalization.Limited_Controlled with
   record
      Domain_Max : Domain_Type;
      Domain_Min : Domain_Type;
   end record;
end SAL.Simple.Function_Tables;
