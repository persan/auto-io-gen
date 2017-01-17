--  Abstract:
--
--  First order lookup for monotonic (and thus invertable) tables
--
--  Copyright (C) 1999, 2003, 2005 Stephen Leake.  All Rights Reserved.
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

generic
package SAL.Poly.Function_Tables.Monotonic.First_Order is

   type Function_Table_Type is new Function_Tables.Monotonic.Function_Table_Type with null record;

   ---------------
   --  Override Function_Table_Type operations

   overriding function Compute
      (Function_Table : in Function_Table_Type;
       Domain_Value   : in Domain_Type)
      return Range_Type;

   overriding function Compute_Inverse
      (Function_Table : in Function_Table_Type;
       Range_Value    : in Range_Type)
      return Domain_Type;
   --  Compute inverse function result, using a first order interpolation.
   --
   --  Raises Range_Error if Range_Value not in table range.
   --  Raises Initialization_Error if Function_Table not Initialized.

end SAL.Poly.Function_Tables.Monotonic.First_Order;
