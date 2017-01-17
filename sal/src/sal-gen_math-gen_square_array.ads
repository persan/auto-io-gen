--  Abstract :
--
--  Operations on square matrices with the same indices on rows and
--  columns, and real elements.
--
--  Design :
--
--  The standard naming convention would be Index_Array_Float_Type for
--  Row_Type, and Index_Array_Iaf_Type for Array_Type. We do not use
--  the standard naming convention here, because the gain in precision
--  is not needed, since there is only one row type and one vector
--  type.
--
--  Copyright (C) 2001, 2004 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

generic
   type Index_Type is (<>);
   type Row_Type is array (Index_Type) of Real_Type;
   type Array_Type is array (Index_Type) of Row_Type;
package SAL.Gen_Math.Gen_Square_Array is
   pragma Pure;

   function Identity return Array_Type;
   function "-" (Right : in Array_Type) return Array_Type;

   function "-" (Left, Right : in Array_Type) return Array_Type;
   function "+" (Left, Right : in Array_Type) return Array_Type;
   function "*" (Left, Right : in Array_Type) return Array_Type;

   function "*" (Left : in Real_Type; Right : in Array_Type) return Array_Type;
   function "*" (Left : in Array_Type; Right : in Row_Type) return Row_Type;

   function Square_Right (Right : in Array_Type) return Array_Type;
   --  Return Right * Transpose (Right)

   function Square_Left (Right : in Array_Type) return Array_Type;
   --  Return Transpose (Right) * Right

   function Times_Diag (Left : in Array_Type; Right : in Row_Type) return Array_Type;
   --  Return Left * Diag (Right)

   function Diag_Times (Left : in Row_Type; Right : in Array_Type) return Array_Type;
   --  Return Diag (Left) * Right

   function Transpose_Times (Left, Right : in Array_Type) return Array_Type;
   --  Return Transpose (Left) * Right

   function Times_Transpose (Left, Right : in Array_Type) return Array_Type;
   --  Return Left * Transpose (Right)

   function Transpose_Times (Left : in Array_Type; Right : in Row_Type) return Row_Type;
   --  Return Transpose (Left) * Right

end SAL.Gen_Math.Gen_Square_Array;
