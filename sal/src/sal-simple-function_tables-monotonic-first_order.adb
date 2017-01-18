--  Abstract:
--
--  see spec
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR
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

with SAL.Simple.Searches.Binary;
package body SAL.Simple.Function_Tables.Monotonic.First_Order is

   function Domain_Less_Equal
      (List  : in Function_Table_Type;
       Left  : in Integer;
       Right : in Domain_Type)
       return Boolean;
   pragma Inline (Domain_Less_Equal);
   --  Ignore warning about not dispatching because not in package
   --  spec. Changing to class-wide would avoid warning, but then it
   --  wouldn't match generic param for Domain_Searches.

   function Domain_Less_Equal
      (List  : in Function_Table_Type;
       Left  : in Integer;
       Right : in Domain_Type)
       return Boolean
   is begin
      return List.Table (Left).Domain_Value <= Right;
   end Domain_Less_Equal;

   function Table_First (List : in Function_Table_Type) return Integer;
   pragma Inline (Table_First);

   function Table_First (List : in Function_Table_Type) return Integer
   is begin
      return List.Table.all'First;
   end Table_First;

   function Table_Last (List : in Function_Table_Type) return Integer;
   pragma Inline (Table_Last);

   function Table_Last (List : in Function_Table_Type) return Integer
   is begin
      return List.Table.all'Last;
   end Table_Last;

   package Domain_Searches is new SAL.Simple.Searches.Binary
      (Item_Type => Domain_Type,
       List_Type => Function_Table_Type,
       Index_Type => Integer,
       First => Table_First,
       Last => Table_Last,
       Less_Equal => Domain_Less_Equal);

   function Range_Less_Equal
      (List  : in Function_Table_Type;
       Left  : in Integer;
       Right : in Range_Type)
       return Boolean
   is begin
      if List.Increasing_Range then
         return List.Table (Left).Range_Value <= Right;
      else
         return List.Table (Left).Range_Value >= Right;
      end if;
   end Range_Less_Equal;

   package Range_Searches is new SAL.Simple.Searches.Binary
      (Item_Type => Range_Type,
       List_Type => Function_Table_Type,
       Index_Type => Integer,
       First => Table_First,
       Last => Table_Last,
       Less_Equal => Range_Less_Equal);

   overriding function Compute
      (Function_Table : in Function_Table_Type;
       Domain_Value   : in Domain_Type)
       return Range_Type
   is
      Lower : Integer;
   begin
      if Function_Table.Table = null then
         raise Initialization_Error;
      elsif Domain_Value > Function_Table.Domain_Max or
         Domain_Value < Function_Table.Domain_Min
      then
         raise Domain_Error;
      end if;

      Lower := Domain_Searches.Search_Less_Equal (Function_Table, Domain_Value);

      if Lower = Function_Table.Table'Last then
         raise Domain_Error;
      end if;

      declare
         Y1 : Range_Type renames Function_Table.Table (Lower).Range_Value;
         Y2 : Range_Type renames Function_Table.Table (Lower + 1).Range_Value;
         X1 : Domain_Type renames Function_Table.Table (Lower).Domain_Value;
         X2 : Domain_Type renames Function_Table.Table (Lower + 1).Domain_Value;
      begin
         return Y1 + ((Y2 - Y1) / Range_Type (X2 - X1)) * Range_Type (Domain_Value - X1);
      end;
   end Compute;

   overriding function Compute_Inverse
      (Function_Table : in Function_Table_Type;
       Range_Value    : in Range_Type)
       return Domain_Type
   is
      Lower : Integer;
   begin
      if Function_Table.Table = null then
         raise Initialization_Error;
      elsif Range_Value > Function_Table.Range_Max or
         Range_Value < Function_Table.Range_Min
      then
         raise Range_Error;
      end if;

      Lower := Range_Searches.Search_Less_Equal (Function_Table, Range_Value);

      if Lower = Function_Table.Table.all'Last then
         raise Range_Error;
      end if;

      declare
         Y1 : Domain_Type renames Function_Table.Table (Lower).Domain_Value;
         Y2 : Domain_Type renames Function_Table.Table (Lower + 1).Domain_Value;
         X1 : Range_Type renames Function_Table.Table (Lower).Range_Value;
         X2 : Range_Type renames Function_Table.Table (Lower + 1).Range_Value;
      begin
         return Y1 + ((Y2 - Y1) / Domain_Type (X2 - X1)) * Domain_Type (Range_Value - X1);
      end;

   end Compute_Inverse;

end SAL.Simple.Function_Tables.Monotonic.First_Order;
