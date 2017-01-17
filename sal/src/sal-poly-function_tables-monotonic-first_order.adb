--  Abstract:
--
--  see spec
--
--  Copyright (C) 1999, 2000, 2003, 2005 Stephen Leake.  All Rights Reserved.
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

with SAL.Aux.Enum_Iterators;
with SAL.Gen.Alg.Find_Binary;
package body SAL.Poly.Function_Tables.Monotonic.First_Order is

   package Table_Iterators is new SAL.Aux.Enum_Iterators
      (Item_Node_Type => Function_Point_Type,
       Index_Type     => Integer,
       Array_Type     => Table_Type,
       Container_Type => Table_Access_Type);

   --  We actually need Container_Type to be Function_Table_Type, not
   --  Table_Type, so we need these wrappers. Ignore warnings from
   --  GNAT about not dispatching.

   function Function_Table_First (Container : in Function_Table_Type) return Table_Iterators.Iterator_Type;
   function Function_Table_Last  (Container : in Function_Table_Type) return Table_Iterators.Iterator_Type;
   function Function_Table_None  (Container : in Function_Table_Type) return Table_Iterators.Iterator_Type;
   pragma Inline
     (Function_Table_First,
      Function_Table_Last,
      Function_Table_None);

   --  We also need these auxiliary functions

   function Function_Table_First (Container : in Function_Table_Type) return Table_Iterators.Iterator_Type
   is begin
      return Table_Iterators.First (Container.Table);
   end Function_Table_First;

   function Function_Table_Last (Container : in Function_Table_Type) return Table_Iterators.Iterator_Type
   is begin
      return Table_Iterators.Last (Container.Table);
   end Function_Table_Last;

   function Function_Table_None (Container : in Function_Table_Type) return Table_Iterators.Iterator_Type
   is begin
      return Table_Iterators.None (Container.Table);
   end Function_Table_None;

   package Table_Algorithms is new SAL.Gen.Alg
      (Item_Node_Type => Function_Point_Type,
       Container_Type => Function_Table_Type,
       Iterator_Type  => Table_Iterators.Iterator_Type,
       First          => Function_Table_First,
       Last           => Function_Table_Last,
       None           => Function_Table_None,
       Next_Function  => Table_Iterators.Next,
       Next_Procedure => Table_Iterators.Next,
       Current        => Table_Iterators.Current,
       Is_Null        => Table_Iterators.Is_Null);

   function Domain_Equal
      (Left  : in Function_Point_Type;
       Right : in Function_Point_Type)
       return Boolean
   is begin
      return Left.Domain_Value = Right.Domain_Value;
   end Domain_Equal;

   function Domain_Less_Equal
      (Left  : in Function_Point_Type;
       Right : in Function_Point_Type)
       return Boolean
   is begin
      return Left.Domain_Value <= Right.Domain_Value;
   end Domain_Less_Equal;

   function Domain_Greater_Equal
      (Left  : in Function_Point_Type;
       Right : in Function_Point_Type)
       return Boolean
   is begin
      return Left.Domain_Value >= Right.Domain_Value;
   end Domain_Greater_Equal;

   use Table_Iterators;

   package Domain_Searches is new Table_Algorithms.Find_Binary
      (Key_Type                  => Function_Point_Type,
       Is_Equal_Node_Key         => Domain_Equal,
       Is_Greater_Equal_Node_Key => Domain_Greater_Equal,
       Is_Less_Equal_Node_Key    => Domain_Less_Equal);

   function Range_Equal
      (Left      : in Function_Point_Type;
       Right     : in Function_Point_Type)
       return Boolean
   is begin
      return Left.Range_Value = Right.Range_Value;
   end Range_Equal;

   function Increasing_Range_Less_Equal
      (Left      : in Function_Point_Type;
       Right     : in Function_Point_Type)
       return Boolean
   is begin
      return Left.Range_Value <= Right.Range_Value;
   end Increasing_Range_Less_Equal;

   function Increasing_Range_Greater_Equal
      (Left      : in Function_Point_Type;
       Right     : in Function_Point_Type)
       return Boolean
   is begin
      return Left.Range_Value >= Right.Range_Value;
   end Increasing_Range_Greater_Equal;

   package Increasing_Range_Searches is new Table_Algorithms.Find_Binary
      (Key_Type                  => Function_Point_Type,
       Is_Equal_Node_Key         => Range_Equal,
       Is_Greater_Equal_Node_Key => Increasing_Range_Greater_Equal,
       Is_Less_Equal_Node_Key    => Increasing_Range_Less_Equal);

   function Decreasing_Range_Greater_Equal
      (Left      : in Function_Point_Type;
       Right     : in Function_Point_Type)
       return Boolean
   is begin
      return Left.Range_Value <= Right.Range_Value;
   end Decreasing_Range_Greater_Equal;

   function Decreasing_Range_Less_Equal
      (Left      : in Function_Point_Type;
       Right     : in Function_Point_Type)
       return Boolean
   is begin
      return Left.Range_Value >= Right.Range_Value;
   end Decreasing_Range_Less_Equal;

   package Decreasing_Range_Searches is new Table_Algorithms.Find_Binary
      (Key_Type                  => Function_Point_Type,
       Is_Equal_Node_Key         => Range_Equal,
       Is_Greater_Equal_Node_Key => Decreasing_Range_Greater_Equal,
       Is_Less_Equal_Node_Key    => Decreasing_Range_Less_Equal);

   overriding function Compute
      (Function_Table : in Function_Table_Type;
       Domain_Value   : in Domain_Type)
       return Range_Type
   is begin
      if Function_Table.Table = null then
         raise Initialization_Error;
      elsif Domain_Value > Function_Table.Domain_Max or
         Domain_Value < Function_Table.Domain_Min
      then
         raise Domain_Error;
      end if;

      declare
         Lower : Iterator_Type :=
            Domain_Searches.Find_Less_Equal (Function_Table,
                                             (Domain_Value => Domain_Value,
                                              Range_Value => 0.0));
         Y1 : Range_Type renames Current (Lower).Range_Value;
         X1 : Domain_Type renames Current (Lower).Domain_Value;
         Y2 : Range_Type;
         X2 : Domain_Type;
      begin
         if X1 = Domain_Value then
            --  no interpolation needed
            return Y1;
         else
            --  we know Lower is not at last table entry, since X1 /= Domain_Value
            Next (Lower);
            Y2 := Current (Lower).Range_Value;
            X2 := Current (Lower).Domain_Value;
            return Y1 + ((Y2 - Y1) / Range_Type (X2 - X1)) * Range_Type (Domain_Value - X1);
         end if;
      end;
   end Compute;

   overriding function Compute_Inverse
      (Function_Table : in Function_Table_Type;
       Range_Value    : in Range_Type)
       return Domain_Type
   is begin
      if Function_Table.Table = null then
         raise Initialization_Error;
      elsif Range_Value > Function_Table.Range_Max or
         Range_Value < Function_Table.Range_Min
      then
         raise Range_Error;
      end if;

      declare
         Lower : Iterator_Type (Function_Table.Table);
         Y1 : Domain_Type;
         Y2 : Domain_Type;
         X1 : Range_Type;
         X2 : Range_Type;
      begin
         if Function_Table.Increasing_Range then
            Lower := Increasing_Range_Searches.Find_Less_Equal
               (Function_Table,
                (Range_Value => Range_Value,
                 Domain_Value => 0.0));
         else
            Lower := Decreasing_Range_Searches.Find_Less_Equal
               (Function_Table,
                (Range_Value => Range_Value,
                 Domain_Value => 0.0));
         end if;
         Y1 := Current (Lower).Domain_Value;
         X1 := Current (Lower).Range_Value;
         if X1 = Range_Value then
            return Y1;
         else
            Next (Lower);
            Y2 := Current (Lower).Domain_Value;
            X2 := Current (Lower).Range_Value;
            return Y1 + ((Y2 - Y1) / Domain_Type (X2 - X1)) * Domain_Type (Range_Value - X1);
         end if;
      exception
      when Domain_Error =>
         raise Range_Error;
      end;

   end Compute_Inverse;

end SAL.Poly.Function_Tables.Monotonic.First_Order;
