--  Abstract :
--
--  Utilities for AUnit tests, including checks for types in Standard.
--
--  Copyright (C) 2004 - 2007 Stephen Leake.  All Rights Reserved.
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

package SAL.AUnit is

   generic
      type Item_Type (<>) is limited private;
      type Item_Access_Type is access all Item_Type;
   procedure Gen_Check_Access
     (Label    : in String;
      Computed : in Item_Access_Type;
      Expected : in Item_Access_Type);

   generic
      type Item_Type is (<>);
   procedure Gen_Check_Discrete
     (Label    : in String;
      Computed : in Item_Type;
      Expected : in Item_Type);

   generic
      type Item_Type is delta <>;
   procedure Gen_Check_Fixed
     (Label     : in String;
      Computed  : in Item_Type;
      Expected  : in Item_Type;
      Tolerance : in Item_Type := 0.0);

   generic
      type Item_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type) of Item_Type;
      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Item
        (Label    : in String;
         Computed : in Item_Type;
         Expected : in Item_Type);
   procedure Gen_Check_Array
     (Label    : in String;
      Computed : in Array_Type;
      Expected : in Array_Type);

   generic
      type Item_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type) of Item_Type;
      type Tolerance_Type is private;

      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Item
        (Label     : in String;
         Computed  : in Item_Type;
         Expected  : in Item_Type;
         Tolerance : in Tolerance_Type);

      Default_Tolerance : in out Tolerance_Type;

   procedure Gen_Check_Array_Tolerance
     (Label     : in String;
      Computed  : in Array_Type;
      Expected  : in Array_Type;
      Tolerance : in Tolerance_Type := Default_Tolerance);

   generic
      type Item_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Item_Type;
      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Item
        (Label    : in String;
         Computed : in Item_Type;
         Expected : in Item_Type);
   procedure Gen_Check_Unconstrained_Array
     (Label          : in String;
      Computed       : in Array_Type;
      Expected       : in Array_Type;
      Strict_Indices : in Boolean    := True);

   generic
      type Item_Type is private;
      Zero_Item : in Item_Type;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Item_Type;
      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Item
        (Label     : in String;
         Computed  : in Item_Type;
         Expected  : in Item_Type;
         Tolerance : in Item_Type);
   procedure Gen_Check_Unconstrained_Array_Tolerance
     (Label          : in String;
      Computed       : in Array_Type;
      Expected       : in Array_Type;
      Tolerance      : in Item_Type  := Zero_Item;
      Strict_Indices : in Boolean    := True);

   procedure Check
     (Label    : in String;
      Computed : in Integer;
      Expected : in Integer);

   procedure Check
     (Label    : in String;
      Computed : in Boolean;
      Expected : in Boolean);

   procedure Check
     (Label    : in String;
      Computed : in String;
      Expected : in String);

   procedure Check
     (Label     : in String;
      Computed  : in Duration;
      Expected  : in Duration;
      Tolerance : in Duration := 0.0);

end SAL.AUnit;
