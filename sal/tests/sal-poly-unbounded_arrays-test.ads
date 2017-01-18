--  Abstract :
--
--  utilities for testing SAL.Poly.Unbounded_Arrays
--
--  Copyright (C) 1999, 2001, 2007 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with SAL.AUnit;
generic
   with function Image (Item : in Item_Node_Type) return String;
   with procedure Check_Item_Node (Label : in String; Computed : in Item_Node_Type; Expected : in Item_Node_Type);
   with procedure Check_Index (Label : in String; Computed : in Index_Type; Expected : in Index_Type);
package SAL.Poly.Unbounded_Arrays.Test is
   pragma Elaborate_Body; -- body depends on text_IO

   procedure Put (Item : in Array_Type);
   --  Put all the details to Standard_Output.

   type Base_Array_Type is array (Index_Type range <>) of Item_Node_Type;

   procedure Check is new SAL.AUnit.Gen_Check_Discrete (Growth_Type);

   procedure Check
     (Label         : in String;
      Computed      : in Array_Type;
      Growth        : in Growth_Type;
      Initial_Space : in Index_Type;
      Max_Space     : in Index_Type;
      First         : in Index_Type;
      Last          : in Index_Type;
      Base_First    : in Index_Type;
      Base_Last     : in Index_Type;
      Items         : in Base_Array_Type);

end SAL.Poly.Unbounded_Arrays.Test;
