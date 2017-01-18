--  Abstract
--
--  Instantiations for timing
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Unbounded;
with SAL.Poly.Lists.Double;
with SAL.Aux.Definite_Private_Items;
with System.Storage_Pools;
package Time_Poly_Lists_Double_Aux is

   package Integer_Lists_Aux is new SAL.Aux.Definite_Private_Items (Integer);
   package Integer_Lists is new SAL.Poly.Lists.Double
     (Item_Type         => Integer,
      Item_Node_Type    => Integer,
      To_Item_Node      => Integer_Lists_Aux.To_Item_Node,
      Free_Item         => Integer_Lists_Aux.Free_Item,
      Copy              => Integer_Lists_Aux.Copy,
      Node_Storage_Pool =>
        System.Storage_Pools.Root_Storage_Pool'Class (Ada.Strings.Unbounded.String_Access'Storage_Pool));

end Time_Poly_Lists_Double_Aux;
