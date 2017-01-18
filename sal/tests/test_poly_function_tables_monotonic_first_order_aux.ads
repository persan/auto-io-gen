--  Abstract:
--
--  Like the name says.
--
--  Copyright (C) 1999, 2000, 2002 Stephen Leake.  All Rights Reserved.
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

with Ada.Unchecked_Deallocation;
with SAL.Poly.Function_Tables.Monotonic.First_Order;
package Test_Poly_Function_Tables_Monotonic_First_Order_Aux is
   package Float_Function_Tables is new SAL.Poly.Function_Tables (Float, Float);
   package Monotonic_Float_Function_Tables is new Float_Function_Tables.Monotonic;
   package First_Order_Monotonic_Float_Function_Tables is new Monotonic_Float_Function_Tables.First_Order;

   --  need library level table access object to satisfy access rules
   type Table_Access_Type is access all Float_Function_Tables.Table_Type;
   procedure Free is new Ada.Unchecked_Deallocation (Float_Function_Tables.Table_Type, Table_Access_Type);

   Table : Table_Access_Type;

end Test_Poly_Function_Tables_Monotonic_First_Order_Aux;
