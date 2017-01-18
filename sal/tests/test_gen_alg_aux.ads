--  Abstract
--
--  Root of declarations to help test SAL.Gen.Alg.*.
--
--  See the child packages for instantiations of all SAL containers
--  with representative Item_Types.
--
--  The actual algorithms are instantiated in the various test
--  procedures test_gen_alg_*.adb. That way, the output files don't
--  change when a new algorithm is added; only when a new container is
--  added.
--
--  Copyright (C) 2000, 2002 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Test_Storage_Pools;
package Test_Gen_Alg_Aux is
--   pragma Elaborate_Body; -- Test_Storage_Pools is, but we have no body

   Node_Storage_Pool_Name : aliased constant String := "Node_Storage_Pool";
   Node_Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, Node_Storage_Pool_Name'Access);

end Test_Gen_Alg_Aux;
