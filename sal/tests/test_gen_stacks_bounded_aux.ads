--  Abstract:
--
--  Types for testing Sal.Gen.Stacks.Bounded_Limited and
--  Bounded_Nonlimited.
--
--  Copyright (C) 1999, 2000, 2002, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Finalization;
with SAL.Gen.Stacks.Bounded_Nonlimited;
with Test_Storage_Pools;
package Test_Gen_Stacks_Bounded_Aux is
   pragma Elaborate_Body; -- Body depends on Ada.Text_IO.

   -----------
   --  Integer_Stacks

   package Integer_Stacks is new SAL.Gen.Stacks.Bounded_Nonlimited
     (Item_Type => Integer,
      Null_Item => 0);

   -----------
   --  Name_Stacks

   String_Storage_Pool_Name : aliased constant String := "String_Storage_Pool";
   String_Storage_Pool : Test_Storage_Pools.Storage_Pool_Type (1000, String_Storage_Pool_Name'Access);

   type String_Access_Type is access String;
   for String_Access_Type'Storage_Pool use String_Storage_Pool;

   type Name_Type is private;

   Null_Name : constant Name_Type;

   Debug : Boolean := False; -- Set True for debug output from subprograms.

   function Create (Name : in String) return Name_Type;

   function Name (Item : in Name_Type) return String;

   procedure Free_Name (Item : in out Name_Type);

   --  Can't instantiate Name_Stacks here, because Name_Type isn't
   --  frozen yet.
private

   type Name_Type is new Ada.Finalization.Controlled with record
      Name : String_Access_Type := null;
   end record;

   procedure Adjust (Item : in out Name_Type);
   --  Deep copy.

   procedure Finalize (Item : in out Name_Type);
   --  Print Name on Standard_Output, free Name.

   Null_Name : constant Name_Type := (Ada.Finalization.Controlled with Name => null);

end Test_Gen_Stacks_Bounded_Aux;
