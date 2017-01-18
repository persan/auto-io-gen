--  Abstract:
--
--  A storage pool that keeps track of allocation and deallocation,
--  and allows queries. Used to verify storage management in container
--  tests. NOT task safe!
--
--  Copyright (C) 1997 - 1999, 2002 Stephen Leake.  All Rights Reserved.
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

with System.Storage_Pools;
with System.Storage_Elements;
package Test_Storage_Pools is
   pragma Elaborate_Body; -- body depends on Ada.Text_IO;

   type String_Access_Constant_Type is access constant String;

   type Storage_Pool_Type
      (Pool_Size : System.Storage_Elements.Storage_Count;
       Name      : String_Access_Constant_Type) -- for debug messages
   is new System.Storage_Pools.Root_Storage_Pool with private;

   -----------
   --  Override Root_Storage_Pool operations

   procedure Allocate
      (Pool                     : in out Storage_Pool_Type;
       Storage_Address          :    out System.Address;
       Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
       Alignment                : in     System.Storage_Elements.Storage_Count);

   procedure Deallocate
      (Pool                     : in out Storage_Pool_Type;
       Storage_Address          : in     System.Address;
       Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
       Alignment                : in     System.Storage_Elements.Storage_Count);

   function Storage_Size (Pool : in Storage_Pool_Type) return System.Storage_Elements.Storage_Count;

   -----------
   --  New operations (alphabetical)

   function Allocate_Count (Pool : in Storage_Pool_Type) return Natural;
   --  Number of times Allocate has been called successfully.

   function Allocated_Elements (Pool : in Storage_Pool_Type) return Natural;
   --  Net allocated storage.

   procedure Check_Deallocated (Pool : in Storage_Pool_Type);
   --  If Allocated_Elements is not zero, print an error message and
   --  call Show_Storage.

   function Deallocate_Count (Pool : in Storage_Pool_Type) return Natural;
   --  Number of times Deallocate has been called.

   function Max_Allocated_Elements (Pool : in Storage_Pool_Type) return Natural;
   --  Max allocated storage, over lifetime of Pool.

   procedure Reset_Counts (Pool : in out Storage_Pool_Type);
   --  Reset Allocated and Deallocated counts to zero.

   procedure Set_Debug (Pool : in out Storage_Pool_Type; Debug : in Boolean);
   --  If Debug is True, Allocate, Deallocate, and Show_Storage print
   --  helpful messages to Standard_Output.

   procedure Show_Storage (Pool : in Storage_Pool_Type; Force_Debug : in Boolean := False);
   --  Print storage stats to Ada.Text_IO.Standard_Output, if
   --  Pool.Debug or Force_Debug is True.

private

   procedure Initialize (Pool : in out Storage_Pool_Type);

   type Block_Header_Type;
   type Block_Access_Type is access all Block_Header_Type;
   type Block_Header_Type is record
      Size : System.Storage_Elements.Storage_Count;
      Next : Block_Access_Type;
   end record;

   type Storage_Pool_Type
      (Pool_Size : System.Storage_Elements.Storage_Count;
       Name      : String_Access_Constant_Type)
   is new System.Storage_Pools.Root_Storage_Pool with
   record
      Debug                  : Boolean;
      Allocate_Count         : Natural;
      Deallocate_Count       : Natural;
      Allocated_Elements     : Natural;
      Max_Allocated_Elements : Natural;
      First_Free             : Block_Access_Type;
      Storage                : System.Storage_Elements.Storage_Array (1 .. Pool_Size);
      --  The first few elements of each free block contain the block
      --  header. Small requested blocks are padded up to at least the
      --  block header size. All blocks have alignment 8, to keep
      --  things simple.
   end record;

end Test_Storage_Pools;
