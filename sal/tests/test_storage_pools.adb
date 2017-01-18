--  Abstract:
--
--  see spec
--
--  Copyright (C) 1997 - 1999, 2002, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
with System.Address_To_Access_Conversions;
with Ada.Exceptions;
package body Test_Storage_Pools is

   Block_Header_Size : constant System.Storage_Elements.Storage_Count :=
      System.Storage_Elements.Storage_Count (Block_Header_Type'Size /
                                             System.Storage_Elements.Storage_Element'Size);

   --  This would be cleaner if Address_To_Access_Conversions took the
   --  pointer type as parameter, instead of declaring it!
   package Address_To_Block_Access is new System.Address_To_Access_Conversions (Block_Header_Type);

   function To_Block_Access
      (Pool    : in Storage_Pool_Type;
       Address : in System.Address)
       return Block_Access_Type
   is
      use type System.Address;
   begin
      if Address < Pool.Storage (1)'Address or
         Address > Pool.Storage (Pool.Pool_Size)'Address
      then
         raise Storage_Error;
      end if;
      return Block_Access_Type (Address_To_Block_Access.To_Pointer (Address));
   end To_Block_Access;

   function To_Address (Value : in Block_Access_Type) return System.Address
   is begin
      return Address_To_Block_Access.To_Address (Address_To_Block_Access.Object_Pointer (Value));
   end To_Address;

   function Aligned_Address
      (Address     : in System.Storage_Elements.Integer_Address;
       Alignment   : in System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Integer_Address
      --  Adjust Address upwards to next Alignment.
   is
      use System.Storage_Elements;
      Aligned : constant Integer_Address := Address + Address rem Integer_Address (Alignment);
   begin
      return Aligned;
   end Aligned_Address;

   -----------
   --  Override Root_Storage_Pool operations

   procedure Allocate
      (Pool                     : in out Storage_Pool_Type;
       Storage_Address          :    out System.Address;
       Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
       Alignment                : in     System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Alignment);

      use System.Storage_Elements;
      use Ada.Exceptions;
      Block                : Block_Access_Type := Pool.First_Free;
      Block_Start          : Integer_Address;
      Remaining_Block      : Block_Access_Type;
      Aligned              : Integer_Address;
      Prev                 : Block_Access_Type := null;

      --  We store a block header in free'd blocks, to maintain the
      --  free block list. So each allocated block has to be at least
      --  that big.
      Padded_Size    : constant Storage_Count := Storage_Count'Max (Size_In_Storage_Elements, Block_Header_Size);
      Allocated_Size : Storage_Count;
   begin
      if Pool.Debug then
         Ada.Text_IO.Put_Line ("allocating " &
                               Storage_Count'Image (Size_In_Storage_Elements) &
                               " from " &
                               Pool.Name.all);
      end if;

      Find_Free_Fit :
      loop
         if Block = null then
            Raise_Exception (Storage_Error'Identity, "Allocate: pool full (or fragmented)");
         end if;
         exit Find_Free_Fit when Block.Size >= Padded_Size;
         Prev := Block;
         Block := Block.Next;
      end loop Find_Free_Fit;

      --  Aligned points past the end of the just-allocated block; it
      --  is the base of the block of remaining space.
      Block_Start := To_Integer (To_Address (Block));
      Aligned     := Aligned_Address
          (Address   => Block_Start + Integer_Address (Padded_Size),
           Alignment => 8);

      Allocated_Size  := Storage_Count (Aligned - Block_Start);

      --  Allocated_Size might be > Block.Size because of alignment.
      --  In that case, their is no remaining space, so it can't be a
      --  block.
      if Block.Size > Allocated_Size and then Block.Size - Allocated_Size >= Block_Header_Size then
         --  Ok, remaining space can be a real block. But check to see
         --  if it is outside the pool!
         begin
            Remaining_Block := To_Block_Access (Pool, To_Address (Aligned));
         exception
         when Storage_Error =>
            Raise_Exception (Storage_Error'Identity, "Allocate: pool full (or fragmented)");
         end;

         if Prev = null then
            --  Allocated from first free block.
            Pool.First_Free := Remaining_Block;
         else
            Prev.Next := Remaining_Block;
         end if;

         Remaining_Block.all :=
            (Size => Block.Size - Allocated_Size,
             Next => Block.Next);
      else
         --  Remaining space too small for a block. Just link to next
         --  free block.
         if Prev = null then
            --  Allocated from first free block.
            Pool.First_Free := Pool.First_Free.Next;
         else
            Prev.Next := Block.Next;
         end if;

      end if;

      Pool.Allocate_Count         := Pool.Allocate_Count + 1;
      --  Only track actual request in Allocated_Elements, since
      --  that's what will be deallocated.
      Pool.Allocated_Elements     := Pool.Allocated_Elements + Natural (Size_In_Storage_Elements);
      Pool.Max_Allocated_Elements := Natural'Max (Pool.Allocated_Elements, Pool.Max_Allocated_Elements);
      Storage_Address             := To_Address (Block);
   end Allocate;

   procedure Deallocate
      (Pool                     : in out Storage_Pool_Type;
       Storage_Address          : in     System.Address;
       Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
       Alignment                : in     System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Alignment);

      use System.Storage_Elements;
      Block : Block_Access_Type;
   begin
      if Pool.Debug then
         Ada.Text_IO.Put_Line ("deallocating " &
                               Storage_Count'Image (Size_In_Storage_Elements) &
                               " from " &
                               Pool.Name.all);
      end if;

      --  Store a free-list block header in the free'd block, add
      --  block to head of free list.

      Block := To_Block_Access (Pool, Storage_Address);

      Block.all :=
         (Size => Size_In_Storage_Elements,
          Next => Pool.First_Free);

      Pool.First_Free := Block;

      Pool.Deallocate_Count := Pool.Deallocate_Count + 1;
      Pool.Allocated_Elements := Pool.Allocated_Elements - Natural (Size_In_Storage_Elements);
   exception
   when Storage_Error =>
      Ada.Exceptions.Raise_Exception
         (Program_Error'Identity,
          "Address not from storage pool " & Pool.Name.all);
   end Deallocate;

   function Storage_Size (Pool : Storage_Pool_Type) return System.Storage_Elements.Storage_Count
   is begin
      return Pool.Pool_Size;
   end Storage_Size;

   -----------
   --  New operations

   function Allocate_Count (Pool : Storage_Pool_Type) return Natural
   is begin
      return Pool.Allocate_Count;
   end Allocate_Count;

   function Allocated_Elements (Pool : Storage_Pool_Type) return Natural
   is begin
      return Pool.Allocated_Elements;
   end Allocated_Elements;

   procedure Check_Deallocated (Pool : in Storage_Pool_Type)
   is begin
      if Pool.Allocated_Elements /= 0 then
         Ada.Text_IO.Put_Line ("Error : " & Pool.Name.all & " not deallocated");
         Show_Storage (Pool, Force_Debug => True);
      end if;
   end Check_Deallocated;

   function Deallocate_Count (Pool : Storage_Pool_Type) return Natural
   is begin
      return Pool.Deallocate_Count;
   end Deallocate_Count;

   function Max_Allocated_Elements (Pool : Storage_Pool_Type) return Natural
   is begin
      return Pool.Max_Allocated_Elements;
   end Max_Allocated_Elements;

   procedure Reset_Counts (Pool : in out Storage_Pool_Type)
   is begin
      Pool.Deallocate_Count := 0;
      Pool.Allocate_Count := 0;
      Pool.Max_Allocated_Elements := Pool.Allocated_Elements;
   end Reset_Counts;

   procedure Set_Debug (Pool : in out Storage_Pool_Type; Debug : in Boolean)
   is begin
      Pool.Debug := Debug;
   end Set_Debug;

   procedure Show_Storage (Pool : in Storage_Pool_Type; Force_Debug : in Boolean := False)
   is
      use Ada.Text_IO;
   begin
      if Pool.Debug or Force_Debug then
         Put_Line (Pool.Name.all & " : ");
         Put_Line ("Allocate_Count         => " & Natural'Image (Pool.Allocate_Count));
         Put_Line ("Deallocate_Count       => " & Natural'Image (Pool.Deallocate_Count));
         Put_Line ("Allocated_Elements     => " & Natural'Image (Pool.Allocated_Elements));
         Put_Line ("Max_Allocated_Elements => " & Natural'Image (Pool.Max_Allocated_Elements));
      end if;
   end Show_Storage;

   -----------
   --  Private operations

   procedure Initialize (Pool : in out Storage_Pool_Type)
   is
      use System.Storage_Elements;
      use Ada.Exceptions;
   begin
      if Pool.Pool_Size < Block_Header_Size then
         Raise_Exception (Storage_Error'Identity, "Initialize: pool_size < header_size");
      end if;

      Pool.Debug                  := False;
      Pool.Allocate_Count         := 0;
      Pool.Deallocate_Count       := 0;
      Pool.Allocated_Elements     := 0;
      Pool.Max_Allocated_Elements := 0;
      Pool.First_Free             := To_Block_Access
         (Pool,
          To_Address
            (Aligned_Address
               (Address   => To_Integer (Pool.Storage'Address),
                Alignment => 8)));
      Pool.First_Free.all         := (Pool.Pool_Size, null);
   end Initialize;

end Test_Storage_Pools;
