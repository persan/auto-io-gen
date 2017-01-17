--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2004, 2008 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen.Queues.Gen_Bounded_Nonlimited is

   --  Local subprograms

   function Wrap (Queue : in Queue_Type; I : in Integer) return Integer
   is begin
      if I > Queue.Size then
         return I - Queue.Size;
      elsif I < 1 then
         return Queue.Size + I;
      else
         return I;
      end if;
   end Wrap;

   ----------
   --  Public subprograms

   procedure Clear (Queue : in out Queue_Type) is
   begin
      Queue.Count := 0;
   end Clear;

   function Count (Queue : in Queue_Type) return Natural is
   begin
      return Queue.Count;
   end Count;

   function Is_Empty (Queue : in Queue_Type) return Boolean is
   begin
      return Queue.Count = 0;
   end Is_Empty;

   function Is_Full (Queue : in Queue_Type) return Boolean is
   begin
      return Queue.Count = Queue.Size;
   end Is_Full;

   procedure Remove (Queue : in out Queue_Type; Item : out Item_Type) is
   begin
      if Queue.Count = 0 then
         raise Container_Empty;
      end if;

      Item        := Queue.Data (Queue.Head);
      Queue.Count := Queue.Count - 1;

      if Queue.Count > 0 then
         Queue.Head := Wrap (Queue, Queue.Head + 1);
      end if;
   end Remove;

   procedure Add (Queue : in out Queue_Type; Item : in Item_Type) is
   begin
      if Queue.Count = Queue.Size then
         case Queue.Overflow_Handling is
         when Error =>
            raise Container_Full;
         when Overwrite =>
            Queue.Count := Queue.Count - 1;
            Queue.Head  := Wrap (Queue, Queue.Head + 1);
         end case;
      end if;

      if Queue.Count = 0 then
         Queue.Tail     := 1;
         Queue.Head     := 1;
         Queue.Count    := 1;
         Queue.Data (1) := Item;
      else
         Queue.Tail              := Wrap (Queue, Queue.Tail + 1);
         Queue.Data (Queue.Tail) := Item;
         Queue.Count             := Queue.Count + 1;
      end if;
   end Add;

   function Get_Overflow_Handling (Queue : in Queue_Type) return Overflow_Action_Type
   is begin
      return Queue.Overflow_Handling;
   end Get_Overflow_Handling;

   procedure Set_Overflow_Handling (Queue : in out Queue_Type; Handling : in Overflow_Action_Type)
   is begin
      Queue.Overflow_Handling := Handling;
   end Set_Overflow_Handling;

end SAL.Gen.Queues.Gen_Bounded_Nonlimited;
