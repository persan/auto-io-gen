--  Abstract :
--
--  First In First Out circular buffer (also known as a Queue).
--
--  Copyright (C) 2002 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--

generic
   type Element_Type is private;
   FIFO_Size : in Integer; --  count of elements in fifo.
package SAL.Gen_FIFO is
   pragma Pure;

   type FIFO_Type is private;

   procedure Initialize (FIFO : in out FIFO_Type);
   --  Reset FIFO to empty.

   function Is_Full (FIFO : in FIFO_Type) return Boolean;
   --  Return True if FIFO is full.

   function Is_Empty (FIFO : in FIFO_Type) return Boolean;
   --  Return True if FIFO is empty.

   procedure Put
     (FIFO   : in out FIFO_Type;
      Data   : in     Element_Type);
   --  Put Data on FIFO.
   --
   --  Raises SAL.Container_Full if Is_Full.

   procedure Get
     (FIFO   : in out FIFO_Type;
      Data   :    out Element_Type);
   --  Get Data from FIFO.
   --
   --  Raise SAL.Container_Empty if Is_Empty.

private
   subtype Index_Type is Integer range 1 .. FIFO_Size;
   type Element_Array_Index_Type is array (Index_Type) of Element_Type;

   type FIFO_Type is record
      Data     : Element_Array_Index_Type;
      Next_Get : Index_Type := Index_Type'First;
      Next_Put : Index_Type := Index_Type'First;
      Empty    : Boolean    := True;
      --  If Next_Get = Next_Put, FIFO is either empty or full; Empty
      --  distinguishes.
   end record;

end SAL.Gen_FIFO;
