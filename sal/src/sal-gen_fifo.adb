--  Abstract :
--
--  See spec.
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

package body SAL.Gen_FIFO is

   --  local subprograms

   function Inc_Wrap (Item : in Index_Type) return Index_Type
   is begin
      if Item = Index_Type'Last then
         return Index_Type'First;
      else
         return Item + 1;
      end if;
   end Inc_Wrap;

   ----------
   --  Public subprograms

   procedure Get
     (FIFO   : in out FIFO_Type;
      Data   :    out Element_Type)
   is
   begin
      if Is_Empty (FIFO) then
         raise SAL.Container_Empty;
      end if;

      Data := FIFO.Data (FIFO.Next_Get);

      FIFO.Next_Get := Inc_Wrap (FIFO.Next_Get);

      FIFO.Empty := FIFO.Next_Get = FIFO.Next_Put;

   end Get;

   procedure Initialize (FIFO : in out FIFO_Type) is
   begin
      FIFO.Next_Get := Index_Type'First;
      FIFO.Next_Put := Index_Type'First;
      FIFO.Empty    := True;
   end Initialize;

   function Is_Empty (FIFO : in FIFO_Type) return Boolean is
   begin
      return FIFO.Empty;
   end Is_Empty;

   function Is_Full (FIFO : in FIFO_Type) return Boolean is
   begin
      return (not FIFO.Empty) and FIFO.Next_Get = FIFO.Next_Put;
   end Is_Full;

   procedure Put
     (FIFO   : in out FIFO_Type;
      Data   : in     Element_Type)
   is
   begin
      if Is_Full (FIFO) then
         raise SAL.Container_Full;
      end if;

      FIFO.Data (FIFO.Next_Put) := Data;

      FIFO.Next_Put := Inc_Wrap (FIFO.Next_Put);

      FIFO.Empty := False;

   end Put;

end SAL.Gen_FIFO;
