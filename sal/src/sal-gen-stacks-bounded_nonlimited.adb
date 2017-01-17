--  Abstract:
--
--  see spec
--
--  Copyright (C) 1999 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.

package body SAL.Gen.Stacks.Bounded_Nonlimited is

   ------------
   --  Operations on Stack_Type (alphabetical order)

   procedure Clear (Stack : in out Stack_Type)
   is begin
      Stack.Top := 0;
   end Clear;

   function Depth (Stack : in Stack_Type) return Natural
   is begin
      return Stack.Top;
   end Depth;

   function Is_Empty (Stack : in Stack_Type) return Boolean
   is begin
      return Stack.Top = 0;
   end Is_Empty;

   function Is_Full (Stack : in Stack_Type) return Boolean
   is begin
      return Stack.Top = Stack.Size;
   end Is_Full;

   function Max_Depth (Stack : in Stack_Type) return Natural
   is begin
      return Stack.Max_Depth;
   end Max_Depth;

   function Peek (Stack : in Stack_Type; Index : in Positive) return Item_Type
   is begin
      return Stack.Items (Stack.Top - Index + 1);
   end Peek;

   procedure Pop (Stack : in out Stack_Type)
   is begin
      if Is_Empty (Stack) then
         raise Container_Full;
      else
         Stack.Top := Stack.Top - 1;
      end if;
   end Pop;

   procedure Push (Stack : in out Stack_Type; Item : in Item_Type)
   is begin
      if Is_Full (Stack) then
         raise Container_Full;
      else
         Stack.Top := Stack.Top + 1;
         Stack.Items (Stack.Top) := Item;
      end if;
   end Push;

   function Top (Stack : in Stack_Type) return Item_Type
   is begin
      return Peek (Stack, 1);
   end Top;

end SAL.Gen.Stacks.Bounded_Nonlimited;
