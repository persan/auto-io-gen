--  Abstract:
--
--  see spec
--
--  Copyright (C) 1998, 2003 Stephen Leake.  All Rights Reserved.
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

package body SAL.Poly.Stacks.Unbounded_Array is

   procedure Clear (Stack : in out Unbounded_Stack_Type)
   is begin
      Finalize (Stack.Data);
      Initialize (Stack.Data);
   end Clear;

   function Depth (Stack : in Unbounded_Stack_Type) return Natural
   is begin
      return Item_Arrays.Length (Stack.Data);
   end Depth;

   function Is_Empty (Stack : in Unbounded_Stack_Type) return Boolean
   is begin
      return Item_Arrays.Length (Stack.Data) = 0;
   end Is_Empty;

   function Max_Depth (Stack : in Unbounded_Stack_Type) return Natural
   is begin
      return Stack.Max_Depth;
   end Max_Depth;

   function Peek (Stack : in Unbounded_Stack_Type; Index : in Natural) return Item_Type
   is begin
      return Get (Stack.Data, Item_Arrays.First (Stack.Data) - 1 + Index);
   end Peek;

   procedure Pop (Stack : in out Unbounded_Stack_Type)
   is begin
      Delete_First (Stack.Data);
   end Pop;

   procedure Pop (Stack : in out Unbounded_Stack_Type; Item : out Item_Type)
   is begin
      Item := Peek (Stack, 1);
      Delete_First (Stack.Data);
   end Pop;

   procedure Push (Stack : in out Unbounded_Stack_Type; Item : in Item_Type)
   is begin
      Add_First (Stack.Data, Item);
      Stack.Max_Depth := Natural'Max (Stack.Max_Depth, Item_Arrays.Length (Stack.Data));
   end Push;

   function Top (Stack : in Unbounded_Stack_Type) return Item_Type
   is begin
      if Item_Arrays.Length (Stack.Data) < 1 then
         raise SAL.Container_Empty;
      else
         return Peek (Stack, 1);
      end if;
   end Top;

   procedure Initialize (Item : in out Item_Array_Type)
   is begin
      Create (Item, Initial_Stack_Size, Item_Arrays.Prepend, First => 1);
   end Initialize;

end SAL.Poly.Stacks.Unbounded_Array;
