--  Abstract:
--
--  Stack implementation using SAL.Poly.Unbounded_Arrays.
--
--  Copyright (C) 1998 - 2000, 2002, 2003 Stephen Leake.  All Rights Reserved.
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

with System.Storage_Pools;
with SAL.Aux.Definite_Private_Items;
with SAL.Poly.Unbounded_Arrays;
generic
   Initial_Stack_Size : in Natural;
   --  Stack starts with space for Initial_Size items. When it is
   --  filled, space is doubled. When it is emptied to one-quarter,
   --  space is halved, but never below Initial_Size.

   Array_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   --  Root_Storage_Pool is limited, which does not allow defaults.
   --  Default for a global array type should be
   --  <some_global_access_type>'Storage_Pool. Default for a local
   --  array type, which should be reclaimed when it goes out of
   --  scope, is implementation defined (sigh).
package SAL.Poly.Stacks.Unbounded_Array is
   pragma Elaborate_Body; -- SAL.Poly.Unbounded_Arrays is.

   type Unbounded_Stack_Type is new Stack_Type with private;

   ------------
   --  Override dispatching operations on Unbounded_Stack_Type

   overriding procedure Clear (Stack : in out Unbounded_Stack_Type);

   overriding function Depth (Stack : in Unbounded_Stack_Type) return Natural;

   overriding function Is_Empty (Stack : in Unbounded_Stack_Type) return Boolean;

   overriding function Max_Depth (Stack : in Unbounded_Stack_Type) return Natural;

   overriding function Peek (Stack : in Unbounded_Stack_Type; Index : in Natural) return Item_Type;

   overriding procedure Pop (Stack : in out Unbounded_Stack_Type);

   overriding procedure Pop (Stack : in out Unbounded_Stack_Type; Item : out Item_Type);

   overriding procedure Push (Stack : in out Unbounded_Stack_Type; Item : in Item_Type);

   overriding function Top (Stack : in Unbounded_Stack_Type) return Item_Type;

private
   package Item_Aux is new SAL.Aux.Definite_Private_Items (Item_Type);

   package Item_Arrays is new SAL.Poly.Unbounded_Arrays
     (Index_Type         => Standard.Integer,
      Item_Type          => Item_Type,
      Item_Node_Type     => Item_Type,
      To_Item_Node       => Item_Aux.To_Item_Node,
      Free_Item          => Item_Aux.Free_Item,
      Copy_Item_Node     => Item_Aux.Copy,
      Array_Storage_Pool => Array_Storage_Pool);

   type Item_Array_Type is new Item_Arrays.Array_Type with null record;

   --  Override Item_Array_Type operations

   overriding procedure Initialize (Item : in out Item_Array_Type);

   type Unbounded_Stack_Type is new Stack_Type with record
      Max_Depth : Natural := 0;
      Data      : Item_Array_Type;
   end record;

end SAL.Poly.Stacks.Unbounded_Array;
