--  Abstract :
--
--  A simple generic doubly linked list, with useful operations,
--  allowing only definite non-limited item types. Implemented for
--  comparison with SAL.Poly.Lists.Double.
--
--  Copyright (C) 2004 Stephen Leake. All Rights Reserved.
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

with Ada.Finalization;
generic
   type Item_Type is private;
package SAL.Simple.Lists.Double is
   pragma Preelaborate; --  Node_Access_Type.

   type List_Type is new Ada.Finalization.Controlled with private;

   Null_List : constant List_Type;

   --------------
   --  Override List_Type operations

   overriding procedure Finalize (List : in out List_Type);
   --  Free all items in List.

   -------------
   --  New List_Type operations

   procedure Delete_Tail (List : in out List_Type);
   --  Delete item at Tail of List.
   --
   --  Raises Constraint_Error if List is empty.

   procedure Insert_Tail (List : in out List_Type; Item : in Item_Type);
   procedure Add (List : in out List_Type; Item : in Item_Type) renames Insert_Tail;
   --  Add To_Item_Node (Item) to tail of List.

private
   type Node_Type;

   type Node_Access_Type is access Node_Type;

   type Node_Type is record
      Item : Item_Type;
      Prev : Node_Access_Type;
      Next : Node_Access_Type;
   end record;

   type List_Type is new Ada.Finalization.Controlled with record
      Head : Node_Access_Type := null;
      Tail : Node_Access_Type := null;
   end record;

   Null_List : constant List_Type := (Ada.Finalization.Controlled with null, null);

end SAL.Simple.Lists.Double;
