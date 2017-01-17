--  Abstract :
--
--  Helper package for instantiating algorithms for containers that
--  are arrays.
--
--  Copyright (C) 1999, 2000 Stephen Leake.  All Rights Reserved.
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

pragma License (Modified_GPL);

generic
   type Item_Node_Type is private;
   type Index_Type is (<>);
   type Array_Type is array (Index_Type range <>) of Item_Node_Type;
   type Container_Type is access constant Array_Type;
   --  'None' is represented by Current < Container.all'first, so
   --  Index_Type should have a value that is normally not used by
   --  containers.
   --
   --  Access discriminants allowed only for limited types, so we need
   --  an access type for the discriminant of Iterator_Type.

package SAL.Aux.Enum_Iterators is
   pragma Preelaborate; -- named access type

   type Iterator_Type (Container : Container_Type) is record
      Current : Index_Type := Container.all'First;
   end record;

   function First (Container : in Container_Type) return Iterator_Type;
   function Last (Container : in Container_Type) return Iterator_Type;
   function None (Container : in Container_Type) return Iterator_Type;

   function Middle (First, Last : in Iterator_Type) return Iterator_Type;

   function Current (Iterator : in Iterator_Type) return Item_Node_Type;

   function Is_Null (Iterator : in Iterator_Type) return Boolean;

   function Next (Iterator : in Iterator_Type) return Iterator_Type;
   procedure Next (Iterator : in out Iterator_Type);

   function Prev (Iterator : in Iterator_Type) return Iterator_Type;
   procedure Prev (Iterator : in out Iterator_Type);

   pragma Inline (First, Last, None, Middle, Current, Is_Null, Next, Prev);

end SAL.Aux.Enum_Iterators;
