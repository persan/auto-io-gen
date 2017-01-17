--  Abstract :
--
--  Algorithms for searching in a generic unordered container, using a
--  simple linear search.
--
--  An alternate design would be to require the function 'To_Key
--  (Item_Node_Type) return Key_Type' and then use pre-defined "=".
--  That would make it easier to instantiate this package for definite
--  types. However, when Item_Type is class-wide, it is not possible
--  to define To_Key. So we use this approach.
--
--  Copyright (C) 1999, 2000, 2007 Stephen Leake.  All Rights Reserved.
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

generic
   type Item_Type (<>) is limited private;
   type Key_Type (<>) is limited private;
   --  Key_Type can be Item_Type, or a value derived from Item_Type.

   with function Is_Equal_Node_Key (Left : in Item_Node_Type; Right : in Key_Type) return Boolean is <>;

   with procedure Delete
     (Container : in out Container_Type;
      Iterator  : in out Iterator_Type);
   --  Standard Iterator operation.

   with procedure Insert_Before
     (Container : in out Container_Type;
      Before    : in     Iterator_Type;
      Item      : in     Item_Type;
      Copies    : in     Natural        := 1);
   --  Standard Iterator operation.

package SAL.Gen.Alg.Find_Linear is
   pragma Pure;

   function Find_Equal
      (Start : in Iterator_Type;
       Key   : in Key_Type)
      return Iterator_Type;
   --  Return iterator pointing to item matching Key, or none if no
   --  match found.
   --
   --  Search sets result to Start, proceeds using Next. Search
   --  terminates when iterator is null, or when Is_Equal_Node_Key
   --  (Current, Key) is True.

   procedure Delete
      (Container : in out Container_Type;
       Key       : in     Key_Type);
   --  Delete item matching Key from Container.
   --
   --  Raises Not_Found if item matching Key is not found.

   procedure Insert_Before
     (Container : in out Container_Type;
      Before    : in     Key_Type;
      Item      : in     Item_Type;
      Copies    : in     Natural        := 1);
   --  Find Before in Container using Find_Equal, insert
   --  Item before it.
   --
   --  Raises Not_Found if Before_Item is not found in Container.

end SAL.Gen.Alg.Find_Linear;
