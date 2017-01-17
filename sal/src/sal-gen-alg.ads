--  Abstract :
--
--  Root for generic algorithms.
--
--  Note that this package can be used with tagged container types, as
--  long as they are not abstract.
--
--  Design :
--
--  This package takes the generic formal parameters that are common
--  to most algorithms for one container type, and are provided by all
--  container and iterator types.
--
--  Algorithms are implemented in child packages. Some algorithms may
--  require other operations that are only provided by some container
--  or iterator types.
--
--  Copyright (C) 1999, 2000, 2003, 2004, 2008 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
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
   type Item_Node_Type is private;
   type Container_Type (<>) is limited private;

   type Iterator_Type (<>) is private;
   --  Iterator_Type is indefinite, because some require a
   --  discriminant of Container'access. Initializing one with None
   --  should be fast.

   --  The following subprograms are not referenced in this spec, but
   --  we require them here so child specs don't have to. They define
   --  what we mean by "container"; containers (and associated
   --  iterators) are types that provide these procedures.
   --
   --  We can't give 'pragma Unreferenced' here, because they are
   --  referenced in the child packages. So we suppress the "not
   --  referenced" warning in the GNAT project file.
   with function Current (Iterator : in Iterator_Type) return Item_Node_Type is <>;pragma Unreferenced (Current);
   with function First (Container : in Container_Type) return Iterator_Type is <>;pragma Unreferenced (First);
   with function Last (Container : in Container_Type) return Iterator_Type is <>;pragma Unreferenced (Last);
   with function None (Container : in Container_Type) return Iterator_Type is <>;pragma Unreferenced (None);
   with function Is_Null (Iterator : in Iterator_Type) return Boolean is <>;pragma Unreferenced (Is_Null);
   with procedure Next_Procedure (Iterator : in out Iterator_Type); pragma Unreferenced (Next_Procedure);
   with function Next_Function (Iterator : in Iterator_Type) return Iterator_Type;pragma Unreferenced (Next_Function);
package SAL.Gen.Alg is
   pragma Pure;

   procedure Do_Nothing (Container : in Container_Type);
   pragma Inline (Do_Nothing);
   --  For algorithm defaults.

end SAL.Gen.Alg;
