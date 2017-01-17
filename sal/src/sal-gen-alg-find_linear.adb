--  Abstract :
--
--  See spec
--
--  Copyright (C) 1999, 2000, 2002, 2007 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen.Alg.Find_Linear is

   procedure Next (Iterator : in out Iterator_Type) renames Next_Procedure;

   function Find_Equal
      (Start : in Iterator_Type;
       Key   : in Key_Type)
      return Iterator_Type
   is
      Found : Iterator_Type := Start;
   begin
      loop
         exit when Is_Null (Found) or else Is_Equal_Node_Key (Current (Found), Key);
         Next (Found);
      end loop;
      return Found;
   end Find_Equal;

   procedure Delete
      (Container : in out Container_Type;
       Key       : in     Key_Type)
   is
      Iterator : Iterator_Type := Find_Equal (First (Container), Key);
   begin
      if Is_Null (Iterator) then
         raise SAL.Not_Found;
      else
         Delete (Container, Iterator);
      end if;
   end Delete;

   procedure Insert_Before
     (Container : in out Container_Type;
      Before    : in     Key_Type;
      Item      : in     Item_Type;
      Copies    : in     Natural        := 1)
   is
      Iterator : constant Iterator_Type := Find_Equal (First (Container), Before);
   begin
      if Is_Null (Iterator) then
         raise SAL.Not_Found;
      else
         Insert_Before (Container, Iterator, Item, Copies);
      end if;

   end Insert_Before;

end SAL.Gen.Alg.Find_Linear;
