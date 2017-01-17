--  Abstract:
--
--  see spec
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
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

package body SAL.Simple.Searches.Binary is

   function Middle (First, Last : in Index_Type) return Index_Type;
   pragma Inline (Middle);

   function Middle (First, Last : in Index_Type) return Index_Type
   is begin
      return Index_Type'Val (Index_Type'Pos (First) + (Index_Type'Pos (Last) - Index_Type'Pos (First)) / 2);
   end Middle;

   function Search_Less_Equal
      (List : in List_Type;
       Item : in Item_Type)
       return Index_Type
   is
      First : Index_Type := Binary.First (List);
      Last : Index_Type := Binary.Last (List);
      Middle : Index_Type := Binary.Middle (First, Last);
   begin
      loop
         if Middle = First then
            --  done searching; check for success
            if Less_Equal (List, Middle, Item) then
               return Middle;
            else
               raise SAL.Domain_Error;
            end if;
         else
            --  keep searching
            if Less_Equal (List, Middle, Item) then
               First := Middle;
            else
               Last := Middle;
            end if;
            Middle := Binary.Middle (First, Last);
         end if;
      end loop;
   end Search_Less_Equal;

end SAL.Simple.Searches.Binary;
