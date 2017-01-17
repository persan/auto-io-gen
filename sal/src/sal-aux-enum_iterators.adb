--  Abstract :
--
--  see spec.
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

package body SAL.Aux.Enum_Iterators is

   function First (Container : in Container_Type) return Iterator_Type is
   begin
      return (Container, Container'First);
   end First;

   function Last (Container : in Container_Type) return Iterator_Type is
   begin
      return (Container, Container'Last);
   end Last;

   function Middle (First, Last : in Iterator_Type) return Iterator_Type
   is begin
      return
         (Container => First.Container,
          Current => Index_Type'Val
          (Index_Type'Pos (First.Current)
           + (Index_Type'Pos (Last.Current) -
              Index_Type'Pos (First.Current)) / 2));
   end Middle;

   function None (Container : in Container_Type) return Iterator_Type
   is begin
      return (Container, Index_Type'Pred (Container'First));
   end None;

   function Current (Iterator : in Iterator_Type) return Item_Node_Type is
   begin
      return Iterator.Container (Iterator.Current);
   end Current;

   function Is_Null (Iterator : in Iterator_Type) return Boolean is
   begin
      return Iterator.Current < Iterator.Container'First;
   end Is_Null;

   function Next (Iterator : in Iterator_Type) return Iterator_Type
   is begin
      if Iterator.Current = Iterator.Container'Last then
         return (Iterator.Container, Current => Index_Type'Pred (Iterator.Container'First));
      else
         return (Iterator.Container, Current => Index_Type'Succ (Iterator.Current));
      end if;
   end Next;

   procedure Next (Iterator : in out Iterator_Type) is
   begin
      if Iterator.Current = Iterator.Container'Last then
         Iterator.Current := Index_Type'Pred (Iterator.Container'First);
      else
         Iterator.Current := Index_Type'Succ (Iterator.Current);
      end if;
   end Next;

   function Prev (Iterator : in Iterator_Type) return Iterator_Type
   is begin
      return (Iterator.Container, Current => Index_Type'Pred (Iterator.Current));
   end Prev;

   procedure Prev (Iterator : in out Iterator_Type) is
   begin
      Iterator.Current := Index_Type'Pred (Iterator.Current);
   end Prev;

end SAL.Aux.Enum_Iterators;
