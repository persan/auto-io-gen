--  Abstract :
--
--  see spec.
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

with Ada.Exceptions;
package body SAL.Poly.Lists.Double.Validate is

   procedure Assert (Test : in Boolean; Message : in String)
   is begin
      if not Test then
         Ada.Exceptions.Raise_Exception (Validation_Error'Identity, Message);
      end if;
   end Assert;

   procedure Validate (List : in List_Type)
   is
      I : Node_Access_Type;
   begin
      if List.Head = null then
         Assert (List.Tail = null, "head, tail not both null");
      else

         Assert (List.Head.Prev = null, "head.prev /= null");
         Assert (List.Tail.Next = null, "tail.next /= null");

         I := List.Head;
         Test_Elements :
         loop
            if I.Next = null then
               Assert (List.Tail = I, "tail not last item");
               exit Test_Elements;
            else
               Assert (I.Next.Prev = I, "next.prev /= current");
            end if;
            I := I.Next;
         end loop Test_Elements;
      end if;
   end Validate;

end SAL.Poly.Lists.Double.Validate;
