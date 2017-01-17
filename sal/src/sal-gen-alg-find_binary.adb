--  Abstract:
--
--  see spec
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

package body SAL.Gen.Alg.Find_Binary is

   function Find_Less_Equal
      (Container : in Container_Type;
       Key       : in Key_Type)
       return Iterator_Type
   is
      First_I  : Iterator_Type := First (Container);
      Last_I   : Iterator_Type := Last (Container);
      Middle_I : Iterator_Type := Middle (First_I, Last_I);

      procedure Sequence_Last (Item : in out Iterator_Type)
         --  Set Item to last in sequence of equal items starting at Item.
      is begin
         loop
            Middle_I := Item;
            Next_Procedure (Middle_I);
            if Is_Null (Middle_I) or else (not Is_Equal_Node_Key (Current (Middle_I), Key)) then
               return;
            else
               Item := Middle_I;
            end if;
         end loop;
      end Sequence_Last;
   begin

      loop
         if Is_Null (Middle_I) then
            --  not found
            return Middle_I;
         elsif Middle_I = First_I or Middle_I = Last_I then
            --  done searching; check for success
            if Is_Less_Equal_Node_Key (Current (Last_I), Key) then
               Sequence_Last (Last_I);
               return Last_I;
            elsif Is_Less_Equal_Node_Key (Current (First_I), Key) then
               Sequence_Last (First_I);
               return First_I;
            else
               return None (Container);
            end if;
         else
            --  keep searching
            if Is_Less_Equal_Node_Key (Current (Middle_I), Key) then
               First_I := Middle_I;
            else
               Last_I := Middle_I;
            end if;
            Middle_I := Middle (First_I, Last_I);
         end if;
      end loop;
   end Find_Less_Equal;

   function Find_Greater_Equal
      (Container : in Container_Type;
       Key       : in Key_Type)
       return Iterator_Type
   is

      First_I  : Iterator_Type := First (Container);
      Last_I   : Iterator_Type := Last (Container);
      Middle_I : Iterator_Type := Middle (First_I, Last_I);

      procedure Sequence_First (Item : in out Iterator_Type)
         --  Set Item to first in sequence of equal items ending at
         --  Item.
      is begin
         loop
            Middle_I := Item;
            Prev (Middle_I);
            if Is_Null (Middle_I) or else (not Is_Equal_Node_Key (Current (Middle_I), Key)) then
               return;
            else
               Item := Middle_I;
            end if;
         end loop;
      end Sequence_First;
   begin

      loop
         if Is_Null (Middle_I) then
            --  not found
            return Middle_I;
         elsif Middle_I = First_I or Middle_I = Last_I then
            --  done searching; check for success
            if Is_Greater_Equal_Node_Key (Current (Last_I), Key) then
               Sequence_First (Last_I);
               return Last_I;
            elsif Is_Greater_Equal_Node_Key (Current (First_I), Key) then
               Sequence_First (First_I);
               return First_I;
            else
               return None (Container);
            end if;
         else
            --  keep searching
            if Is_Greater_Equal_Node_Key (Current (Middle_I), Key) then
               Last_I := Middle_I;
            else
               First_I := Middle_I;
            end if;
            Middle_I := Middle (First_I, Last_I);
         end if;
      end loop;
   end Find_Greater_Equal;

end SAL.Gen.Alg.Find_Binary;
