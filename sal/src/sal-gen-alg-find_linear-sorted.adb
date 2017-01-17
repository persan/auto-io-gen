--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2000 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen.Alg.Find_Linear.Sorted is

   function Next (Iterator : in Iterator_Type) return Iterator_Type renames Next_Function;
   procedure Next (Iterator : in out Iterator_Type) renames Next_Procedure;

   function Prev (Iterator : in Iterator_Type) return Iterator_Type renames Prev_Function;
   procedure Prev (Iterator : in out Iterator_Type) renames Prev_Procedure;

   procedure Add
      (Container     : in out Container_Type;
       Item          : in     Item_Type;
       Item_Iterator :    out Iterator_Type;
       Direction     : in     SAL.Direction_Type := SAL.Forward)
   is
      Iterator : Iterator_Type := None (Container); -- unconstrained subtype requires initialization
      Found    : Boolean;
   begin
      Find_Greater_Equal_Item (Container, Item, Iterator, Found, Direction);

      if Found and not Allow_Duplicate_Keys then
         raise Duplicate_Key;
      end if;

      Insert_Before (Container, Iterator, Item);

      if Is_Null (Iterator) then
         --  inserted at end of Container
         Item_Iterator := Last (Container);
      else
         Item_Iterator := Prev (Iterator);
      end if;
   end Add;

   procedure Add
      (Container : in out Container_Type;
       Item      : in     Item_Type;
       Direction : in     SAL.Direction_Type := SAL.Forward)
   is
      Dummy_Iterator : Iterator_Type := First (Container); -- unconstrained type needs initialization
   begin
      Add (Container, Item, Dummy_Iterator, Direction);
   end Add;

   procedure Find_Greater_Equal_Item
      (Container : in     Container_Type;
       Item      : in     Item_Type;
       Iterator  :    out Iterator_Type;
       Found     :    out Boolean;
       Direction : in     Direction_Type := Forward)
   is begin
      case Direction is
      when Forward =>
         Iterator := First (Container);
         loop
            exit when Is_Null (Iterator) or else Is_Greater_Equal_Node_Item (Current (Iterator), Item);
            Next (Iterator);
         end loop;

         --  If Is_Null (Iterator), all elements are less than
         --  Item_Node; Insert_Before will insert after Last.

         Found := (not Is_Null (Iterator)) and then Is_Equal_Node_Item (Current (Iterator), Item);

      when Backward =>
         Iterator := Last (Container);
         loop
            exit when Is_Null (Iterator) or else not Is_Greater_Equal_Node_Item (Current (Iterator), Item);
            Prev (Iterator);
         end loop;

         if Is_Null (Iterator) then
            --  All elements are greater_equal to Item_Node;
            --  Insert_Before should insert before First.
            Iterator := First (Container);
         else
            Next (Iterator);
         end if;
         Found := (not Is_Null (Iterator)) and then Is_Equal_Node_Item (Current (Iterator), Item);

      end case;
   end Find_Greater_Equal_Item;

   procedure Find_Greater_Equal_Key
      (Container : in     Container_Type;
       Key       : in     Key_Type;
       Iterator  :    out Iterator_Type;
       Found     :    out Boolean;
       Direction : in     Direction_Type := Forward)
   is begin
      case Direction is
      when Forward =>
         Iterator := First (Container);
         loop
            exit when Is_Null (Iterator) or else Is_Greater_Equal_Node_Key (Current (Iterator), Key);
            Next (Iterator);
         end loop;
         Found := (not Is_Null (Iterator)) and then Is_Equal_Node_Key (Current (Iterator), Key);

      when Backward =>
         Iterator := Last (Container);
         loop
            exit when Is_Null (Iterator) or else not Is_Greater_Equal_Node_Key (Current (Iterator), Key);
            Prev (Iterator);
         end loop;
         if Is_Null (Iterator) then
            --  All elements are greater_equal to Item_Node.
            Iterator := First (Container);
         else
            Next (Iterator);
         end if;
         Found := (not Is_Null (Iterator)) and then Is_Equal_Node_Key (Current (Iterator), Key);

      end case;
   end Find_Greater_Equal_Key;

   procedure Find_Greater_Equal_Node
      (Container : in     Container_Type;
       Item_Node : in     Item_Node_Type;
       Iterator  :    out Iterator_Type;
       Found     :    out Boolean;
       Direction : in     Direction_Type := Forward)
   is begin
      case Direction is
      when Forward =>
         Iterator := First (Container);
         loop
            exit when Is_Null (Iterator) or else Is_Greater_Equal_Node_Node (Current (Iterator), Item_Node);
            Next (Iterator);
         end loop;
         Found := (not Is_Null (Iterator)) and then Is_Equal_Node_Node (Current (Iterator), Item_Node);

      when Backward =>
         Iterator := Last (Container);
         loop
            exit when Is_Null (Iterator) or else not Is_Greater_Equal_Node_Node (Current (Iterator), Item_Node);
            Prev (Iterator);
         end loop;

         if Is_Null (Iterator) then
            --  All elements are greater_equal to Item_Node.
            Iterator := Last (Container);
         else
            Next (Iterator);
         end if;
         Found := (not Is_Null (Iterator)) and then Is_Equal_Node_Node (Current (Iterator), Item_Node);

      end case;
   end Find_Greater_Equal_Node;

   procedure Delete
      (Container : in out Container_Type;
       Key       : in     Key_Type;
       Direction : in     SAL.Direction_Type := SAL.Forward)
   is
      Iterator : Iterator_Type := First (Container); -- Iterator_Type is unconstrained
      Found    : Boolean;
   begin
      Find_Greater_Equal_Key (Container, Key, Iterator, Found, Direction);
      if Found then
         Delete (Container, Iterator);
      else
         raise SAL.Not_Found;
      end if;
   end Delete;

   procedure Remove_Out_Of_Order
      (Source : in out Container_Type;
       Dest   : in out Container_Type)
   is
      type Remove_Type is (None, Last, Current, Next);

      Iterator      : Iterator_Type := First (Source);
      Last_Iterator : Iterator_Type := First (Source);
      Next_Iterator : Iterator_Type := First (Source);
      Temp_Iterator : Iterator_Type := None (Source);
      Found         : Boolean;
      Remove        : Remove_Type;
   begin
      if Is_Null (Iterator) then
         --  Source is empty, and thus is sorted.
         return;
      else
         Next (Next_Iterator);
         if Is_Null (Next_Iterator) then
            --  Source has one element, and thus is sorted.
            return;
         end if;
      end if;

      Next (Iterator);
      Next (Next_Iterator);
      loop
         exit when Is_Null (Iterator);
         --  We want to handle the case where a single item has been
         --  edited so it is out of order. That means we look at the
         --  three items Last_Iterator, Iterator, Next_Iterator. We
         --  compare all three to each other, so there are six cases:
         --
         --  (1) (1, 2, 3) => Remove = None
         --
         --  (2) (1, 3, 2) => Remove = Current
         --
         --  (3) (3, 1, 2) => Remove = Last
         --
         --  (4) (3, 2, 1) => Remove = Current
         --
         --  (5) (2, 1, 3) => Remove = Current
         --
         --  (6) (2, 3, 1) => Remove = Next
         --
         --  Note that case (3) can only happen if Last_Iterator is
         --  First (Source); otherwise 1 would have been removed on
         --  the previous check.
         --
         --  If an item is removed, we do not advance Iterator,
         --  because we need to repeat the check with the new item.
         --
         --  Next_Iterator is null if Iterator is Last, so we handle
         --  that specially.
         --
         --  Note that we have to move one item at a time, to keep Dest sorted.
         --
         --  If Item key has only changed a little, it should probably
         --  be added to tail of Dest, so search Dest Backward.
         --
         --  Splice_Before renders the iterator moved invalid.

         if Is_Null (Next_Iterator) then
            --  Only two cases here!
            if Is_Greater_Equal_Node_Node (Current (Iterator), Current (Last_Iterator)) then
               Remove := None;
            else
               Remove := Current;
            end if;

         elsif Is_Greater_Equal_Node_Node (Current (Iterator), Current (Last_Iterator)) then
            --  case 1, 2, or 6
            if Is_Greater_Equal_Node_Node (Current (Next_Iterator), Current (Iterator)) then
               --  case 1
               Remove := None;
            elsif Is_Greater_Equal_Node_Node (Current (Next_Iterator), Current (Last_Iterator)) then
               --  case 2
               Remove := Current;
            else
               --  case 6
               Remove := Next;
            end if;
         else
            --  case 3, 4, or 5
            if Is_Greater_Equal_Node_Node (Current (Next_Iterator), Current (Iterator)) then
               --  case 3 or 5
               if Is_Greater_Equal_Node_Node (Current (Next_Iterator), Current (Last_Iterator)) then
                  --  case 5
                  Remove := Current;
               else
                  --  case 3
                  Remove := Last;
               end if;
            else
               --  case 4
               Remove := Current;
            end if;
         end if;

         case Remove is
         when None =>
            --  We assume the Next procedure is faster than
            --  iterator assignment; this is true for linked lists and
            --  arrays.
            Next (Iterator);
            Next (Last_Iterator);
            if not Is_Null (Next_Iterator) then
               Next (Next_Iterator);
            end if;

         when Last =>
            Find_Greater_Equal_Node (Dest, Current (Last_Iterator), Temp_Iterator, Found, Backward);

            Splice_Before
               (Source => Source,
                First  => Last_Iterator,
                Last   => Last_Iterator,
                Dest   => Dest,
                Before => Temp_Iterator);

            --  Last_Iterator is now invalid. Also, Last_Iterator was
            --  First (Source), so we must reset all three iterators.
            Last_Iterator := Iterator;
            Next (Iterator);
            Next (Next_Iterator);

         when Current =>
            Find_Greater_Equal_Node (Dest, Current (Iterator), Temp_Iterator, Found, Backward);

            Splice_Before
               (Source => Source,
                First  => Iterator,
                Last   => Iterator,
                Dest   => Dest,
                Before => Temp_Iterator);

            --  Iterator is now invalid.
            Iterator := Next (Last_Iterator);

         when Next =>
            --  Can't get here if Next_Iterator is null
            Find_Greater_Equal_Node (Dest, Current (Next_Iterator), Temp_Iterator, Found, Backward);

            Splice_Before
               (Source => Source,
                First  => Next_Iterator,
                Last   => Next_Iterator,
                Dest   => Dest,
                Before => Temp_Iterator);

            --  Next_Iterator is now invalid.
            Next_Iterator := Next (Iterator);

         end case;
      end loop;

   end Remove_Out_Of_Order;

   procedure Merge
      (Container : in out Container_Type;
       From      : in out Container_Type)
   is
      Iterator       : Iterator_Type := First (Container);
      From_Iterator  : Iterator_Type := First (From);
   begin
      loop
         exit when Is_Null (From_Iterator) or Is_Null (Iterator);
         if not Is_Greater_Equal_Node_Node (Current (From_Iterator), Current (Iterator)) then
            --  find all items in From that belong before Current (Iterator)
            loop
               Next (From_Iterator);
               exit when Is_Null (From_Iterator) or else
                  Is_Greater_Equal_Node_Node (Current (From_Iterator), Current (Iterator));
            end loop;

            if not Is_Null (From_Iterator) then
               Prev (From_Iterator);
            end if;

            Splice_Before
               (Source => From,
                First  => First (From),
                Last   => From_Iterator,
                Dest   => Container,
                Before => Iterator);

            From_Iterator := First (From);
         end if;
         Next (Iterator);
      end loop;

      if not Is_Null (From_Iterator) then
         --  insert rest of From at end of Container
         Splice_Before
            (Source => From,
             First  => None (Container),
             Last   => None (Container),
             Dest   => Container,
             Before => None (Container));
      end if;
   end Merge;

   procedure Sort
      (Container      : in out Container_Type;
       Temp_Container : in out Container_Type)
   is begin
      Remove_Out_Of_Order (Container, Temp_Container);
      Merge (Container, Temp_Container);
   end Sort;

end SAL.Gen.Alg.Find_Linear.Sorted;
