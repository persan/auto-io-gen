--  Abstract:
--
--  Test Sal.Gen.Lists.Double, with various item types.
--
--  Copyright (C) 1997 - 2000 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with SAL;
with Test_Gen_Lists_Double_Aux;
with Test_Storage_Pools;
procedure Test_Gen_Lists_Double is
   Debug : Boolean := False;
begin
   if Ada.Command_Line.Argument_Count > 0 then
      Debug := Boolean'Value (Ada.Command_Line.Argument (1));
   end if;

   Test_Storage_Pools.Set_Debug (Test_Gen_Lists_Double_Aux.Node_Storage_Pool, Debug);

   Put_Line ("Definite non-tagged non-limited items (integer_lists)");
   Integer_Lists :
   declare
      use Test_Gen_Lists_Double_Aux.Integers;
      use Lists;

      List : List_Type;
      Other_List :  List_Type;
   begin
      Put_Line ("null List:");
      Check_Print (List, "List");

      Put_Line ("Insert_Tail 1 3 5");
      Insert_Tail (List, 1);
      Insert_Tail (List, 3);
      Insert_Tail (List, 5);
      Check_Print (List, "List");
      Put_Line ("Is_Empty (List) => " & Boolean'Image (Is_Empty (List)));

      Put_Line ("Insert_Head 0");
      Insert_Head (List, 0);
      Check_Print (List, "List");

      Put_Line ("Delete_Head = " & Integer'Image (Head (List)));
      Delete_Head (List);
      Check_Print (List, "List");

      Put_Line ("Delete_Tail = " & Integer'Image (Tail (List)));
      Delete_Tail (List);
      Check_Print (List, "List");

      Put_Line ("Swap");
      Swap (A => List, B => Other_List);
      Check_Print (List, "List");
      Check_Print (Other_List, "Other_List");

      Put_Line ("Copy");
      Copy (Source => Other_List, Dest => List);
      Check_Print (List, "List");
      Check_Print (Other_List, "Other_List");

      --  Count tested by Print_List

      Finalize (List);
      Finalize (Other_List);
   end Integer_Lists;
   Test_Storage_Pools.Show_Storage (Test_Gen_Lists_Double_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Gen_Lists_Double_Aux.Node_Storage_Pool);

   ----------
   --  Basic Iterators operations

   New_Line;
   Put_Line ("Testing basic Iterators operations.");
   Integer_List_Iterators_Basic :
   declare
      use Test_Gen_Lists_Double_Aux.Integers;
      use Lists;
      use Iterators;

      List : List_Type;
      Iterator : Iterator_Type;
   begin
      if Is_Null (Iterator) then
         Put_Line ("ok, default iterator is Null");
      else
         Put_Line ("Error: default iterator is NOT Null");
      end if;

      if Is_Null (None (List)) then
         Put_Line ("ok, None is Null");
      else
         Put_Line ("Error: None is NOT Null");
      end if;

      Insert_Tail (List, 1);
      Insert_Tail (List, 2);
      Insert_Tail (List, 3);

      Put ("List"); Print_List (List);

      Iterator := First (List);
      if Is_Null (Iterator) then
         Put_Line ("Error: First IS Null");
      else
         Put_Line ("ok, First is not Null");
      end if;

      if Current (Iterator) = 1 then
         Put_Line ("First is 1");
      else
         Put_Line ("Error: First is not 1");
      end if;

      Next (Iterator);
      if Current (Iterator) = 2 then
         Put_Line ("Next is 2");
      else
         Put_Line ("Error : Next is not 2");
      end if;

      Iterator := Last (List);
      if Current (Iterator) = 3 then
         Put_Line ("Last is 3");
      else
         Put_Line ("Error: Last is not 3");
      end if;

      Prev (Iterator);
      if Current (Iterator) = 2 then
         Put_Line ("Next is 2");
      else
         Put_Line ("Error : Next is not 2");
      end if;

      Finalize (List);
   end Integer_List_Iterators_Basic;
   Test_Storage_Pools.Check_Deallocated (Test_Gen_Lists_Double_Aux.Node_Storage_Pool);

   ----------
   --  List modification with Iterators

   New_Line;
   Put_Line ("Testing List modification Iterators operations.");
   Integer_List_Iterator_Copy_Before :
   declare
      use Test_Gen_Lists_Double_Aux.Integers;
      use Lists;
      use Iterators;

      List_1 : List_Type;
      List_2 : List_Type;

      Iterator_1 : Iterator_Type;
      Iterator_2 : Iterator_Type;
      Iterator_3 : Iterator_Type;
   begin
      Insert_Tail (List_1, 1);
      Insert_Tail (List_1, 2);
      Insert_Tail (List_1, 3);
      Insert_Tail (List_1, 4);
      Put ("List_1");  Print_List (List_1);

      --  Full path coverage of
      --  sal.gen.lists.double.iterators.Copy_Before, Insert

      --  Source.Tail = null
      Put_Line ("Copy_Before, empty to list_1");
      Copy_Before (List_2, None (List_2), None (List_2), List_1, None (List_1));
      Check_Print (List_2, "source");
      Check_Print (List_1, "dest");

      --  Copy_Before : Source_Current = Null, Source_First = null, Before.Current = null
      --  Insert : Prev = null, Next = null
      Put_Line ("Copy_Before, List_1 to empty");
      Copy_Before (List_1, None (List_1), None (List_1), List_2, None (List_2));
      Check_Print (List_1, "Source List_1");
      Check_Print (List_2, "Dest List_2");

      --  Copy_Before : Source_Current /= Null, Source_First /= null, Before.Current /= null
      Put_Line ("Copy_Before, partial List_1 to before tail (list_2)");
      Iterator_1 := First (List_1);
      Next (Iterator_1);
      Iterator_2 := Last (List_1);
      Prev (Iterator_2);
      Copy_Before (List_1, Iterator_1, Iterator_2, List_2, Last (List_2));
      Check_Print (List_1, "Source List_1");
      Check_Print (List_2, "Dest List_2");

      Put_Line ("Before between First, Last");
      begin
         Copy_Before (List_1, First (List_1), Last (List_1), List_2, Iterator_1);
         Put_Line ("Oops, Iterator_Error NOT raised");
      exception
      when SAL.Iterator_Error =>
         Put_Line ("Ok, Iterator_Error raised");
      end;
      Check_Print (List_1, "Source List_1");
      Check_Print (List_2, "Dest List_2");

      Put_Line ("First not before Last");
      begin
         Copy_Before (List_1, Last (List_1), First (List_1), List_2, Iterator_1);
         Put_Line ("Oops, Iterator_Error NOT raised");
      exception
      when SAL.Iterator_Error =>
         Put_Line ("Ok, Iterator_Error raised");
      end;
      Check_Print (List_1, "Source List_1");
      Check_Print (List_2, "Dest List_2");

      ----------
      --  Full path coverage of sal.gen.lists.double.iterators.Delete
      --  (list, iterator), Delete (private)

      --  Delete (private) : node.next = null, node.prev /= null
      Iterator_1 := Last (List_1);
      Put_Line ("Delete Last (List_1)," & Integer'Image (Current (Iterator_1)));
      Delete (List_1, Iterator_1);
      if Is_Null (Iterator_1) then
         Put_Line ("ok, iterator is done");
      else
         Put_Line ("Error: iterator is NOT done");
      end if;
      Check_Print (List_1, "List_1");

      --  Delete (private) : node.next /= null, node.prev = null
      Iterator_1 := First (List_1);
      Put_Line ("Delete First (List_1)," & Integer'Image (Current (Iterator_1)));
      Delete (List_1, Iterator_1);
      if Current (Iterator_1) /= 3 then
         Put_Line ("Error: iterator did not advance");
      else
         Put_Line ("ok, iterator did advance");
      end if;
      Check_Print (List_1, "List_1");

      --  Delete (private) : node.next /= null, node.prev /= null
      Iterator_1 := First (List_1);
      Next (Iterator_1);
      Put_Line ("Delete Next (First (List_1))," & Integer'Image (Current (Iterator_1)));
      Delete (List_1, Iterator_1);
      if Current (Iterator_1) /= 1 then
         Put_Line ("Error: iterator did not advance");
      else
         Put_Line ("ok, iterator did advance");
      end if;
      Check_Print (List_1, "List_1");

      --  Delete (private) : null list result
      Iterator_1 := First (List_1);
      Put_Line ("Delete List_1 all");
      Delete (List_1, Iterator_1);
      Delete (List_1, Iterator_1);
      Delete (List_1, Iterator_1);
      Delete (List_1, Iterator_1);
      if Is_Empty (List_1) then
         Put_Line ("ok, list is empty");
      else
         Put_Line ("Error: list not empty");
      end if;
      if Is_Null (Iterator_1) then
         Put_Line ("ok, iterator is null");
      else
         Put_Line ("Error: iterator is not null");
      end if;
      Check_Print (List_1, "List_1");

      Put_Line ("Delete List_2 with null iterator");
      begin
         Delete (List_2, Iterator_1);
         Put_Line ("Oops, Constraint_Error NOT raised");
      exception
      when Constraint_Error =>
         Put_Line ("Ok, Constraint_Error raised");
      end;
      Check_Print (List_2, "List_2");

      Put_Line ("Testing Delete (range)");

      --  first.current = null, last.current = null, null list
      Put_Line ("Delete null range from null list");
      Iterator_1 := None (List_1);
      Iterator_2 := None (List_1);
      Delete (List_1, Iterator_1, Iterator_2);
      Check_Print (List_1, "List_1");

      --  first.current = null, last.current = null, not null list
      Put_Line ("Delete all using none, none range");
      Insert_Tail (List_1, 1);
      Insert_Tail (List_1, 2);
      Insert_Tail (List_1, 3);
      Iterator_1 := None (List_1);
      Iterator_2 := None (List_1);
      Delete (List_1, Iterator_1, Iterator_2);
      Check_Print (List_1, "List_1");

      --  first.current /= null, last.current /= null
      Put_Line ("Delete middle");
      Insert_Tail (List_1, 1);
      Insert_Tail (List_1, 2);
      Insert_Tail (List_1, 3);
      Insert_Tail (List_1, 4);
      Iterator_1 := First (List_1);
      Next (Iterator_1);
      Iterator_2 := Last (List_1);
      Prev (Iterator_2);
      Delete (List_1, Iterator_1, Iterator_2);
      Check_Print (List_1, "List_1");

      Put_Line ("First not before Last");
      Insert_Tail (List_1, 5);
      Insert_Tail (List_1, 6);
      Check_Print (List_1, "pre-delete");
      Iterator_1 := Last (List_1);
      Iterator_2 := Last (List_1);
      Prev (Iterator_2);
      Prev (Iterator_2);
      begin
         Delete (List_1, Iterator_1, Iterator_2);
         Put_Line ("Oops, Iterator_Error NOT raised");
      exception
      when SAL.Iterator_Error =>
         Put_Line ("Ok, Iterator_Error raised");
      end;
      Check_Print (List_1, "post delete");

      ----------
      --  Full path coverage of
      --  sal.gen.lists.double.iterators.Insert_After (Insert covered
      --  above)

      Put_Line ("Testing Insert_After (iterator, item)");
      Finalize (List_1);
      Insert_Tail (List_1, 1);
      Insert_Tail (List_1, 2);
      Insert_Tail (List_1, 3);
      Insert_Tail (List_1, 4);
      Check_Print (List_1, "pre-insert");

      --  after.current = null, copies = 0
      Put_Line ("insert_after (none, 5, copies => 0)");
      Insert_After (List_1, None (List_1), 5, Copies => 0);
      Check_Print (List_1, "");

      --  after.current = null, copies /= 0
      Put_Line ("insert_after (none, 5, copies => 2)");
      Insert_After (List_1, None (List_1), 5, Copies => 2);
      Check_Print (List_1, "");

      --  after.current /= null, copies = 0
      Iterator_1 := First (List_1);
      Put_Line ("insert_after (first, 6, copies => 1)");
      Insert_After (List_1, Iterator_1, 6, Copies => 1);
      Check_Print (List_1, "List_1");

      ----------
      --  Full path coverage of
      --  sal.gen.lists.double.iterators.Insert_Before (Insert covered
      --  above)
      Put_Line ("Testing Insert_Before (iterator, item)");
      Finalize (List_1);
      Insert_Tail (List_1, 1);
      Insert_Tail (List_1, 2);
      Insert_Tail (List_1, 3);
      Insert_Tail (List_1, 4);
      Check_Print (List_1, "pre-insert");

      --  Before.current = null, copies = 0
      Put_Line ("insert_Before (none, 5, copies => 0)");
      Insert_Before (List_1, None (List_1), 5, Copies => 0);
      Check_Print (List_1, "");

      --  Before.current = null, copies /= 0
      Put_Line ("insert_Before (none, 5, copies => 2)");
      Insert_Before (List_1, None (List_1), 5, Copies => 2);
      Check_Print (List_1, "");

      --  Before.current /= null, copies = 0
      Iterator_1 := First (List_1);
      Put_Line ("insert_Before (first, 6, copies => 1)");
      Insert_Before (List_1, Iterator_1, 6, Copies => 1);
      Check_Print (List_1, "List_1");

      ----------
      --  Full path coverage of
      --  sal.gen.lists.double.iterators.Splice_After
      Put_Line ("Testing Splice_After");
      Finalize (List_1);
      Check_Print (List_1, "List_1 pre-splice");

      Finalize (List_2);
      Check_Print (List_2, "List_2 pre-splice");

      --  source_first = null, source.head = null
      Put_Line ("empty source");
      Splice_After
         (Source => List_2,
          First  => None (List_2),
          Last   => None (List_2),
          Dest   => List_1,
          After  => None (List_1));
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  boundary value: source_first.prev = null, source_last.next = null
      --  path: source_first = null, source.head /= null, source_last = null
      --  Source.head = source_first, source.tail = source_last
      --  Dest_After = null, Dest_tail = null
      Put_Line ("non-empty source, empty dest");
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Splice_After
         (Source => List_2,
          First => None (List_2),
          Last => None (List_2),
          Dest => List_1,
          After => None (List_1));
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  boundary value: source_first.prev /= null, source_last.next /= null
      --  path: source_first /= null, source_last /= null
      --  Source.head = source_first, source.tail = source_last
      --  Dest_After = null, Dest_tail = null
      Put_Line ("partial non-empty source, empty dest");
      Finalize (List_1);
      Finalize (List_2);
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Iterator_1 := First (List_2);
      Next (Iterator_1);
      Iterator_2 := Last (List_2);
      Prev (Iterator_2);
      Splice_After
         (Source => List_2,
          First => Iterator_1,
          Last => Iterator_2,
          Dest => List_1,
          After => None (List_1));
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  path: source_first /= null, source_last /= null
      --  Source.head /= source_first, source.tail /= source_last
      --  Dest_After = null, Dest_tail /= null
      Put_Line ("partial non-empty source, non-empty dest");
      Finalize (List_1);
      Insert_Tail (List_1, 11);
      Insert_Tail (List_1, 12);
      Check_Print (List_1, "List_1 pre-splice");
      Finalize (List_2);
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Iterator_1 := First (List_2);
      Next (Iterator_1);
      Iterator_2 := Last (List_2);
      Prev (Iterator_2);
      Splice_After
         (Source => List_2,
          First  => Iterator_1,
          Last   => Iterator_2,
          Dest   => List_1,
          After  => None (List_1));
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  source_first /= null, source_last /= null
      --  Source.head /= source_first, source.tail /= source_last
      --  Dest_After /= null
      --  boundary : dest_after /= dest.tail
      Put_Line ("after non-tail");
      Finalize (List_1);
      Insert_Tail (List_1, 11);
      Insert_Tail (List_1, 12);
      Check_Print (List_1, "List_1 pre-splice");
      Iterator_3 := First (List_1);
      Finalize (List_2);
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Iterator_1 := First (List_2);
      Next (Iterator_1);
      Iterator_2 := Last (List_2);
      Prev (Iterator_2);
      Splice_After
         (Source => List_2,
          First => Iterator_1,
          Last => Iterator_2,
          Dest => List_1,
          After => Iterator_3);
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  source_first /= null, source_last /= null
      --  Source.head /= source_first, source.tail /= source_last
      --  Dest_After /= null
      --  boundary : dest_after = dest.tail
      Put_Line ("after tail");
      Finalize (List_1);
      Insert_Tail (List_1, 11);
      Insert_Tail (List_1, 12);
      Check_Print (List_1, "List_1 pre-splice");
      Iterator_3 := First (List_1);
      Next (Iterator_3);
      Finalize (List_2);
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Iterator_1 := First (List_2);
      Next (Iterator_1);
      Iterator_2 := Last (List_2);
      Prev (Iterator_2);
      Splice_After
         (Source => List_2,
          First => Iterator_1,
          Last => Iterator_2,
          Dest => List_1,
          After => Iterator_3);
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      ----------
      --  Full path coverage of
      --  sal.gen.lists.double.iterators.Splice_Before

      Put_Line ("Testing Splice_Before");
      Finalize (List_1);
      Check_Print (List_1, "List_1 pre-splice");

      Finalize (List_2);
      Check_Print (List_2, "List_2 pre-splice");

      --  source_first = null, source.head = null
      Put_Line ("empty source");
      Splice_Before
         (Source => List_2,
          First  => None (List_2),
          Last   => None (List_2),
          Dest   => List_1,
          Before => None (List_1));
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  boundary value: source_first.prev = null, source_last.next = null
      --  path: source_first = null, source.head /= null, source_last = null
      --  Source.head = source_first, source.tail = source_last
      --  Dest_Before = null, Dest_head = null
      Put_Line ("non-empty source, empty dest");
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Splice_Before
         (Source => List_2,
          First  => None (List_2),
          Last   => None (List_2),
          Dest   => List_1,
          Before => None (List_1));
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  boundary value: source_first.prev /= null, source_last.next /= null
      --  path: source_first /= null, source_last /= null
      --  Source.head = source_first, source.tail = source_last
      --  Dest_Before = null, Dest_head = null
      Put_Line ("partial non-empty source, empty dest");
      Finalize (List_1);
      Finalize (List_2);
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Iterator_1 := First (List_2);
      Next (Iterator_1);
      Iterator_2 := Last (List_2);
      Prev (Iterator_2);

      Splice_Before
         (Source => List_2,
          First  => Iterator_1,
          Last   => Iterator_2,
          Dest   => List_1,
          Before => None (List_1));
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  path: source_first /= null, source_last /= null
      --  Source.head /= source_first, source.tail /= source_last
      --  Dest_Before = null, Dest_head /= null
      Put_Line ("partial non-empty source, non-empty dest");
      Finalize (List_1);
      Insert_Tail (List_1, 11);
      Insert_Tail (List_1, 12);
      Check_Print (List_1, "List_1 pre-splice");
      Finalize (List_2);
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Iterator_1 := First (List_2);
      Next (Iterator_1);
      Iterator_2 := Last (List_2);
      Prev (Iterator_2);
      Splice_Before
         (Source => List_2,
          First => Iterator_1,
          Last => Iterator_2,
          Dest => List_1,
          Before => None (List_1));
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  source_first /= null, source_last /= null
      --  Source.head /= source_first, source.tail /= source_last
      --  Dest_Before /= null
      --  boundary : dest_Before /= dest.head
      Put_Line ("Before non-head");
      Finalize (List_1);
      Insert_Tail (List_1, 11);
      Insert_Tail (List_1, 12);
      Check_Print (List_1, "List_1 pre-splice");
      Iterator_3 := First (List_1);
      Next (Iterator_3);
      Finalize (List_2);
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Iterator_1 := First (List_2);
      Next (Iterator_1);
      Iterator_2 := Last (List_2);
      Prev (Iterator_2);
      Splice_Before
         (Source => List_2,
          First => Iterator_1,
          Last => Iterator_2,
          Dest => List_1,
          Before => Iterator_3);
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      --  source_first /= null, source_last /= null
      --  Source.head /= source_first, source.tail /= source_last
      --  Dest_Before /= null
      --  boundary : dest_Before = dest.head
      Put_Line ("Before Head");
      Finalize (List_1);
      Insert_Tail (List_1, 11);
      Insert_Tail (List_1, 12);
      Check_Print (List_1, "List_1 pre-splice");
      Iterator_3 := First (List_1);
      Finalize (List_2);
      Insert_Tail (List_2, 1);
      Insert_Tail (List_2, 2);
      Insert_Tail (List_2, 3);
      Insert_Tail (List_2, 4);
      Check_Print (List_2, "List_2 pre-splice");
      Iterator_1 := First (List_2);
      Next (Iterator_1);
      Iterator_2 := Last (List_2);
      Prev (Iterator_2);
      Splice_Before
         (Source => List_2,
          First => Iterator_1,
          Last => Iterator_2,
          Dest => List_1,
          Before => Iterator_3);
      Check_Print (List_1, "Dest List_1");
      Check_Print (List_2, "source List_2");

      Finalize (List_1);
      Finalize (List_2);
   end Integer_List_Iterator_Copy_Before;
   Test_Storage_Pools.Check_Deallocated (Test_Gen_Lists_Double_Aux.Node_Storage_Pool);

   ------------
   --  non-scalar list types
   --
   --  There are two differences in the body of sal.gen.lists.double
   --  for these types: Free_Item and Copy are not null. Free_Item is
   --  only called from Delete (private), we need to do at least one
   --  deletion. This mostly shows that you can instantiate more
   --  complex types.

   Put_Line ("Indefinite items (Symbols)");
   Symbol_Lists :
   declare
      use Test_Gen_Lists_Double_Aux.Symbols;
      use Lists;
      use Iterators;

      List : List_Type;
      Iterator : Iterator_Type;
   begin
      Put_Line ("Appending some Symbols");
      Insert_Tail (List, (Floating_Point, 6));
      Insert_Tail (List, (Floating_Point, 5));
      Insert_Tail (List, (Discrete, -4, 4));
      Insert_Tail (List, (Discrete, -3, 3));
      Check_Print (List, "Symbols");
      Put_Line ("deleting last");
      Delete_Tail (List);
      Check_Print (List, "Symbols");
      Put_Line ("deleting second");
      Iterator := First (List);
      Next (Iterator);
      Delete (List, Iterator);
      Check_Print (List, "Symbols");
      Finalize (List);
   end Symbol_Lists;
   Put_Line ("Symbols list finalized");
   Test_Storage_Pools.Check_Deallocated (Test_Gen_Lists_Double_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Gen_Lists_Double_Aux.Symbols.Storage_Pool);

   ------------
   --  Puppets

   Put_Line ("Indefinite limited items (puppets)");
   Puppet_Lists :
   declare
      use Test_Gen_Lists_Double_Aux.Puppets;
      use Lists;
      use Iterators;

      List : List_Type;
      Iterator : Iterator_Type;
   begin
      Put_Line ("Appending some puppets");
      Insert_Tail (List, ("Muppet 2 5"));
      Insert_Tail (List, ("Muppet 3 6"));
      Insert_Tail (List, ("Beanie 4"));
      Insert_Tail (List, ("Beanie 6"));
      Check_Print (List, "Beanies");
      Put_Line ("deleting last");
      Delete_Tail (List);
      Check_Print (List, "Beanies");
      Put_Line ("deleting second");
      Iterator := First (List);
      Next (Iterator);
      Delete (List, Iterator);
      Check_Print (List, "Beanies");
      Finalize (List);
   end Puppet_Lists;
   Put_Line ("Puppets list finalized");
   Test_Storage_Pools.Check_Deallocated (Test_Gen_Lists_Double_Aux.Node_Storage_Pool);
   Test_Storage_Pools.Check_Deallocated (Test_Gen_Lists_Double_Aux.Puppets.Storage_Pool);
end Test_Gen_Lists_Double;
