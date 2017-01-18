--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2004, 2005, 2008 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with AUnit.Assertions;

with SAL.Gen.Queues.Gen_Bounded_Nonlimited.Gen_Test;
package body Test_Gen_Queues_Bounded_Nonlimited
is
   package Integer_Queues is new SAL.Gen.Queues.Gen_Bounded_Nonlimited (Integer);
   package Integer_Queues_Test is new Integer_Queues.Gen_Test;
   use Integer_Queues;

   Queue : Queue_Type (5);
   Item  : Integer;

   procedure Check
     (Label    : in String;
      Exp_Head : in Integer;
      Exp_Tail : in Integer)
   is
      use AUnit.Assertions;
      use Integer_Queues_Test;
      Head : constant Integer := Get_Head (Queue);
      Tail : constant Integer := Get_Tail (Queue);
   begin
      Assert (Head = Exp_Head, Label & " Head; expected" & Integer'Image (Exp_Head) & " got" & Integer'Image (Head));
      Assert (Tail = Exp_Tail, Label & " Tail; expected" & Integer'Image (Exp_Tail) & " got" & Integer'Image (Tail));
   end Check;

   procedure Check
     (Label    : in String;
      Exp_Head : in Integer;
      Exp_Tail : in Integer;
      Exp_Item : in Integer)
   is
      use AUnit.Assertions;
      use Integer_Queues_Test;
   begin
      Assert (Item = Exp_Item, Label & " Item; expected" & Integer'Image (Exp_Item) & " got" & Integer'Image (Item));
      Check (Label, Exp_Head, Exp_Tail);
   end Check;

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Add (Queue, 1);
      Check ("1", Exp_Head => 1, Exp_Tail => 1);
      Remove (Queue, Item);
      Check ("2", Exp_Head => 1, Exp_Tail => 1, Exp_Item => 1);

      Add (Queue, 1);
      Add (Queue, 2);
      Add (Queue, 3);
      Add (Queue, 4);
      Check ("1234", Exp_Head => 1, Exp_Tail => 4);

      Remove (Queue, Item);
      Check ("234", Exp_Head => 2, Exp_Tail => 4, Exp_Item => 1);

      Add (Queue, 5);
      Check ("2345", Exp_Head => 2, Exp_Tail => 5);

      Add (Queue, 6);
      Check ("23456", Exp_Head => 2, Exp_Tail => 1);
      Remove (Queue, Item);
      Check ("3456", Exp_Head => 3, Exp_Tail => 1, Exp_Item => 2);
   end Nominal;

   procedure Errors (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
   begin
      Clear (Queue);
      begin
         Remove (Queue, Item);
         Assert (False, "didn't get exception for Remove on empty");
      exception
      when SAL.Container_Empty =>
         Assert (True, "");
      end;

      Add (Queue, 1);
      Add (Queue, 2);
      Add (Queue, 3);
      Add (Queue, 4);
      Add (Queue, 5);
      begin
         Add (Queue, 6);
         Assert (False, "didn't get exception for add on full");
      exception
      when SAL.Container_Full =>
         Assert (True, "");
      end;
   end Errors;

   procedure Overwrite (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      Queue : Queue_Type (5);
      Item  : Integer;

      procedure Check
        (Label    : in String;
         Exp_Head : in Integer;
         Exp_Tail : in Integer;
         Exp_Item : in Integer)
      is
         use Integer_Queues_Test;
         Head : constant Integer := Get_Head (Queue);
         Tail : constant Integer := Get_Tail (Queue);
      begin
         Assert
           (Head = Exp_Head, Label & " Head; expected" & Integer'Image (Exp_Head) & " got" & Integer'Image (Head));
         Assert
           (Tail = Exp_Tail, Label & " Tail; expected" & Integer'Image (Exp_Tail) & " got" & Integer'Image (Tail));
         Assert
           (Item = Exp_Item, Label & " Item; expected" & Integer'Image (Exp_Item) & " got" & Integer'Image (Item));
      end Check;
   begin
      Set_Overflow_Handling (Queue, SAL.Overwrite);
      Clear (Queue);

      Add (Queue, 1);
      Add (Queue, 2);
      Add (Queue, 3);
      Add (Queue, 4);
      Add (Queue, 5);
      begin
         Add (Queue, 6);
         Remove (Queue, Item);
         Check ("2", Exp_Head => 3, Exp_Tail => 1, Exp_Item => 2);
         Remove (Queue, Item);
         Check ("3", Exp_Head => 4, Exp_Tail => 1, Exp_Item => 3);
         Remove (Queue, Item);
         Check ("4", Exp_Head => 5, Exp_Tail => 1, Exp_Item => 4);
         Remove (Queue, Item);
         Check ("5", Exp_Head => 1, Exp_Tail => 1, Exp_Item => 5);
         Remove (Queue, Item);
         Check ("6", Exp_Head => 1, Exp_Tail => 1, Exp_Item => 6);
      exception
      when SAL.Container_Full =>
         Assert (False, "got exception for add on full");
      end;
   end Overwrite;

   ----------
   --  Public subprograms

   function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("SAL.Gen_Queues_Bounded_Nonlimited");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Errors'Access, "Errors");
      Register_Routine (T, Overwrite'Access, "Overwrite");
   end Register_Tests;

end Test_Gen_Queues_Bounded_Nonlimited;
