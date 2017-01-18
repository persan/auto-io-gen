--  Abstract :
--
--  See spec
--
--  Copyright (C) 2006 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with SAL.AUnit;
with AUnit.Assertions;
package body SAL.AUnit.Test is

   ----------
   --  Test subprograms
   --
   --  We only test things that are complicated

   procedure Test_Fixed (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Small : constant := 0.1;
      type A_Fixed_Type is delta Small range -2**15 * Small .. (2**15-1) * Small;
      for A_Fixed_Type'Small use Small;
      for A_Fixed_Type'Size use 16;

      procedure Check_Fixed is new Gen_Check_Fixed (A_Fixed_Type);

      procedure Fail
        (Label     : in String;
         Computed  : in A_Fixed_Type;
         Expected  : in A_Fixed_Type;
         Tolerance : in A_Fixed_Type)
      is begin
         Check_Fixed ("", Computed, Expected, Tolerance);
         Standard.AUnit.Assertions.Assert (False, "Fail " & Label);
      exception
      when Standard.AUnit.Assertions.Assertion_Error =>
         --  test passed.
         null;
      end Fail;

   begin

      --  These should all pass
      Check_Fixed ("'Last", A_Fixed_Type'Last, 3276.7, 0.0);
      Check_Fixed ("'Last +- 0.1 -", A_Fixed_Type'Last - 0.1, 3276.7, 0.1);
      Check_Fixed ("1.0", 1.0, 1.0, 0.0);
      Check_Fixed ("-1.0", -1.0, -1.0, 0.0);
      Check_Fixed ("1.0 +- 0.1 +", 1.1, 1.0, 0.1);
      Check_Fixed ("1.0 +- 0.1 -", 0.9, 1.0, 0.1);
      Check_Fixed ("-1.0 +- 0.1 +", -0.9, -1.0, 0.1);
      Check_Fixed ("-1.0 +- 0.1 -", -1.1, -1.0, 0.1);
      Check_Fixed ("'First", A_Fixed_Type'First, -3276.8, 0.0);
      Check_Fixed ("'First +- 0.1 +", A_Fixed_Type'First + 0.1, -3276.8, 0.1);

      --  These should fail
      Fail ("'Last", A_Fixed_Type'Last, 3276.6, 0.0);
      Fail ("2.0", 2.0, 1.0, 0.0);
      Fail ("1.0", 1.0, 2.0, 0.0);
      Fail ("-1.0", -1.0, -2.0, 0.0);
      Fail ("-2.0", -2.0, -1.0, 0.0);
      Fail ("'First", A_Fixed_Type'First, -3276.6, 0.0);

   end Test_Fixed;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("SAL.AUnit.Test");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Fixed'Access, "Test_Fixed");
   end Register_Tests;

end SAL.AUnit.Test;
