--  Abstract :
--
--  test SAL.Gen_Sets
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
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
with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with SAL.Gen_Sets;
package body Test_Gen_Sets is

   type Field_Type is (Day, Month, Year, Hour, Minute, Seconds, AM_PM);

   package Field_Sets is new SAL.Gen_Sets (Field_Type);
   use Field_Sets;

   --  Test cases

   procedure Test_All (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Set : constant Field_Sets.Set_Type :=
        (Day                                            => False,
         Month | Year | Hour | Minute | Seconds | AM_PM => True);
   begin
      AUnit.Assertions.Assert (not All_Set (Set), "Day");

      AUnit.Assertions.Assert (All_Set (Set (Month .. Seconds)), "month .. seconds");
   end Test_All;

   procedure Test_Any (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Set : constant Field_Sets.Set_Type :=
        (Day                                            => True,
         Month | Year | Hour | Minute | Seconds | AM_PM => False);
   begin
      AUnit.Assertions.Assert (Any (Set), "Day");

      AUnit.Assertions.Assert (not Any (Set (Month .. Seconds)), "month .. seconds");
   end Test_Any;

   procedure Test_First (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert
        (Day = First
           ((Day              => True,
             Month | Year | Hour => False)),
         "Day");

      AUnit.Assertions.Assert
        (Month = First
           ((Month       => True,
             Year | Hour => False)),
         "Month");

   end Test_First;

   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test SAL.Gen_Sets");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_All'Access, "all");
      Register_Routine (T, Test_Any'Access, "any");
      Register_Routine (T, Test_First'Access, "first");
   end Register_Tests;

end Test_Gen_Sets;
