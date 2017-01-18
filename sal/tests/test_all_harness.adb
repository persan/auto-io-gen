--  Abstract :
--
--  Run all AUnit tests for SAL. See Build/*/Makefile for non-AUnit
--  tests.
--
--  Copyright (C) 2003 - 2008 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Reporter.Text;
with AUnit.Test_Suites;
with Test.Config_Files.All_Suite;

with AUnit.Test_Results;
with AUnit.Options;
procedure Test_All_Harness
is
   use AUnit.Test_Suites;

   Suite  : constant Access_Test_Suite := new Test_Suite;

   Result : AUnit.Test_Results.Result;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Outcome :  AUnit.Status;
begin
   --  This is first because it's a suite.
   Add_Test (Suite, Test.Config_Files.All_Suite);



   Run (Suite, AUnit.Options.Default_Options, Result, Outcome);

   Reporter.Report (Result);

end Test_All_Harness;
