--  Abstract :
--
--  Run one test, while working on it.

with AUnit.Test_Results.Text_Reporter;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with SAL.AUnit.Test;
procedure Test_One_Harness
is
   Suite  : constant Access_Test_Suite := new Test_Suite;
   Result : AUnit.Test_Results.Result;

begin
   Add_Test (Suite, new SAL.AUnit.Test.Test_Case);

   Run (Suite.all, Result);

   AUnit.Test_Results.Text_Reporter.Report (Result);

end Test_One_Harness;
