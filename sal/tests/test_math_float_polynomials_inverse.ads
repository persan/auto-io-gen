--  Abstract :
--
--  Test SAL.Gen_Math.Gen_Polynomials.gen_inverse
--

with Ada.Strings.Unbounded;
with AUnit.Test_Cases;
package Test_Math_Float_Polynomials_Inverse is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   type Test_Case_Access is access all Test_Case;

   --  Override:

   procedure Register_Tests (T : in out Test_Case);

   function Name (T : Test_Case) return AUnit.Message_String;

end Test_Math_Float_Polynomials_Inverse;
