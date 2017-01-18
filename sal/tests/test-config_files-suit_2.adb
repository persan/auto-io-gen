with Test_Gen_FIFO;
with Test_Gen_Queues_Bounded_Nonlimited;
with Test_Gen_Sets;
with Test_Math_Double_Cubic;
with Test_Math_Double_DOF_6;
with Test_Math_Double_Manipulator_3_Wertz;
with Test_Math_Double_Tank;
with Test_Math_Float_DOF_3;
with Test_Math_Float_DOF_3_Left;
with Test_Math_Float_DOF_3_Wertz;
with Test_Math_Float_DOF_6;
with Test_Math_Float_DOF_6_Config;
with Test_Math_Float_DOF_6_Integrator_Utils_Left;
with Test_Math_Float_DOF_6_Integrator_Utils_Wertz;
with Test_Math_Float_DOF_6_Left;
with Test_Math_Float_DOF_6_Wertz;
with Test_Math_Float_Den_Hart_Left;
with Test_Math_Float_Gen_Runge_Kutta_4th_Left;
with Test_Math_Float_Gen_Runge_Kutta_4th_Wertz;
with Test_Math_Float_Linear_Fit;
with Test_Math_Float_Manipulator_6_Left;
with Test_Math_Float_Manipulator_7_Left;
with Test_Math_Float_Polynomial_Fit;
with Test_Math_Float_Polynomials;
with Test_Math_Float_Polynomials_Inverse;
with Test_Math_Float_Scalar;
with Test_Math_Float_Scalar_Config;
with Test_Math_Float_Stats;
with Test_Math_Float_Vector;
with Test_Network_Order;
with Test_Poly_Unbounded_Arrays;
with Test_Poly_Unbounded_Arrays_Find_Linear;
with Test_Time_Conversions;
with SAL.AUnit.Test;
with SAL.AUnit.Text_IO.Test;
with SAL.CSV.Test;
with SAL.File_Names.Test;
with SAL.Intel_Hex_IO.Test;

function Test.Config_Files.Suit_2 return AUnit.Test_Suites.Access_Test_Suite
is
   use AUnit.Test_Suites;

   use Test.Config_Files;

   Suite : constant Access_Test_Suite := new Test_Suite;

begin
   --  This is before others because it has an Unbounded_Array that is
   --  initialized at elaboration time; other test that use
   --  Test_Storage_Pools will report non-zero initial allocations.
   Add_Test (Suite, new Test_Poly_Unbounded_Arrays_Find_Linear.Test_Case (Debug_Level => 0));

   Add_Test (Suite, new SAL.AUnit.Test.Test_Case);
   Add_Test (Suite, new SAL.AUnit.Text_IO.Test.Test_Case);
   Add_Test (Suite, new SAL.CSV.Test.Test_Case);
   Add_Test (Suite, new SAL.File_Names.Test.Test_Case);
   Add_Test (Suite, new SAL.Intel_Hex_IO.Test.Test_Case);
   Add_Test (Suite, new Test_Gen_FIFO.Test_Case);
   Add_Test (Suite, new Test_Gen_Queues_Bounded_Nonlimited.Test_Case);
   Add_Test (Suite, new Test_Gen_Sets.Test_Case);
   Add_Test (Suite, new Test_Math_Double_Cubic.Test_Case);
   Add_Test (Suite, new Test_Math_Double_DOF_6.Test_Case);
   Add_Test (Suite, new Test_Math_Double_Manipulator_3_Wertz.Test_Case);
   Add_Test (Suite, new Test_Math_Double_Tank.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_3.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_3_Left.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_3_Wertz.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_6.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_6_Config.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_6_Left.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_6_Integrator_Utils_Left.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_6_Integrator_Utils_Wertz.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_6_Wertz.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Den_Hart_Left.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Gen_Runge_Kutta_4th_Left.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Gen_Runge_Kutta_4th_Wertz.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Linear_Fit.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Manipulator_6_Left.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Manipulator_7_Left.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Polynomial_Fit.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Polynomials.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Polynomials_Inverse.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Scalar_Config.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Scalar.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Stats.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Vector.Test_Case);
   Add_Test (Suite, new Test_Network_Order.Test_Case);
   Add_Test (Suite, new Test_Poly_Unbounded_Arrays.Test_Case (Debug_Level => 0));
   Add_Test (Suite, new Test_Time_Conversions.Test_Case);

   return Suite;
end Test.Config_Files.Suit_2;
