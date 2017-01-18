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

with AUnit.Test_Results.Text_Reporter;
with AUnit.Test_Suites;
with SAL.AUnit.Test;
with SAL.AUnit.Text_IO.Test;
with SAL.CSV.Test;
with SAL.File_Names.Test;
with SAL.Intel_Hex_IO.Test;
with Test.Config_Files.All_Suite;
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
procedure Test_All_Harness
is
   use AUnit.Test_Suites;

   Suite  : constant Access_Test_Suite := new Test_Suite;

   Result : AUnit.Test_Results.Result;

begin
   --  This is first because it's a suite.
   Add_Test (Suite, Test.Config_Files.All_Suite);

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

   Run (Suite.all, Result);

   AUnit.Test_Results.Text_Reporter.Report (Result);

end Test_All_Harness;
