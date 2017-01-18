--  Abstract :
--
--  Test SAL.Gen_Math.Gen_Manipulator, using the
--  SAL.Math_Float.Manipulator_6 instantiation and the Kraft Hand
--  Controller as an example. Null_Space_Projector is tested in
--  Test_Math_Float_Manipulator_7, because the null space of a 6 DOF
--  arm is the empty set. Slow_Inertia and Slow_Gravity_Torque are
--  also tested in Test_Math_Float_Manipulator_7, since we know the
--  mass of the RRC independently.
--
--  Copyright (C) 2003, 2005 Stephen Leake.  All Rights Reserved.
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
--

with Ada.Strings.Unbounded;
with AUnit.Test_Cases;
package Test_Math_Float_Manipulator_6_Left is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   type Test_Case_Access is access all Test_Case;

   --  Override:

   procedure Register_Tests (T : in out Test_Case);

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access;

   procedure Set_Up_Case (Test : in out Test_Case);

   procedure Tear_Down_Case (Test : in out Test_Case);

end Test_Math_Float_Manipulator_6_Left;
