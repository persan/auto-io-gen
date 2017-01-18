--  Abstract:
--
--  see spec
--
--  Copyright (C) 2007 - 2008 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with SAL.Gen_Math.Gen_Manipulator.Gen_Wertz;
with SAL.Math_Double.AUnit;
with SAL.Math_Double.DOF_3.Wertz; use SAL.Math_Double.DOF_3.Wertz;
with SAL.Math_Double.DOF_6.AUnit;
with SAL.Math_Double.DOF_6.DC_Array_DCV_Inverse;
with SAL.Math_Double.DOF_6.Wertz;
with SAL.Math_Double.Den_Hart.Wertz;
with SAL.Math_Double.Elementary;
with SAL.Math_Double.Scalar; use SAL.Math_Double.Scalar;
package body Test_Math_Double_Manipulator_3_Wertz is

   use SAL.Math_Double;
   use SAL.Math_Double.DOF_3;
   use SAL.Math_Double.DOF_6;

   type Joint_Index_Type          is range 0 .. 2;
   type Joint_Array_Real_Type     is array (Joint_Index_Type) of Real_Type;
   type Joint_Array_Pose_Type     is array (Joint_Index_Type) of DOF_6.Pose_Type;
   type Joint_Array_Mass_Type     is array (Joint_Index_Type) of DOF_6.Mass_Type;
   type Joint_Array_Den_Hart_Type is array (Joint_Index_Type) of Den_Hart.Den_Hart_Type;

   package Manip_3 is new SAL.Math_Double.Gen_Manipulator
     (Elementary                      => SAL.Math_Double.Elementary,
      Math_Scalar                     => SAL.Math_Double.Scalar,
      Math_DOF_3                      => SAL.Math_Double.DOF_3,
      Math_DOF_6                      => SAL.Math_Double.DOF_6,
      Math_Den_Hart                   => SAL.Math_Double.Den_Hart,
      Math_DOF_6_DC_Array_DCV_Inverse => SAL.Math_Double.DOF_6.DC_Array_DCV_Inverse,
      Joint_Index_Type                => Joint_Index_Type,
      Joint_Array_Real_Type           => Joint_Array_Real_Type,
      Joint_Array_Pose_Type           => Joint_Array_Pose_Type,
      Joint_Array_Mass_Type           => Joint_Array_Mass_Type,
      Joint_Array_Den_Hart_Type       => Joint_Array_Den_Hart_Type);

   package Manip_3_Wertz is new Manip_3.Gen_Wertz
     (Math_DOF_3_Wertz    => SAL.Math_Double.DOF_3.Wertz,
      Math_DOF_6_Wertz    => SAL.Math_Double.DOF_6.Wertz,
      Math_Den_Hart_Wertz => SAL.Math_Double.Den_Hart.Wertz);

   use Manip_3;

   --  Geometry of a typical high-gain antenna. Joint 0 is the
   --  deployment joint, link 0 is a straight rod, joints 1 and 2
   --  intersect, the antenna is mounted on link 2. Links 1 and 2 have
   --  very small mass; the antenna is fairly large.

   Nominal_Position : constant Joint_Array_Real_Type := (0.0, 0.0, 0.0);

   Dh_Pose_Base : constant Pose_Type := ((0.0, 0.0, 0.0), To_Unit_Quaternion (Pi, Z));

   Geometry : constant Joint_Array_Den_Hart_Type :=
     (0             =>
        (Class      => SAL.Math_Double.Den_Hart.Revolute,
         A          => 0.0,
         Trig_Alpha => Sin_Cos (-Half_Pi),
         D          => 0.0),
      1             =>
        (Class      => SAL.Math_Double.Den_Hart.Revolute,
         A          => 2.0,
         Trig_Alpha => Sin_Cos (0.0),
         D          => 0.0),
      2             =>
        (Class      => SAL.Math_Double.Den_Hart.Revolute,
         A          => 0.0,
         Trig_Alpha => Sin_Cos (Half_Pi),
         D          => 0.0));

   Tool_Pose_Last : constant Pose_Type := ((0.5, 0.0,  0.0), Zero_Unit_Quaternion);

   Mass_Link : constant Joint_Array_Mass_Type :=
     (0 => To_Mass (5.0, (1.0, 0.0, 0.0), (1.0, 10.0, 10.0, 0.0, 0.0, 0.0)),
      1 => To_Mass (0.1, (0.0, 0.0, 0.0), (0.1, 0.1, 0.1, 0.0, 0.0, 0.0)),
      2 => To_Mass (0.1, (0.0, 0.0, 0.0), (0.1, 0.1, 0.1, 0.0, 0.0, 0.0)));

   Tool_Mass_Tool : constant Mass_Type := To_Mass (2.0, (0.5, 0.0, 0.0), (2.0, 2.0, 2.0, 0.0, 0.0, 0.0));

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double.DOF_6.AUnit;

      Tool_Pose_Base  : Pose_Type;
      Total_Mass_Base : Mass_Type;
   begin
      --  First do a simpler case
      Manip_3_Wertz.Slow_Mass_Pose
        (Joint           => Nominal_Position,
         DH_Pose_Base    => Zero_Pose,
         Den_Hart        => Geometry,
         Tool_Pose_Last  => Zero_Pose,
         Mass_Link       => Mass_Link,
         Tool_Mass_Tool  => Zero_Mass,
         Tool_Pose_Base  => Tool_Pose_Base,
         Total_Mass_Base => Total_Mass_Base);

      Check ("simple pose", Tool_Pose_Base, ((2.0, 0.0, 0.0), Zero_Unit_Quaternion));
      Check
        ("simple mass",
         Total_Mass_Base,
         To_Mass (5.2, (1.038_462, 0.0, 0.0), (1.2, 10.39_231, 10.39_231, 0.0, 0.0, 0.0)));

      Manip_3_Wertz.Slow_Mass_Pose
        (Joint           => Nominal_Position,
         DH_Pose_Base    => Dh_Pose_Base,
         Den_Hart        => Geometry,
         Tool_Pose_Last  => Tool_Pose_Last,
         Mass_Link       => Mass_Link,
         Tool_Mass_Tool  => Tool_Mass_Tool,
         Tool_Pose_Base  => Tool_Pose_Base,
         Total_Mass_Base => Total_Mass_Base);

      Check ("full pose", Tool_Pose_Base, ((-2.5, 0.0, 0.0), To_Unit_Quaternion (Pi, Z)));
      Check
        ("full mass",
         Total_Mass_Base,
         To_Mass (7.2, (-1.583_333, 0.0, 0.0), (3.2, 17.95, 17.95, 0.0, 0.0, 0.0)));
   end Nominal;

   ----------
   --  Public routines

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Double_Manipulator_3_Wertz");
   end Name;

   procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Double.AUnit.Default_Tolerance := 0.000_01;
   end Set_Up_Case;

   procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Double.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Double_Manipulator_3_Wertz;
