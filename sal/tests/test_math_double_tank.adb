--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2005 - 2007 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);


with SAL.Math_Double.AUnit;
with SAL.Math_Double.Tank;
with Ada.Numerics; use Ada.Numerics;
package body Test_Math_Double_Tank is

   procedure Test_Sphere (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double;
      use SAL.Math_Double.AUnit;
      use SAL.Math_Double.Tank;

      Rho : constant Real_Type := 1000.0;

      Radius         : Real_Type;
      Center_Inertia : Real_Type;

      Expected_Radius : constant Real_Type := 0.5;

      Mass : constant Real_Type := 4.0 / 3.0 * Pi * Rho * Expected_Radius**3;

      Expected_Center_Inertia : constant Real_Type := 0.4 * Mass * Expected_Radius**2;
   begin
      Compute_Sphere
        (Rho            => Rho,
         Mass           => Mass,
         Radius         => Radius,
         Center_Inertia => Center_Inertia);

      Check ("Radius", Radius, Radius);
      Check ("Center_Inertia", Center_Inertia, Expected_Center_Inertia);
   end Test_Sphere;

   procedure Test_Sphere_Sect_Height (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double;
      use SAL.Math_Double.AUnit;
      use SAL.Math_Double.Tank;

      Rho : constant Real_Type := 1000.0;
      R   : constant Real_Type := 0.5;

      H_Computed : Real_Type;
      H_Expected : Real_Type;
      CM_Z       : Real_Type;
      I_zz       : Real_Type;
      I_xx       : Real_Type;

      function Mass (H : in Real_Type) return Real_Type
      is begin
         return Pi * Rho * (3.0 * H**2 * R - H**3) / 3.0;
      end Mass;

   begin
      --  Show that the Height computation for spherical section is
      --  correct, by computing the first mass from the height.
      --
      --  See other tests for mass moments.

      for I in 0 .. 10 loop
         H_Expected := 2.0 * R * Real_Type (I) / 10.0;
         Compute_Sphere_Sect (Rho, R, Mass (H_Expected), H_Computed, CM_Z, I_zz, I_xx);
         Check (Integer'Image (I), H_Computed, H_Expected);
      end loop;
   end Test_Sphere_Sect_Height;

   procedure Test_Sphere_Sect_Moments (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Math_Double;
      use SAL.Math_Double.AUnit;
      use SAL.Math_Double.Tank;

      Rho : constant Real_Type := 1000.0;
      R   : constant Real_Type := 0.5;

      H        : Real_Type;
      CM_Z     : Real_Type;
      I_zz     : Real_Type;
      I_xx_Bot : Real_Type;
      I_xx_CM  : Real_Type;

      Whole_Mass     : constant Real_Type := 4.0 / 3.0 * Pi * Rho * R**3;
      Whole_I_zz     : constant Real_Type := 0.4 * Whole_Mass * R**2;
      Whole_I_xx_Bot : constant Real_Type := (0.4 + 1.0) * Whole_Mass * R**2;
      Whole_I_xx_CM  : constant Real_Type := Whole_I_zz;

      Half_Mass     : constant Real_Type := 0.5 * Whole_Mass;
      Half_CM_Z     : constant Real_Type := 5.0 / 8.0 * R;
      Half_I_zz     : constant Real_Type := 0.5 * Whole_I_zz;
      Half_I_xx_Bot : constant Real_Type := 13.0 * Half_Mass * R**2 / 20.0;
      Half_I_xx_CM  : constant Real_Type := 83.0 * Half_Mass * R**2 / 320.0;
   begin
      --  We can easily compute the values for a half and whole sphere.

      Compute_Sphere_Sect (Rho, R, Whole_Mass, H, CM_Z, I_zz, I_xx_Bot);

      I_xx_CM := I_Non_Sym_CM (Whole_Mass, CM_Z, I_xx_Bot);

      Check ("Whole height", H, 2.0 * R);
      Check ("Whole cm_z", CM_Z, R);
      Check ("Whole I_zz", I_zz, Whole_I_zz);
      Check ("Whole I_xx_Bot", I_xx_Bot, Whole_I_xx_Bot);
      Check ("Whole I_xx_CM", I_xx_CM, Whole_I_xx_CM);

      Compute_Sphere_Sect (Rho, R, Half_Mass, H, CM_Z, I_zz, I_xx_Bot);

      I_xx_CM := I_Non_Sym_CM (Half_Mass, CM_Z, I_xx_Bot);

      Check ("Half height", H, R);
      Check ("Half cm_z", CM_Z, Half_CM_Z);
      Check ("Half I_zz", I_zz, Half_I_zz);
      Check ("Half I_xx_Bot", I_xx_Bot, Half_I_xx_Bot);
      Check ("Half I_xx_CM", I_xx_CM, Half_I_xx_CM);

   end Test_Sphere_Sect_Moments;

   ----------
   --  Public routines

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Sphere'Access, "Test_Sphere");
      Register_Routine (T, Test_Sphere_Sect_Height'Access, "Test_Sphere_Sect_Height");
      Register_Routine (T, Test_Sphere_Sect_Moments'Access, "Test_Sphere_Sect_Moments");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Double_Tank");
   end Name;

   procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Double.AUnit.Default_Tolerance := 0.000_001;
   end Set_Up_Case;

   procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      SAL.Math_Double.AUnit.Default_Tolerance := 0.0;
   end Tear_Down_Case;

end Test_Math_Double_Tank;
