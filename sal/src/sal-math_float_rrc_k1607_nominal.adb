--  Abstract:
--
--  see spec
--
--  References:
--
--  [1] "Introduction to Robotics Mechanics & Control" John J. Craig,
--      Addison-Wesley, 1986
--
--  [2] "Robotics Research Corporation K-1607 Mass, CG & Inertia Data", January
--      15, 1990
--
--  Copyright (C) 1994, 2005, 2007 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

--  Modification History:
--
--   9 Dec 1993 Stephe Leake    Created
--   3 Jun 1994 Stephe Leake
--      fix J2 mass; add offset for J3_RF

with SAL.Math_Float.DOF_3.Left;
with SAL.Math_Float.DOF_6.Left;
with SAL.Math_Float.Den_Hart;
with SAL.Math_Float.Scalar;
package body SAL.Math_Float_RRC_K1607_Nominal is

   use SAL.Math_Float;

   --  RRC uses non-ISO units, so we have to convert.

   Earth_Gravity                          : constant := 9.80665;                           -- meters/second**2;
   Meters_Per_Inch                        : constant := 0.02540;
   Newtons_Per_Pound                      : constant := 4.44822;
   Kilograms_Per_Pound                    : constant := Newtons_Per_Pound / Earth_Gravity; -- 0.453592
   Kilogram_Meter2_Per_Pound_Inch_Second2 : constant := Newtons_Per_Pound * Meters_Per_Inch / Earth_Gravity;
   --  This comes from the equality:
   --  (1 lb inch sec^2) [(4.4 N/lb) (0.0254 M/inch) ((1 kg m/sec^2)/9.8 N)] = 1 kg m^2
   --  the value in [] is the conversion constant: 0.011521

   function To_Meters (Inches : in Real_Type) return Real_Type
   is begin
      return Inches * Meters_Per_Inch;
   end To_Meters;

   function Lb_To_Kg (Pounds : in Real_Type) return Real_Type
   is begin
      return Pounds * Kilograms_Per_Pound;
   end Lb_To_Kg;

   function Lb_In_Sec2_To_Kg_M2 (Pound_Inch_Second2 : in Real_Type) return Real_Type
   is begin
      return Pound_Inch_Second2 * Kilogram_Meter2_Per_Pound_Inch_Second2;
   end Lb_In_Sec2_To_Kg_M2;

   --   Coordinate frames are assigned using the Denavit-Hartenberg conventions
   --   [1], such that all joints are at zero when the arm is "straight out"
   --   (joint axes 1, 3, 5, 7 are parallel and at the center of their ranges;
   --   joint axes 2, 4, 6 are parallel, 2 is near the center of its range, and
   --   4 and 6 are at full extension). Joints are numbered from the base,
   --   starting with joint 1. The base frame is at the intersection of the
   --   first two joints, with +Z towards the tool, +X away from the base
   --   plate, and -Y along the second joint axis (with joint 1 at zero). The
   --   final frame is at the tool mounting plate, called the tool-plate frame.
   --   The Object frame is defined by Tp_T_Obj relative to the tool-plate
   --   frame.

   A2 : constant Real_Type := To_Meters (Inches => -5.625);       -- - 0.14288 meters
   D3 : constant Real_Type := To_Meters (Inches => 27.000);       --   0.68580 meters
   A3 : constant Real_Type := To_Meters (Inches => -4.250);       -- - 0.10795 meters
   A4 : constant Real_Type := To_Meters (Inches =>  4.250);       --   0.10795 meters
   D5 : constant Real_Type := To_Meters (Inches => 27.000);       --   0.68580 meters
   A5 : constant Real_Type := To_Meters (Inches => -3.125);       -- - 0.079375 meters
   A6 : constant Real_Type := To_Meters (Inches =>  3.125);       --   0.079375 meters
   D7 : constant Real_Type := To_Meters (Inches =>  6.600)        --   0.16764 meters
     + 0.005; -- new G10 toolplate thicker than original aluminum toolplate

   function Geometry return SAL.Math_Float_Manipulator_7.Joint_Array_Den_Hart_Type
   is
      use SAL.Math_Float.Scalar, SAL.Math_Float.Den_Hart;
   begin
      return
        (1 => (Revolute, A => 0.0, Trig_Alpha => Sin_Cos (0.0),      D => 0.0),
         2 => (Revolute, A => 0.0, Trig_Alpha => Sin_Cos (Pi / 2.0), D => 0.0),
         3 => (Revolute, A =>  A2, Trig_Alpha => Sin_Cos (-Pi/2.0), D =>  D3),
         4 => (Revolute, A =>  A3, Trig_Alpha => Sin_Cos (Pi / 2.0), D => 0.0),
         5 => (Revolute, A =>  A4, Trig_Alpha => Sin_Cos (-Pi/2.0), D =>  D5),
         6 => (Revolute, A =>  A5, Trig_Alpha => Sin_Cos (Pi / 2.0), D => 0.0),
         7 => (Revolute, A =>  A6, Trig_Alpha => Sin_Cos (-Pi/2.0), D => 0.0));
   end Geometry;

   function Tlast_T_Tp return SAL.Math_Float.DOF_6.Pose_Type
   is
   begin
      return ((0.0, 0.0, D7), SAL.Math_Float.DOF_3.Zero_Unit_Quaternion);
   end Tlast_T_Tp;

   function Mass return SAL.Math_Float_Manipulator_7.Joint_Array_Mass_Type
   is
      use SAL.Math_Float.Scalar;
      use SAL.Math_Float.DOF_3;
      use SAL.Math_Float.DOF_3.Left;
      use SAL.Math_Float.DOF_6;
      use SAL.Math_Float.DOF_6.Left;

      --  Mass values are from [2]. The pitch joint modules are broken
      --  at the joint and each piece measured independently. The roll
      --  joint modules are not; the mass of the distal (moving)
      --  portion is included in the CG and X and Y moments of inertia
      --  of the proximal link. The Z moment of inertia is given, and
      --  included in the distal link. This gives the correct total
      --  joint gravity torque and inertia, although individual
      --  bearing wrenches will be off.
      --
      --  First, we record the numbers directly from the document,
      --  adding the distal Izz from the roll modules to the
      --  appropriate pitch module clevis.

      --  The base plate and housing don't move, so we don't represent them.

      --  RRC mass values are in Pounds (sigh), and inertia numbers
      --  are in units of Pound-Inch-Second^2 (sigh^2).

      J1_Rg_Distal_Izz : constant Real_Type := Lb_In_Sec2_To_Kg_M2 (4.1);

      J2_PG_Clevis : constant Mass_Type := To_Mass
        (Total          => Lb_To_Kg (53.0),
         Center         => (0.0, To_Meters (-1.90), To_Meters (-1.0)),
         Center_Inertia =>
           (Ixx         => Lb_In_Sec2_To_Kg_M2 (5.7),
            Iyy         => Lb_In_Sec2_To_Kg_M2 (5.5) + J1_Rg_Distal_Izz,
            Izz         => Lb_In_Sec2_To_Kg_M2 (3.6),
            Ixy         => 0.0,
            Ixz         => 0.0,
            Iyz         => 0.0));

      J2_PG_Case : constant Mass_Type := To_Mass
        (Total          => Lb_To_Kg (67.5),
         Center         => (To_Meters (-0.70), To_Meters (0.40), To_Meters (1.20)),
         Center_Inertia =>
           (Ixx         => Lb_In_Sec2_To_Kg_M2 (3.6),
            Iyy         => Lb_In_Sec2_To_Kg_M2 (2.1),
            Izz         => Lb_In_Sec2_To_Kg_M2 (3.0),
            Ixy         => 0.0,
            Ixz         => 0.0,
            Iyz         => 0.0));

      J3_RF : constant Mass_Type := To_Mass
        (Total          => Lb_To_Kg (64.0),
         Center         => (0.0, 0.0, To_Meters (13.25)),
         Center_Inertia =>
           (Ixx         => Lb_In_Sec2_To_Kg_M2 (5.0),
            Iyy         => Lb_In_Sec2_To_Kg_M2 (5.0),
            Izz         => Lb_In_Sec2_To_Kg_M2 (1.4),
            Ixy         => 0.0,
            Ixz         => 0.0,
            Iyz         => 0.0));

      J3_RG_Distal_Izz : constant Real_Type := Lb_In_Sec2_To_Kg_M2 (0.8);

      J4_PE_Clevis : constant Mass_Type := To_Mass
        (Total          => Lb_To_Kg (19.0),
         Center         => (To_Meters (1.50), To_Meters (-0.90), To_Meters (-0.50)),
         Center_Inertia =>
           (Ixx         => Lb_In_Sec2_To_Kg_M2 (1.1),
            Iyy         => Lb_In_Sec2_To_Kg_M2 (1.3) + J3_RG_Distal_Izz,
            Izz         => Lb_In_Sec2_To_Kg_M2 (0.94),
            Ixy         => 0.0,
            Ixz         => 0.0,
            Iyz         => 0.0));

      J4_PE_Case : constant Mass_Type := To_Mass
        (Total          => Lb_To_Kg (25.0),
         Center         => (To_Meters (1.25), To_Meters (0.50), To_Meters (0.75)),
         Center_Inertia =>
           (Ixx         => Lb_In_Sec2_To_Kg_M2 (1.0),
            Iyy         => Lb_In_Sec2_To_Kg_M2 (1.2),
            Izz         => Lb_In_Sec2_To_Kg_M2 (0.92),
            Ixy         => 0.0,
            Ixz         => 0.0,
            Iyz         => 0.0));

      J5_RC : constant Mass_Type := To_Mass
        (Total          => Lb_To_Kg (25.5),
         Center         => (0.0, 0.0, To_Meters (11.25)),
         Center_Inertia =>
           (Ixx         => Lb_In_Sec2_To_Kg_M2 (2.07),
            Iyy         => Lb_In_Sec2_To_Kg_M2 (2.07),
            Izz         => Lb_In_Sec2_To_Kg_M2 (0.15),
            Ixy         => 0.0,
            Ixz         => 0.0,
            Iyz         => 0.0));

      J6_PC_Clevis : constant Mass_Type := To_Mass
        (Total          => Lb_To_Kg (7.5),
         Center         => (To_Meters (1.10), To_Meters (-0.70), To_Meters (-0.40)),
         Center_Inertia =>
           (Ixx         => Lb_In_Sec2_To_Kg_M2 (0.33),
            Iyy         => Lb_In_Sec2_To_Kg_M2 (0.39) + J3_RG_Distal_Izz,
            Izz         => Lb_In_Sec2_To_Kg_M2 (0.28),
            Ixy         => 0.0,
            Ixz         => 0.0,
            Iyz         => 0.0));

      J6_PC_Case : constant Mass_Type := To_Mass
        (Total          => Lb_To_Kg (15.5) - 0.5, -- see J7_TB below
         Center         => (To_Meters (1.50), To_Meters (0.35), To_Meters (0.20)),
         Center_Inertia =>
           (Ixx         => Lb_In_Sec2_To_Kg_M2 (0.58),
            Iyy         => Lb_In_Sec2_To_Kg_M2 (0.44),
            Izz         => Lb_In_Sec2_To_Kg_M2 (0.62),
            Ixy         => 0.0,
            Ixz         => 0.0,
            Iyz         => 0.0));

      J7_TB_Distal_Izz : constant Real_Type := Lb_In_Sec2_To_Kg_M2 (0.014);

      J7_TB : constant Mass_Type := To_Mass
         --  Includes only Izz and Total. Total must be non-zero for
         --  Add to allow a non-zero result, thus we
         --  include 0.5 kg of J6_PC_Case in J7_TB.
        (Total          => 0.5,
         Center         => (others => 0.0),
         Center_Inertia =>
           (Izz         => J7_TB_Distal_Izz,
            Ixx         => 0.0,
            Iyy         => 0.0,
            Ixy         => 0.0,
            Ixz         => 0.0,
            Iyz         => 0.0));

      Minus_Half_Pi_X : constant Pose_Type := (Zero_Cart_Vector, To_Unit_Quaternion (-Pi/2.0, X));

   begin
      return
        (1 => Minus_Half_Pi_X * J2_PG_Clevis,
         2 => Add (J2_PG_Case, J3_RF, ((A2, 0.0, 0.0), To_Unit_Quaternion (-Pi/2.0, X))),
         3 => Minus_Half_Pi_X * J4_PE_Clevis,
         4 => Add (J4_PE_Case, J5_RC, ((A4, 0.0, 0.0), To_Unit_Quaternion (-Pi/2.0, X))),
         5 => Minus_Half_Pi_X * J6_PC_Clevis,
         6 => J6_PC_Case,
         7 => J7_TB);
   end Mass;

   function Is_Singular (Position : in SAL.Math_Float_Manipulator_7.Joint_Array_Real_Type) return Boolean
   is
      use SAL.Math_Float.Scalar;
      Half_PI : constant Real_Type := Pi / 2.0;
   begin
      return
        abs (abs Position (3) - Half_PI) < 0.12 or
        abs Position (4) < 0.6 or
        abs Position (6) < 0.02;
   end Is_Singular;

end SAL.Math_Float_RRC_K1607_Nominal;
