--  Abstract :
--
--  Perform timing comparison between
--       Manipulator_Math.Mult (Pose, Den_Hart, Position)
--  and  Pose * Manipulator_Math.To_Pose (Den_Hart, Position)
--  and between
--       Manipulator_Math.Mult (Den_Hart, Position, Pose)
--  and  Manipulator_Math.To_Pose (Den_Hart, Position) * Pose.
--
--  Copyright (C) 2002, 2003, 2005, 2006 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;               use Ada.Text_IO;
with SAL.Time_Me;
with SAL.Math_Float.DOF_3.Left; use SAL.Math_Float.DOF_3.Left;
with SAL.Math_Float.DOF_3;      use SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_6.Left;
with SAL.Math_Float.DOF_6.Text_IO;
with SAL.Math_Float.Den_Hart.Left;
with SAL.Math_Float.Den_Hart.Text_IO;
with SAL.Math_Float.Scalar;
with SAL.Math_Float.Text_IO;    use SAL.Math_Float.Text_IO;
with SAL.Math_Float;            use SAL.Math_Float;
with Time_Aux;
procedure Time_Math_Float_Den_Hart
is
   Calls         : constant Natural := 10000;
   Time_Used     : Real_Type;
   Pose          : SAL.Math_Float.DOF_6.Pose_Type;
   Den_Hart      : SAL.Math_Float.Den_Hart.Den_Hart_Type;
   Position      : Real_Type;

   Result        : SAL.Math_Float.DOF_6.Pose_Type;
   pragma Warnings (Off, Result); --  assigned but never read

   procedure Call_Mult1
   is
   begin
      Result := SAL.Math_Float.Den_Hart.Left.Mult (Pose, Den_Hart, Position);
   end Call_Mult1;

   procedure Call_Long_Calculation1
   is
      use DOF_6.Left;
   begin
      Result := Pose * SAL.Math_Float.Den_Hart.Left.To_Pose (Den_Hart, Position);
   end Call_Long_Calculation1;

   procedure Call_Mult2
   is begin
      Result := SAL.Math_Float.Den_Hart.Left.Mult (Den_Hart, Position, Pose);
   end Call_Mult2;

   procedure Call_Long_Calculation2
   is
      use DOF_6.Left;
   begin
      Result := SAL.Math_Float.Den_Hart.Left.To_Pose (Den_Hart, Position) * Pose;
   end Call_Long_Calculation2;

   function Time_Null is new SAL.Time_Me.Gen_Time_Me (Time_Me => Time_Aux.Call_Null);
   function Time_Mult1 is new SAL.Time_Me.Gen_Time_Me (Time_Me => Call_Mult1);
   function Time_Mult2 is new SAL.Time_Me.Gen_Time_Me (Time_Me => Call_Mult2);
   function Time_Long_Calculation1 is new SAL.Time_Me.Gen_Time_Me (Time_Me => Call_Long_Calculation1);
   function Time_Long_Calculation2 is new SAL.Time_Me.Gen_Time_Me (Time_Me => Call_Long_Calculation2);

   procedure Perform_Timing
     (Pose     : in DOF_6.Pose_Type;
      Den_Hart : in SAL.Math_Float.Den_Hart.Den_Hart_Type;
      Position : in Real_Type)
   is
   begin
      Put ("Pose     => "); DOF_6.Text_IO.Put (Pose); New_Line;
      Put ("Den_Hart => "); SAL.Math_Float.Den_Hart.Text_IO.Put (Den_Hart); New_Line;
      Put ("Position => "); Put (Position); New_Line;

      Time_Math_Float_Den_Hart.Pose     := Pose;
      Time_Math_Float_Den_Hart.Den_Hart := Den_Hart;
      Time_Math_Float_Den_Hart.Position := Position;

      Put ("Seconds per call for Mult (Pose, Den_Hart, Position) =>    ");
      Put (Item => Real_Type (Time_Mult1 (Calls)) / Real_Type (Calls), Exp => 3); New_Line;

      Put ("Seconds per call for Pose * To_Pose(Den_Hart, Position) => ");
      Put (Item => Real_Type (Time_Long_Calculation1 (Calls)) / Real_Type (Calls), Exp => 3); New_Line;

      Put ("Seconds per call for Mult (Den_Hart, Position, Pose) =>    ");
      Put (Item => Real_Type (Time_Mult2 (Calls)) / Real_Type (Calls), Exp => 3); New_Line;

      Put ("Seconds per call for To_Pose(Den_Hart, Position) * Pose => ");
      Put (Item => Real_Type (Time_Long_Calculation2 (Calls)) / Real_Type (Calls), Exp => 3); New_Line;
      New_Line;
   end Perform_Timing;
begin
   Put_Line ("Compare time per call for Pose * Den_Hart and Den_Hart * Pose =>");
   New_Line; New_Line;

   Put_Line ("Calling Null procedure " & Natural'Image (Calls) & " times.");
   Time_Used := Real_Type (Time_Null (Calls));
   Put (Time_Used); Put_Line (" seconds were used.");
   Put (Time_Used * 1_000_000.0 / Real_Type (Calls));
   Put_Line (" microseconds per call of overhead.");

   Perform_Timing
     (Pose     => DOF_6.Zero_Pose,
      Den_Hart => (SAL.Math_Float.Den_Hart.Revolute, 0.0, Scalar.Sin_Cos (0.0), 0.0),
      Position =>  0.0);

   Perform_Timing
     (Pose     => ((1.0, 2.0, 3.0),
                   (To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit)))),
      Den_Hart => (SAL.Math_Float.Den_Hart.Revolute, 0.1, Scalar.Sin_Cos (0.0), 0.1),
      Position =>  0.1);

   Perform_Timing
     (Pose     => ((1.0, 2.0, 3.0), (To_Unit_Quaternion (0.5, 0.5, 0.5, 0.5))),
      Den_Hart => (SAL.Math_Float.Den_Hart.Revolute, 0.5, Scalar.Sin_Cos (0.5), 0.5),
      Position =>  0.5);
end Time_Math_Float_Den_Hart;
