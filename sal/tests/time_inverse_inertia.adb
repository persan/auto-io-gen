--  Abstract :
--
--  Time various forms of inverse inertia computation.
--
--  Copyright (C) 2002, 2003, 2006 Stephen Leake.  All Rights Reserved.
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

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Math_Float.DOF_3; use SAL.Math_Float.DOF_3;
with SAL.Time_Me;
procedure Time_Inverse_Inertia
is
   package Duration_Text_IO is new Fixed_IO (Duration);

   Call_Count : Integer := 10_000;

   Result : Cart_Array_Cart_Vector_Type;
   pragma Unreferenced (Result);

   Inertia : constant Inertia_Type := (1.0, 2.0, 3.0, 0.4, 0.5, 0.6);

   procedure Call_Det_Inverse
   is begin
      Result := Inverse (To_CACV (Inertia));
   end Call_Det_Inverse;

   function Time_Det_Inverse is new SAL.Time_Me.Gen_Time_Me (Call_Det_Inverse);

   function Fast_Inverse_1 (Inertia : in Inertia_Type) return Cart_Array_Cart_Vector_Type
   is
      use SAL.Math_Float;

      Ixx : constant Real_Type := Inertia (DOF_3.Ixx);
      Iyy : constant Real_Type := Inertia (DOF_3.Iyy);
      Izz : constant Real_Type := Inertia (DOF_3.Izz);
      Ixy : constant Real_Type := Inertia (DOF_3.Ixy);
      Ixz : constant Real_Type := Inertia (DOF_3.Ixz);
      Iyz : constant Real_Type := Inertia (DOF_3.Iyz);

      T_1 : constant Real_Type := Iyy * Izz - Iyz * Iyz;
      T_2 : constant Real_Type := Ixy * Iyz - Ixz * Iyy;
      T_3 : constant Real_Type := Ixz * Iyz - Ixy * Izz;
      T_4 : constant Real_Type := 1.0 / (Ixx * T_1 + Ixy * T_3 + Ixz * T_2);
      T_5 : constant Real_Type := T_3 * T_4;
      T_6 : constant Real_Type := T_2 * T_4;
      T_7 : constant Real_Type := (Ixy * Ixz - Ixx * Iyz) * T_4;

   begin
      return
        (X =>
           (X => T_1 * T_4,
            Y => T_5,
            Z => T_6),
         Y =>
           (X => T_5,
            Y => (Ixx * Izz - Ixz * Ixz) * T_4,
            Z => T_7),
         Z =>
           (X => T_6,
            Y => T_7,
            Z => (Ixx * Iyy - Ixy * Ixy) * T_4));
   end Fast_Inverse_1;

   procedure Call_Fast_Inverse_1
   is begin
      Result := Fast_Inverse_1 (Inertia);
   end Call_Fast_Inverse_1;

   function Time_Fast_Inverse_1 is new SAL.Time_Me.Gen_Time_Me (Call_Fast_Inverse_1);

   function Fast_Inverse_2 (Inertia : in Inertia_Type) return Cart_Array_Cart_Vector_Type
   is
      use SAL.Math_Float;

      Ixx : constant Real_Type := Inertia (DOF_3.Ixx);
      Iyy : constant Real_Type := Inertia (DOF_3.Iyy);
      Izz : constant Real_Type := Inertia (DOF_3.Izz);
      Ixy : constant Real_Type := Inertia (DOF_3.Ixy);
      Ixz : constant Real_Type := Inertia (DOF_3.Ixz);
      Iyz : constant Real_Type := Inertia (DOF_3.Iyz);

      T_1 : constant Real_Type := Iyz * Iyz;
      T_2 : constant Real_Type := Ixz * Ixz;
      T_3 : constant Real_Type := Ixy * Ixy;
      T_4 : constant Real_Type := 1.0 / (Ixx * Iyy * Izz - T_3 * Izz - Ixx * T_1 + 2.0 * Ixy * Ixz * Iyz - T_2 * Iyy);
      T_5 : constant Real_Type := (Ixz * Iyz - Ixy * Izz) * T_4;
      T_6 : constant Real_Type := (Ixy * Iyz - Ixz * Iyy) * T_4;
      T_7 : constant Real_Type := (Ixy * Ixz - Ixx * Iyz) * T_4;

   begin
      return
        (X =>
           (X => (Iyy * Izz - T_1) * T_4,
            Y => T_5,
            Z => T_6),
         Y =>
           (X => T_5,
            Y => (Ixx * Izz - T_2) * T_4,
            Z => T_7),
         Z =>
           (X => T_6,
            Y => T_7,
            Z => (Ixx * Iyy - T_3) * T_4));
   end Fast_Inverse_2;

   procedure Call_Fast_Inverse_2
   is begin
      Result := Fast_Inverse_2 (Inertia);
   end Call_Fast_Inverse_2;

   function Time_Fast_Inverse_2 is new SAL.Time_Me.Gen_Time_Me (Call_Fast_Inverse_2);

   procedure Display (Comment : in String; Time : in Duration)
   is
   begin
      Put_Line (Comment);
      Put ("Total time (seconds) :");
      Duration_Text_IO.Put (Time);
      New_Line;
      Put ("Microseconds per call :");
      Duration_Text_IO.Put ((1_000_000 * Time) / Call_Count);
      New_Line;
      New_Line;
   end Display;

begin
   case Ada.Command_Line.Argument_Count is
   when 0 =>
      --  use defaults
      null;
   when 2 =>
      Call_Count := Integer'Value (Ada.Command_Line.Argument (1));

   when others =>
      Put_Line ("arguments : <call_count>");
      Put_Line ("defaults  : " & Integer'Image (Call_Count));
      return;
   end case;

   Display ("determinant", Time_Det_Inverse (Call_Count));
   Display ("Fast_Inverse_1", Time_Fast_Inverse_1 (Call_Count));
   Display ("Fast_Inverse_2", Time_Fast_Inverse_2 (Call_Count));

end Time_Inverse_Inertia;
