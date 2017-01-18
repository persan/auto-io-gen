--  Abstract:
--
--  Test SAL.Gen_Math.Gen_Manipulator.Null_Space_Projector,
--  .Slow_Inertia, and .Slow_Gravity_Torque, using the Manipulator_7
--  instantiation and the RRC K1607 as an example. All other
--  subprograms are tested in Test_Math_Float_Manipulator_6.
--
--  Design Decisions:
--
--  Null_Space_Projector is tested here, because the null space of a 6
--  DOF arm is the empty set. Slow_Inertia and Slow_Gravity_Torque are
--  tested here because we know the mass of the RRC independently.
--

with Ada.Text_IO;                  use Ada.Text_IO;
with SAL.Gen_Array_Text_IO;
with SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_6.Left;
with SAL.Math_Float.DOF_6.Text_IO; use SAL.Math_Float.DOF_6.Text_IO;
with SAL.Math_Float.Den_Hart.Text_IO;
with SAL.Math_Float.Scalar;        use SAL.Math_Float.Scalar;
with SAL.Math_Float.Text_IO;
with SAL.Math_Float_Manipulator_7.Left;
with SAL.Math_Float_RRC_K1607_Nominal;
procedure Debug_Math_Float_Manipulator_7
is
   use SAL.Math_Float_Manipulator_7;
   use SAL.Math_Float_Manipulator_7.Math;
   use SAL.Math_Float_Manipulator_7.Left;

   Geometry : constant Joint_Array_Den_Hart_Type := SAL.Math_Float_RRC_K1607_Nominal.Geometry;
   Mass     : constant Joint_Array_Mass_Type     := SAL.Math_Float_RRC_K1607_Nominal.Mass;

   T0_A_Grav  : constant SAL.Math_Float.DOF_3.Cart_Vector_Type := (0.0, 0.0, -9.80665);
   Tlast_T_Tp : constant SAL.Math_Float.DOF_6.Pose_Type        := ((0.0, 0.0, 0.16764),
                                                                   SAL.Math_Float.DOF_3.Zero_Unit_Quaternion);
   --   0.16764 meters
   Tp_T_Obj   : constant SAL.Math_Float.DOF_6.Pose_Type        := ((0.0, 0.0, 0.1),
                                                                   SAL.Math_Float.DOF_3.Zero_Unit_Quaternion);

   Nominal_Position : constant Joint_Array_Real_Type :=
     (0.0, -Pi / 2.0, 0.0, Pi / 2.0, 0.0, Pi / 2.0, 0.0);
   --  nice non-singular position.

   package Joint_Array_Real_Text_IO is new SAL.Gen_Array_Text_IO.Float_1D
      (Element_Type             => SAL.Math_Float.Real_Type,
       Index_Type               => Joint_Index_Type,
       Index_Array_Element_Type => Joint_Array_Real_Type,
       Element_Put              => SAL.Math_Float.Text_IO.Put,
       Element_Get              => SAL.Math_Float.Text_IO.Get);

   use Joint_Array_Real_Text_IO;

   package Joint_Array_Pose_Text_IO is new SAL.Gen_Array_Text_IO.Private_1D
      (Element_Type             => SAL.Math_Float.DOF_6.Pose_Type,
       Index_Type               => Joint_Index_Type,
       Index_Array_Element_Type => Joint_Array_Pose_Type,
       Element_Put              => SAL.Math_Float.DOF_6.Text_IO.Put_Item,
       Element_Get              => SAL.Math_Float.DOF_6.Text_IO.Get_Item);

   use Joint_Array_Pose_Text_IO;

   package Joint_Array_Den_Hart_Text_IO is new SAL.Gen_Array_Text_IO.Private_1D
      (Element_Type             => SAL.Math_Float.Den_Hart.Den_Hart_Type,
       Index_Type               => Joint_Index_Type,
       Index_Array_Element_Type => Joint_Array_Den_Hart_Type,
       Element_Put              => SAL.Math_Float.Den_Hart.Text_IO.Put_Item,
       Element_Get              => SAL.Math_Float.Den_Hart.Text_IO.Get_Item);

   use Joint_Array_Den_Hart_Text_IO;
begin
   Test_Forward_Kinematics :
   declare
      use Joint_Array_Real_Ops;

      procedure Test_T0_T_Ti (Joints : Joint_Array_Real_Type)
      is
         T0_T_Ti : constant Joint_Array_Pose_Type := Slow_T0_T_Ti (Joints, Geometry);
      begin
         Put ("Joints => "); Put (Joints); New_Line;
         Put_Line ("T0_T_Ti => ");
         Put (T0_T_Ti, Single_Line_Array => False, Single_Line_Element => False, Named_Association_Array => True);
         New_Line (2);
      end Test_T0_T_Ti;
   begin
      Put_Line ("Test Slow_T0_T_Ti");
      Test_T0_T_Ti ((0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0));
      Test_T0_T_Ti ((2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0));
   end Test_Forward_Kinematics;

   Test_Projector_Type :
   declare
      Ti_T_Obj         : Joint_Array_Pose_Type;
      T0_T_Obj         : SAL.Math_Float.DOF_6.Pose_Type;
      Jacobian         : Jacobian_Type;
      Inverse_Jacobian : Inverse_Jacobian_Type;

      procedure Test_Null_Space_Projector (Joint_Pos, Joint_Delta : in Joint_Array_Real_Type)
      is
         use SAL.Math_Float.DOF_6;
         use SAL.Math_Float.DOF_6.Left;
         use Joint_Array_Real_Ops;
         use Joint_Array_DCV_Ops;

         Projector     : Projector_Type;
         P_Joint_Delta : Joint_Array_Real_Type;
      begin
         Slow_Ti_T_Obj (Joint_Pos, Geometry, Tlast_T_Tp, Tp_T_Obj, Ti_T_Obj, T0_T_Obj);
         Jacobian         := Slow_Jacobian (Ti_T_Obj);
         Inverse_Jacobian := Inverse (Jacobian);
         Put ("Joint_Pos => "); Put (Joint_Pos); New_Line;
         Projector        := Null_Space_Projector (Jacobian, Inverse_Jacobian);
         --  IMPROVEME: auto_text_io doesn't support this type (array of generic formal)
         --  Put_Line ("Projector => "); Put (Projector); New_Line;
         P_Joint_Delta    := Projector * Joint_Delta;
         Put ("Joint_Delta     => "); Put (Joint_Delta); New_Line;
         Put ("P * Joint_Delta => "); Put (P_Joint_Delta); New_Line;
         Put_Line ("Pose (Joint_Pos) - Pose (Joint_Pos + P * Joint_Delta) => ");
         Put (T0_T_Obj - Slow_T0_T_Obj (Joint_Pos + P_Joint_Delta, Geometry, Tlast_T_Tp, Tp_T_Obj));
         New_Line (2);
      end Test_Null_Space_Projector;
   begin
      Put_Line ("Testing SAL.Math_Float.Manipulator_7.Projectors");
      Put_Line ("Very nice");
      Test_Null_Space_Projector (Nominal_Position, (others => 0.1));
      Put_Line ("Wrist Singular");
      Test_Null_Space_Projector ((0.0, -Pi / 2.0, 0.0, Pi / 2.0, 0.0, 0.0, 0.0), (5 => 0.1, others => 0.0));
      Put_Line ("Elbow Singular");
      Test_Null_Space_Projector ((0.0, -Pi / 2.0, 0.0, 0.0, 0.0, Pi / 2.0, 0.0), (5 => 0.1, others => 0.0));
      Put_Line ("Shoulder +Singular");
      Test_Null_Space_Projector ((0.0, -Pi / 2.0, Pi / 2.0, Pi / 2.0, 0.0, Pi / 2.0, 0.0), (2 => 0.1, others => 0.0));
      Put_Line ("Shoulder -Singular");
      Test_Null_Space_Projector ((0.0, -Pi / 2.0, -Pi / 2.0, Pi / 2.0, 0.0, Pi / 2.0, 0.0), (2 => 0.1, others => 0.0));
   end Test_Projector_Type;

   Test_Inertia_Type :
   declare
      procedure Test_Slow_Inertia (Joint_Pos : Joint_Array_Real_Type)
      is
         Inertia : constant Inertia_Type := Slow_Inertia (Joint_Pos, Geometry, Mass);
         pragma Unreferenced (Inertia);
      begin
         Put ("Joint_Pos => "); Put (Joint_Pos); New_Line;
         Put_Line ("Inertia => ");
         --  IMPROVEME: auto_text_io doesn't support this type (array of generic formal)
         --  Put (Inertia);
         New_Line (2);
      end Test_Slow_Inertia;

      procedure Test_Slow_Gravity_Torque (Joint_Pos : Joint_Array_Real_Type)
      is
         T0_T_Ti : constant Joint_Array_Pose_Type := Slow_T0_T_Ti (Joint_Pos, Geometry);

         Gravity_Torque : constant Joint_Array_Real_Type := Slow_Gravity_Torque (T0_T_Ti, T0_A_Grav, Mass);
      begin
         Put ("Joint_Pos      => "); Put (Joint_Pos); New_Line;
         Put ("Gravity_Torque => "); Put (Gravity_Torque); New_Line;
      end Test_Slow_Gravity_Torque;

   begin
      Put_Line ("Testing mass and inertia operations");
      Put_Line ("Geometry => "); Put (Geometry); New_Line;
      --  IMPROVEME: auto_text_io doesn't support this type (array of generic formal)
         --  Put_Line ("Mass => "); Put (Mass); New_Line;

      Put_Line (" Testing Slow_Inertia");
      Test_Slow_Inertia (Nominal_Position);
      Test_Slow_Inertia ((0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0));

      Put_Line (" Testing Slow_Gravity_Torque");
      Test_Slow_Gravity_Torque ((0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0));
      Put_Line ("should be independent of joint 1 (gravity is along joint 1 axis)");
      Test_Slow_Gravity_Torque ((2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0));
      Test_Slow_Gravity_Torque ((0.0, Pi / 2.0, 0.0, 0.0, 0.0, 0.0, 0.0));
      Test_Slow_Gravity_Torque ((0.0, Pi / 2.0, 0.0, -Pi / 2.0, 0.0, 0.0, 0.0));
      Test_Slow_Gravity_Torque ((0.0, Pi / 2.0, 0.0, -Pi / 2.0, 0.0, Pi / 2.0, 0.0));
   end Test_Inertia_Type;

end Debug_Math_Float_Manipulator_7;
