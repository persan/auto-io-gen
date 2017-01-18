--  Abstract:
--
--  Test SAL.Gen_Math.Gen_Manipulator, using the
--  SAL.Math_Float.Manipulator_6 instantiation and the Kraft Hand
--  Controller as an example. Null_Space_Projector is tested in
--  Test_Math_Float_Manipulator_7, because the null space of a 6 DOF
--  arm is the empty set. Slow_Inertia and Slow_Gravity_Torque are
--  also tested in Test_Math_Float_Manipulator_7, since we know the
--  mass of the RRC independently.
--

with Ada.Text_IO;                          use Ada.Text_IO;
with SAL.Gen_Array_Text_IO;
with SAL.Math_Float.DOF_3.Left;
with SAL.Math_Float.DOF_6.Left;
with SAL.Math_Float.DOF_6.Text_IO;         use SAL.Math_Float.DOF_6.Text_IO;
with SAL.Math_Float.Den_Hart.Text_IO;
with SAL.Math_Float.Scalar;                use SAL.Math_Float.Scalar;
with SAL.Math_Float.Text_IO;
with SAL.Math_Float_Kraft_HC_Nominal;
with SAL.Math_Float_Manipulator_6.Left;
procedure Debug_Math_Float_Manipulator_6
is
   use SAL.Math_Float;
   use SAL.Math_Float_Manipulator_6;
   use SAL.Math_Float_Manipulator_6.Math;
   use SAL.Math_Float_Manipulator_6.Left;

   Geometry         : constant Joint_Array_Den_Hart_Type      := SAL.Math_Float_Kraft_HC_Nominal.Geometry;
   Nominal_Position : constant Joint_Array_Real_Type          := (0.0, -Pi/2.0, Pi / 2.0, 0.0, -Pi/2.0, 0.0);
   Tlast_T_Tp       : constant SAL.Math_Float.DOF_6.Pose_Type := ((0.0, 0.0, 0.1),
                                                                  (SAL.Math_Float.DOF_3.Zero_Unit_Quaternion));
   Tp_T_Obj         : constant SAL.Math_Float.DOF_6.Pose_Type := ((0.0, 0.0, -0.1),
                                                                  (SAL.Math_Float.DOF_3.Zero_Unit_Quaternion));

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
   Put_Line ("Testing Generic_Manipulator with right handed Kraft Hand Controller");
   Put_Line ("Geometry => "); Put (Geometry); New_Line;

   Test_Jacobian_Type :
   declare
      use SAL.Math_Float.DOF_3;
      use SAL.Math_Float.DOF_3.Left;
      use Joint_Array_Real_Ops;

      Ti_T_Obj : Joint_Array_Pose_Type;
      T0_T_Obj : SAL.Math_Float.DOF_6.Pose_Type;
      Jacobian : Jacobian_Type;

      function Close_Enough (Left, Right : in Jacobian_Type) return Boolean
      is
      begin
         for I in Left'Range
         loop
            for J in Left (Left'First)'Range
            loop
               if abs (Left (I)(J) - Right (I)(J)) > Real_Type'Epsilon then
                  return False;
               end if;
            end loop;
         end loop;
         return True;
      end Close_Enough;

      procedure Test_Jacobian
        (Joints : Joint_Array_Real_Type;
         Tlast_T_TP,
         TP_T_Obj : SAL.Math_Float.DOF_6.Pose_Type := SAL.Math_Float.DOF_6.Zero_Pose)
      is
         --  Inverse is just a call to an instantiation of
         --  Generic_Inverse_Array_Math, so we don't need to test it
         --  here.
      begin
         Slow_Ti_T_Obj (Joints, Geometry, Tlast_T_TP, TP_T_Obj, Ti_T_Obj, T0_T_Obj);
         Jacobian := Slow_Jacobian (Ti_T_Obj);
         Put ("Joints     => "); Put (Joints); New_Line;
         Put ("Tlast_T_TP => "); Put (Tlast_T_TP); New_Line;
         Put ("TP_T_Obj   => "); Put (TP_T_Obj); New_Line;
         --  IMPROVEME: auto_text_io doesn't support this type (array of generic formal)
         --  Put_Line ("Jacobian => "); Put (Jacobian); New_Line;
         New_Line;
      end Test_Jacobian;

      procedure Test_Change_Frame
        (Jacobian      : Jacobian_Type;
         Current_T_New : SAL.Math_Float.DOF_6.Pose_Type)
      is
         New_Jacob_1 : constant Jacobian_Type := Transform_Jacobian (Current_T_New, Jacobian);
         New_Jacob_2 : constant Jacobian_Type := SAL.Math_Float.DOF_6.Left.To_Rate_Transform (Current_T_New) * Jacobian;
      begin
         --  IMPROVEME: auto_text_io doesn't support this type (array of generic formal)
         --  Put_Line ("Jacobian => "); Put (Jacobian); New_Line;
         Put_Line ("Current_T_New => "); Put (Current_T_New); New_Line;
         Put_Line ("Transform_Jacobian (Current_T_New, Jacobian) => ");
         --  IMPROVEME: auto_text_io doesn't support this type (array of generic formal)
         --  Put (New_Jacob_1); New_Line;
         Put ("To_Rate_Transform (Current_T_New) * Jacobian => ");
         if Close_Enough (New_Jacob_1, New_Jacob_2) then
            Put_Line ("same");
         else
            --  IMPROVEME: auto_text_io doesn't support this type (array of generic formal)
            --  Put (New_Jacob_2); New_Line;
            null;
         end if;
         New_Line;
      end Test_Change_Frame;

   begin
      Put_Line ("Testing Slow_Jacobian");
      Put_Line ("Test effect of Tlast_T_TP, TP_T_Obj");
      Test_Jacobian (Nominal_Position);
      Test_Jacobian (Nominal_Position, Tlast_T_Tp);
      Test_Jacobian (Nominal_Position, Tlast_T_Tp, Tp_T_Obj);

      Put_Line ("move each joint by +0.1 radians");
      Test_Jacobian (Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.0, 0.1));
      Test_Jacobian (Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.1, 0.0));
      Test_Jacobian (Nominal_Position + (0.0, 0.0, 0.0, 0.1, 0.0, 0.0));
      Test_Jacobian (Nominal_Position + (0.0, 0.0, 0.1, 0.0, 0.0, 0.0));
      Test_Jacobian (Nominal_Position + (0.0, 0.1, 0.0, 0.0, 0.0, 0.0));
      Test_Jacobian (Nominal_Position + (0.1, 0.0, 0.0, 0.0, 0.0, 0.0));

      Put_Line ("Test Change_Frame");
      Put_Line ("move 0.1 in each Cartesian axis");
      Slow_Ti_T_Obj (Nominal_Position, Geometry, Tlast_T_Tp, Tp_T_Obj, Ti_T_Obj, T0_T_Obj);
      Jacobian := Slow_Jacobian (Ti_T_Obj);
      Test_Change_Frame (Jacobian, (Zero_Cart_Vector, Zero_Unit_Quaternion));
      Test_Change_Frame (Jacobian, ((0.1, 0.0, 0.0), Zero_Unit_Quaternion));
      Test_Change_Frame (Jacobian, ((0.0, 0.1, 0.0), Zero_Unit_Quaternion));
      Test_Change_Frame (Jacobian, ((0.0, 0.0, 0.1), Zero_Unit_Quaternion));
      Test_Change_Frame (Jacobian, (Zero_Cart_Vector, To_Unit_Quaternion (0.1, X)));
      Test_Change_Frame (Jacobian, (Zero_Cart_Vector, To_Unit_Quaternion (0.1, Y)));
      Test_Change_Frame (Jacobian, (Zero_Cart_Vector, To_Unit_Quaternion (0.1, Z)));
   end Test_Jacobian_Type;

   Test_Forward_Kinematics :
   declare
      use SAL.Math_Float.DOF_6;
      use SAL.Math_Float.DOF_6.Left;
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

      function Close_Enough (Left, Right : in Joint_Array_Pose_Type) return Boolean
      is
      begin
         for I in Left'Range
         loop
            if not (Mag (Left (I) - Right (I)) <= (0.001, 0.001)) then
               return False;
            end if;
         end loop;
         return True;
      end Close_Enough;

      procedure Test_Ti_T_Obj
        (Joints : Joint_Array_Real_Type;
         Tlast_T_TP,
         TP_T_Obj : SAL.Math_Float.DOF_6.Pose_Type := SAL.Math_Float.DOF_6.Zero_Pose)
      is
         Ti_T_Obj_Indirect : Joint_Array_Pose_Type;
         Ti_T_Obj          : Joint_Array_Pose_Type;
         T0_T_Obj          : Pose_Type;
         T0_T_Obj_Direct   : constant Pose_Type := Slow_T0_T_Obj (Joints, Geometry, Tlast_T_TP, TP_T_Obj);
      begin
         Slow_Ti_T_Obj (Joints, Geometry, Tlast_T_TP, TP_T_Obj, Ti_T_Obj, T0_T_Obj);
         Put ("Joints     => "); Put (Joints); New_Line;
         Put ("Compare to Inverse (Slow_T0_T_Ti) => ");
         Ti_T_Obj_Indirect := Inverse (Slow_T0_T_Ti (Joints, Geometry), Tlast_T_TP, TP_T_Obj);
         if Close_Enough (Ti_T_Obj_Indirect, Ti_T_Obj) then
            Put_Line ("same");
         else
            Put (Ti_T_Obj_Indirect); New_Line;
         end if;
         Put ("Compare T0_T_Obj to Slow_T0_T_Obj => ");
         if Mag (T0_T_Obj_Direct - T0_T_Obj) <= (0.001, 0.001) then
            Put_Line ("same");
         else
            Put (T0_T_Obj_Direct); New_Line;
         end if;
         New_Line;
      end Test_Ti_T_Obj;

   begin
      Put_Line ("Test Slow_T0_T_Ti");
      Put_Line ("nominal position");
      Test_T0_T_Ti (Nominal_Position);
      Put_Line ("move each joint by 0.1 radians");
      Test_T0_T_Ti (Nominal_Position + (0.1, 0.0, 0.0, 0.0, 0.0, 0.0));
      Test_T0_T_Ti (Nominal_Position + (0.0, 0.1, 0.0, 0.0, 0.0, 0.0));
      Test_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.1, 0.0, 0.0, 0.0));
      Test_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.0, 0.1, 0.0, 0.0));
      Test_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.1, 0.0));
      Test_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.0, 0.1));

      Put_Line ("Test Slow_T0_T_Obj, Slow_Ti_T_Obj");
      Put_Line ("Test effect of Tlast_T_TP, TP_T_Obj");
      Test_Ti_T_Obj (Nominal_Position, SAL.Math_Float.DOF_6.Zero_Pose, SAL.Math_Float.DOF_6.Zero_Pose);
      Test_Ti_T_Obj (Nominal_Position, Tlast_T_Tp, SAL.Math_Float.DOF_6.Zero_Pose);
      Test_Ti_T_Obj (Nominal_Position, Tlast_T_Tp, Tp_T_Obj);

      Put_Line ("move each joint by 0.1 radians");
      Test_Ti_T_Obj (Nominal_Position + (-0.1, 0.0, 0.0, 0.0, 0.0, 0.0));
      Test_Ti_T_Obj (Nominal_Position + (0.0, 0.1, 0.0, 0.0, 0.0, 0.0));
      Test_Ti_T_Obj (Nominal_Position + (0.0, 0.0, -0.1, 0.0, 0.0, 0.0));
      Test_Ti_T_Obj (Nominal_Position + (0.0, 0.0, 0.0, 0.1, 0.0, 0.0));
      Test_Ti_T_Obj (Nominal_Position + (0.0, 0.0, 0.0, 0.0, -0.1, 0.0));
      Test_Ti_T_Obj (Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.0, 0.1));
   end Test_Forward_Kinematics;

   Test_Inverse_Kinematics :
   declare
      Goal_Joint       : constant Joint_Array_Real_Type := Nominal_Position;
      Ti_T_Obj         : Joint_Array_Pose_Type;
      Nominal_Pose     : SAL.Math_Float.DOF_6.Pose_Type;
      Inverse_Jacobian : Inverse_Jacobian_Type;

      use type SAL.Math_Float.DOF_6.Pose_Type;

      procedure Test_Incremental_To_Joint
        (Nominal_T_Delta : in SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
         Accuracy        : in SAL.Math_Float.DOF_6.Dual_Real_Type        := (0.001, 0.01);
         Iteration_Limit : in Integer                                    := 10)
      is
         use SAL.Math_Float.DOF_6;
         use SAL.Math_Float.DOF_6.Left;
         use Joint_Array_Real_Ops;

         Guess      : constant Joint_Array_Real_Type := Goal_Joint;
         Iterations : Integer;
         Result     : Joint_Array_Real_Type;
         Goal_Pose  : constant Pose_Type             := Nominal_Pose + Nominal_T_Delta;
      begin
         Put ("Guess           => "); Put (Guess); New_Line;
         Put ("Nominal_T_Delta => "); Put (Nominal_T_Delta); New_Line;
         Put ("Iteration_Limit => " & Integer'Image (Iteration_Limit)); New_Line;
         SAL.Math_Float.Text_IO.Real_Text_IO.Default_Exp := 3;
         Put ("Accuracy        => "); Put (Accuracy); New_Line;
         SAL.Math_Float.Text_IO.Real_Text_IO.Default_Exp := 0;
         Slow_Incremental_To_Joint (Goal_Pose, Guess, Geometry,
                                    Tlast_T_Tp, Tp_T_Obj, Inverse_Jacobian, Accuracy, Iteration_Limit,
                                    Iterations, Result);
         Put ("Iterations      => " & Integer'Image (Iterations)); New_Line;
         Put ("Guess - Result      => "); Put (Goal_Joint - Result); New_Line;
         Put ("Goal_Pose - Result  => ");
         Put (Slow_T0_T_Obj (Result, Geometry, Tlast_T_Tp, Tp_T_Obj) - Goal_Pose); New_Line;
         New_Line;
      exception
      when SAL.Singular =>
         Put_Line ("SINGULAR raised");
         New_Line;
      end Test_Incremental_To_Joint;

      procedure Test_To_Joint
        (Goal_Joint      : in Joint_Array_Real_Type;
         Guess           : in Joint_Array_Real_Type;
         Accuracy        : in SAL.Math_Float.DOF_6.Dual_Real_Type := (0.001, 0.01);
         Partition_Limit : in Integer                             := 10;
         Iteration_Limit : in Integer                             := 5)
      is
         use SAL.Math_Float.DOF_6;
         use SAL.Math_Float.DOF_6.Left;
         use Joint_Array_Real_Ops;

         Tp_T_Obj : constant Pose_Type := Zero_Pose;
         --  override global; make Jacobian sufficiently complex to
         --  see wrist singularity.

         Goal_Pose  : constant Pose_Type := Slow_T0_T_Obj (Goal_Joint, Geometry, Tlast_T_Tp, Tp_T_Obj);
         Result     : Joint_Array_Real_Type;
         Partitions : Integer;
         Iterations : Integer;
      begin
         Put ("Guess               => "); Put (Guess); New_Line;
         Put ("Goal_Joint          => "); Put (Goal_Joint); New_Line;
         SAL.Math_Float.Text_IO.Real_Text_IO.Default_Exp := 3;
         Put ("Accuracy            => "); Put (Accuracy); New_Line;
         SAL.Math_Float.Text_IO.Real_Text_IO.Default_Exp := 0;
         Put ("Partition_Limit     => " & Integer'Image (Partition_Limit)); New_Line;
         Put ("Iteration_Limit     => " & Integer'Image (Iteration_Limit)); New_Line;
         Slow_To_Joint (Goal_Pose, Guess, Geometry, Tlast_T_Tp, Tp_T_Obj, Accuracy, Partition_Limit,
                                     Iteration_Limit, Partitions, Iterations, Result);
         Put ("Partitions          => " & Integer'Image (Partitions)); New_Line;
         Put ("Iterations          => " & Integer'Image (Iterations)); New_Line;
         Put ("Result              => "); Put (Result); New_Line;
         Put ("Goal_Joint - Result => "); Put (Goal_Joint - Result); New_Line;
         Put ("Goal_Pose - Result  => ");
         Put (Goal_Pose - Slow_T0_T_Obj (Result, Geometry, Tlast_T_Tp, Tp_T_Obj)); New_Line;
         New_Line;
      exception
      when SAL.Singular =>
         Put_Line ("SINGULAR raised");
         New_Line;
      end Test_To_Joint;

   begin
      Put_Line ("Testing Slow_Incremental_To_Joint");
      Slow_Ti_T_Obj (Goal_Joint, Geometry, Tlast_T_Tp, Tp_T_Obj, Ti_T_Obj, Nominal_Pose);
      Inverse_Jacobian := Inverse (Slow_Jacobian (Ti_T_Obj));

      Put_Line ("Test near Nominal_Position");
      Test_Incremental_To_Joint (SAL.Math_Float.DOF_6.Zero_Dual_Cart_Vector);
      Test_Incremental_To_Joint ((0.02, 0.0, 0.0, 0.0, 0.0, 0.0));
      Test_Incremental_To_Joint ((0.0, 0.04, 0.0, 0.0, 0.0, 0.0));
      Test_Incremental_To_Joint ((0.0, 0.0, 0.08, 0.0, 0.0, 0.0));
      Test_Incremental_To_Joint ((0.0, 0.0, 0.02, 0.12, 0.0, 0.0));
      Test_Incremental_To_Joint ((0.0, 0.04, 0.0, 0.0, 0.24, 0.0));
      Test_Incremental_To_Joint ((0.08, 0.0, 0.0, 0.0, 0.0, 0.48));

      Put_Line ("Test iteration limits");
      Test_Incremental_To_Joint ((0.08, 0.0, 0.0, 0.0, 0.0, 0.48), Iteration_Limit => 5);
      Test_Incremental_To_Joint ((0.08, 0.0, 0.0, 0.0, 0.0, 0.48), Iteration_Limit => 4);

      Put_Line ("Test accuracy limits");
      Test_Incremental_To_Joint ((0.02, 0.0, 0.0, 0.0, 0.0, 0.0), Accuracy => (others => 1.0e-7));
      Test_Incremental_To_Joint ((0.02, 0.0, 0.0, 0.0, 0.0, 0.0), Accuracy => (others => 1.0e-8));
      Test_Incremental_To_Joint ((0.02, 0.0, 0.0, 0.0, 0.0, 0.0), Accuracy => (others => 1.0e-9));

      Put_Line ("Testing Slow_To_Joint");
      Test_To_Joint
        (Goal_Joint => Nominal_Position,
         Guess =>      Nominal_Position);
      Test_To_Joint
        (Goal_Joint => (0.0, -Pi/2.0, Pi / 4.0, 0.0, -Pi/2.0, 0.0),
         Guess =>      (0.0, -Pi/4.0, Pi / 4.0, 0.0, -Pi/2.0, 0.0));
      Test_To_Joint
        (Goal_Joint => (0.0, -Pi/2.0, Pi / 2.0, 0.0, -Pi/2.0, 0.0),
         Guess =>      (0.0, -Pi/4.0, Pi / 4.0, 0.0, -Pi/2.0, 0.0));

      Put_Line ("Force more than one partition by going thru wrist singularity");
      Test_To_Joint
        (Goal_Joint => (0.0, -Pi/4.0, Pi / 4.0, 0.0, -2.0, 0.0),
         Guess =>      (0.0, -Pi/4.0, Pi / 4.0, 0.0,  1.0, 0.0));

      Put_Line ("Force SINGULAR by starting at a singular position");
      Test_To_Joint
        (Goal_Joint => Nominal_Position,
         Guess =>      (others => 0.0));
   end Test_Inverse_Kinematics;

end Debug_Math_Float_Manipulator_6;
