--  Abstract:
--
--  Operations for parent that use left multiply operators.
--
--  Copyright (C) 2002, 2004, 2005, 2007 Stephen Leake.  All Rights Reserved.
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
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with SAL.Gen_Math.Gen_Den_Hart.Gen_Left;
with SAL.Gen_Math.Gen_DOF_3.Gen_Left;
with SAL.Gen_Math.Gen_DOF_6.Gen_Left;
generic
   with package Math_DOF_3_Left is new Math_DOF_3.Gen_Left;
   with package Math_DOF_6_Left is new Math_DOF_6.Gen_Left (Math_DOF_3_Left);
   with package Math_Den_Hart_Left is new Math_Den_Hart.Gen_Left (Math_DOF_3_Left);
package SAL.Gen_Math.Gen_Manipulator.Gen_Left is

   function Slow_Jacobian (Ti_T_Obj : in Joint_Array_Pose_Type) return Jacobian_Type;
   --  returns the Jacobian expressed in the object frame.
   --  See Slow_Ti_T_Obj below to compute Ti_T_Obj.

   function Transform_Jacobian
     (Current_T_New : in Math_DOF_6.Pose_Type;
      Jacobian      : in Jacobian_Type)
     return Jacobian_Type;
   function "*"
     (Left  : in Math_DOF_6.Rate_Transform_Type;
      Right : in Jacobian_Type)
     return Jacobian_Type;
   --  Change the frame of the Jacobian. Left is To_Rate_Transform (Current_T_New).

   --  Kinematics

   function Slow_T0_T_Obj
     (Joint      : in Joint_Array_Real_Type;
      Den_Hart   : in Joint_Array_Den_Hart_Type;
      Tlast_T_Tp : in Math_DOF_6.Pose_Type;
      Tp_T_Obj   : in Math_DOF_6.Pose_Type)
     return Math_DOF_6.Pose_Type;

   function Slow_T0_T_Ti
     (Joint    : in Joint_Array_Real_Type;
      Den_Hart : in Joint_Array_Den_Hart_Type)
     return Joint_Array_Pose_Type;

   procedure Slow_Ti_T_Obj
     (Joint      : in     Joint_Array_Real_Type;
      Den_Hart   : in     Joint_Array_Den_Hart_Type;
      Tlast_T_Tp : in     Math_DOF_6.Pose_Type;
      Tp_T_Obj   : in     Math_DOF_6.Pose_Type;
      Ti_T_Obj   :    out Joint_Array_Pose_Type;
      T0_T_Obj   :    out Math_DOF_6.Pose_Type);

   function Inverse
     (T0_T_Ti    : in Joint_Array_Pose_Type;
      Tlast_T_Tp : in Math_DOF_6.Pose_Type;
      TP_T_Obj   : in Math_DOF_6.Pose_Type)
     return Joint_Array_Pose_Type;
   --  returns Ti_T_Obj.

   procedure Slow_Incremental_To_Joint
     (T0_T_Obj         : in     Math_DOF_6.Pose_Type;
      Guess            : in     Joint_Array_Real_Type;
      Den_Hart         : in     Joint_Array_Den_Hart_Type;
      Tlast_T_Tp       : in     Math_DOF_6.Pose_Type;
      Tp_T_Obj         : in     Math_DOF_6.Pose_Type;
      Inverse_Jacobian : in     Inverse_Jacobian_Type;
      Accuracy         : in     Math_DOF_6.Dual_Real_Type := (0.001, 0.01);
      Iteration_Limit  : in     Integer := 3;
      Iterations       :    out Integer;
      Joint            :    out Joint_Array_Real_Type);
   --  Partial inverse kinematics; returns Joint such that
   --  Slow_T0_T_Obj (Joint) is within Accuracy of T0_T_Obj. Assumes
   --  Slow_T0_T_Obj (Guess) is close to T0_T_Obj, so that
   --  Inverse_Jacobian does not need to be recalculated during the
   --  iterations. Assumes Inverse_Jacobian is evaluated near Guess.
   --  Iterations is number of iterations needed; mainly for
   --  debugging, but may also be used to decide when to re-calculate
   --  Inverse_Jacobian.
   --
   --  Raises Singular if no solution found within -- Iteration_Limit
   --  cycles.

   procedure Slow_To_Joint
     (T0_T_Obj        : in     Math_DOF_6.Pose_Type;
      Guess           : in     Joint_Array_Real_Type;
      Den_Hart        : in     Joint_Array_Den_Hart_Type;
      Tlast_T_Tp      : in     Math_DOF_6.Pose_Type;
      Tp_T_Obj        : in     Math_DOF_6.Pose_Type;
      Accuracy        : in     Math_DOF_6.Dual_Real_Type := (0.001, 0.01);
      Partition_Limit : in     Integer := 10;
      Iteration_Limit : in     Integer := 5;
      Partitions      :    out Integer;
      Iterations      :    out Integer;
      Joint           :    out Joint_Array_Real_Type);
   --  Full numerical inverse kinematics. Guess must not be singular.
   --  Iterates until To_Pose (result) is within Accuracy of T0_T_Obj.
   --  Partition_Limit limits how many times the path from Guess to
   --  T0_T_Obj is partitioned. Iteration_Limit limits how long the
   --  algorithm spends at each partition before partitioning the path
   --  again.
   --
   --  Raises Singular if no solution found within iteration limits.
   --  Raises Constraint_Error if Joint_Index_Type'length /= 6.

   ----------
   --  Gravity and inertia

   function Slow_Inertia
     (Joint    : in Joint_Array_Real_Type;
      Den_Hart : in Joint_Array_Den_Hart_Type;
      Mass     : in Joint_Array_Mass_Type)
     return Inertia_Type;
   --  Mass is the mass of each link, expressed in the link frame,
   --  with the payload lumped into the last link.
   --
   --  Raises CONSTRAINT_ERROR if not all joints are REVOLUTE.

   function Slow_Gravity_Torque
     (T0_T_Ti   : in Joint_Array_Pose_Type;
      T0_A_Grav : in Math_DOF_3.Cart_Vector_Type;
      Mass      : in Joint_Array_Mass_Type)
     return Joint_Array_Real_Type;
   --  Returns torque at the joints due to the effects of T0_A_Grav.
   --  T0_A_Grav is normally the acceleration due to gravity, in frame
   --  T0. Mass must be expressed in joint frames.

end SAL.Gen_Math.Gen_Manipulator.Gen_Left;
