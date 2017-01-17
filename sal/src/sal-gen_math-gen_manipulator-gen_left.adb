--  Abstract:
--
--  see spec
--
--  Modification History:
--
--     Oct. 9, 1991     Dana Miller     Created
--     Dec. 31, 1991    Dana Miller
--      24 Jan 1992     Stephe Leake
--          match spec changes. localize use.
--      24 June 1992    Stephe Leake
--          match spec changes.
--       8 July 1992    Stephe Leake
--          match changes to Math_6_DOF spec
--       9 Nov 1992     Stephe Leake
--          match spec changes
--  30 Nov 1993 Stephe Leake
--      match spec changes
--  27 Sept 1994    Victoria Buckland
--      Utilized Manipulator_Math.Mult functions in Slow_T0_T_Obj,
--      Slow_T0_T_Ti, and Slow_Ti_T_Obj.
--  21 May 2002 Stephe Leake
--      match spec changes

package body SAL.Gen_Math.Gen_Manipulator.Gen_Left is

   --  Jacobians

   function Slow_Jacobian (Ti_T_Obj : in Joint_Array_Pose_Type) return Jacobian_Type
   is
      Jacobian_Column : Math_DOF_6.Dual_Cart_Vector_Type;
      Result          : Jacobian_Type;
   begin
      for Joint in Joint_Index_Type loop
         Jacobian_Column := Math_Den_Hart_Left.Partial_Jacobian (Ti_T_Obj (Joint));

         --  Result cannot be assigned by slicing so it is assigned
         --  with a loop instead.
         for Axis in Math_DOF_6.Dual_Cart_Axis_Type loop
            Result (Axis)(Joint) := Jacobian_Column (Axis);
         end loop;
      end loop;
      return Result;
   end Slow_Jacobian;

   function Transform_Jacobian
     (Current_T_New : in Math_DOF_6.Pose_Type;
      Jacobian      : in Jacobian_Type)
     return Jacobian_Type
   is
      use Math_DOF_6;
      use Math_DOF_6_Left;
      Result          : Jacobian_Type;
      Object_Velocity : Dual_Cart_Vector_Type;
   begin
      for Joint in Joint_Index_Type
      loop
         Object_Velocity := Transform_Rate
           (Current_T_New,
            Dual_Cart_Vector_Type'
              (Jacobian (TX)(Joint),
               Jacobian (TY)(Joint),
               Jacobian (TZ)(Joint),
               Jacobian (RX)(Joint),
               Jacobian (RY)(Joint),
               Jacobian (RZ)(Joint)));

         --  Result cannot be assigned by slicing so it is assigned
         --  with a loop instead.
         for Axis in Dual_Cart_Axis_Type loop
            Result (Axis)(Joint) := Object_Velocity (Axis);
         end loop;
      end loop;
      return Result;
   end Transform_Jacobian;

   function "*" (Left : in Math_DOF_6.Rate_Transform_Type; Right : in Jacobian_Type) return Jacobian_Type
   is
      use Math_DOF_6;
      use Math_DOF_6_Left;
      Result          : Jacobian_Type;
      Object_Velocity : Dual_Cart_Vector_Type;
   begin
      for Joint in Joint_Index_Type loop
         Object_Velocity := Left * Dual_Cart_Vector_Type'
           (Right (TX)(Joint),
            Right (TY)(Joint),
            Right (TZ)(Joint),
            Right (RX)(Joint),
            Right (RY)(Joint),
            Right (RZ)(Joint));

         --  Result cannot be assigned by slicing so is assigned with a loop instead.
         for Axis in Dual_Cart_Axis_Type loop
            Result (Axis)(Joint) := Object_Velocity (Axis);
         end loop;
      end loop;
      return Result;
   end "*";

   --  Projectors

   --  Kinematics

   function Slow_T0_T_Obj
     (Joint      : in Joint_Array_Real_Type;
      Den_Hart   : in Joint_Array_Den_Hart_Type;
      Tlast_T_Tp : in Math_DOF_6.Pose_Type;
      Tp_T_Obj   : in Math_DOF_6.Pose_Type)
     return Math_DOF_6.Pose_Type
   is
      use Math_DOF_6;
      use Math_DOF_6_Left;
      Result : Pose_Type := Zero_Pose;
   begin
      for I in Joint'First .. Joint'Last loop
         Result := Math_Den_Hart_Left.Mult (Result, Den_Hart (I), Joint (I));
      end loop;

      return Result * Tlast_T_Tp * Tp_T_Obj;
   end Slow_T0_T_Obj;

   function Slow_T0_T_Ti
     (Joint    : in Joint_Array_Real_Type;
      Den_Hart : in Joint_Array_Den_Hart_Type)
     return Joint_Array_Pose_Type
   is
      Result : Joint_Array_Pose_Type;
   begin
      Result (Result'First) :=
        Math_Den_Hart_Left.To_Pose (Den_Hart (Joint_Index_Type'First), Joint (Joint_Index_Type'First));

      for I in Joint_Index_Type'Succ (Joint_Index_Type'First) .. Joint_Index_Type'Last loop
         Result (I) := Math_Den_Hart_Left.Mult (Result (Joint_Index_Type'Pred (I)), Den_Hart (I), Joint (I));
      end loop;
      return Result;
   end Slow_T0_T_Ti;

   procedure Slow_Ti_T_Obj
     (Joint      : in     Joint_Array_Real_Type;
      Den_Hart   : in     Joint_Array_Den_Hart_Type;
      Tlast_T_Tp : in     Math_DOF_6.Pose_Type;
      Tp_T_Obj   : in     Math_DOF_6.Pose_Type;
      Ti_T_Obj   :    out Joint_Array_Pose_Type;
      T0_T_Obj   :    out Math_DOF_6.Pose_Type)
   is
      use Math_DOF_6;
      use Math_DOF_6_Left;
      Result : Joint_Array_Pose_Type;
   begin
      Result (Result'Last) := Tlast_T_Tp * Tp_T_Obj;
      for I in reverse Joint_Index_Type'First .. Joint_Index_Type'Pred (Joint_Index_Type'Last) loop
         Result (I) := Math_Den_Hart_Left.Mult
           (Den_Hart (Joint_Index_Type'Succ (I)),
            Joint (Joint_Index_Type'Succ (I)),
            Result (Joint_Index_Type'Succ (I)));
      end loop;
      Ti_T_Obj := Result;
      T0_T_Obj := Math_Den_Hart_Left.Mult (Den_Hart (Joint_Index_Type'First),
                                      Joint (Joint_Index_Type'First),
                                      Result (Joint_Index_Type'First));

   end Slow_Ti_T_Obj;

   function Inverse
     (T0_T_Ti     : in Joint_Array_Pose_Type;
      Tlast_T_Tp  : in Math_DOF_6.Pose_Type;
      TP_T_Obj    : in Math_DOF_6.Pose_Type)
     return Joint_Array_Pose_Type
   is
      use Math_DOF_6;
      use Math_DOF_6_Left;
      T0_T_Object : constant Pose_Type := T0_T_Ti (T0_T_Ti'Last) * Tlast_T_Tp * TP_T_Obj;
      Result      : Joint_Array_Pose_Type;
   begin
      for I in Joint_Index_Type
      loop
         Result (I) := Inverse_Times (T0_T_Ti (I), T0_T_Object);
      end loop;
      return Result;
   end Inverse;

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
      Joint            :    out Joint_Array_Real_Type)
   is
      use Math_DOF_3;
      use Math_DOF_6;
      use Math_DOF_6_Left;
      use Joint_Array_Real_Ops;

      Joint_Result    : Joint_Array_Real_Type := Guess;
      T0_T_Obj_Result : Pose_Type;
      Delta_Pose      : Dual_Cart_Vector_Type;
   begin
      for I in 1 .. Iteration_Limit loop
         Iterations := I;

         T0_T_Obj_Result := Slow_T0_T_Obj (Joint_Result, Den_Hart, Tlast_T_Tp, Tp_T_Obj);
         Delta_Pose      := T0_T_Obj - T0_T_Obj_Result;

         --  Update Joint_Result now, so some motion will be commanded
         --  even for steps smaller than Accuracy

         Joint_Result := Joint_Result + (Inverse_Jacobian * Delta_Pose);

         if Mag (Delta_Pose) <= Accuracy then
            Joint := Joint_Result;
            return;
         end if;

      end loop;

      raise Singular;
   end Slow_Incremental_To_Joint;

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
      Joint           :    out Joint_Array_Real_Type)
   is
      --  algorithm outline:
      --
      --    0) Set T0_T_Guess_Current := T0_T_Guess, T0_T_Goal_Current :=
      --       T0_T_Obj
      --
      --    1) attempt Newton-Raphson solution, starting at T0_T_Guess_Current,
      --       working to T0_T_Goal_Current, computing Jacobian each step.
      --
      --    2) if 1) fails, cut step in half: T0_T_Goal_Current :=
      --       (T0_T_Goal_Current - T0_T_Guess_Current)/2.0, goto 1.
      --
      --    3) if 1) succeeds, and T0_T_Goal_Current := T0_T_Goal, return. If
      --       not, set T0_T_Guess := T0_T_Goal_Current, T0_T_Goal_Current :=
      --       T0_T_Goal, goto 1.
      --

      use Math_Scalar;
      use Math_DOF_6;
      use Math_DOF_6.DCV_Ops;
      use Math_DOF_6_Left;
      use Joint_Array_Real_Ops;

      T0_T_Goal_Current   : Pose_Type             := T0_T_Obj;
      T0_T_Guess_Current  : Pose_Type;
      Joint_Guess_Current : Joint_Array_Real_Type := Guess;

      Joint_Pos_Current : Joint_Array_Real_Type;
      Ti_T_Obj          : Joint_Array_Pose_Type;
      T0_T_Obj_Current  : Pose_Type;
      Delta_Pose        : Dual_Cart_Vector_Type;
      Inverse_Jacobian  : Inverse_Jacobian_Type;

      Converged  : Boolean := False;
      Final_Goal : Boolean := True;

   begin

      for I in 1 .. Partition_Limit loop
         Partitions := I;

         T0_T_Guess_Current := Slow_T0_T_Obj
           (Joint      => Joint_Guess_Current,
            Den_Hart   => Den_Hart,
            Tlast_T_Tp => Tlast_T_Tp,
            Tp_T_Obj   => Tp_T_Obj);

         Joint_Pos_Current := Joint_Guess_Current;
         T0_T_Obj_Current  := T0_T_Guess_Current;

         begin
            for J in 1 .. Iteration_Limit loop
               Slow_Ti_T_Obj
                 (Joint      => Joint_Pos_Current,
                  Den_Hart   => Den_Hart,
                  Tlast_T_Tp => Tlast_T_Tp,
                  Tp_T_Obj   => Tp_T_Obj,
                  Ti_T_Obj   => Ti_T_Obj,
                  T0_T_Obj   => T0_T_Obj_Current);

               Delta_Pose := T0_T_Goal_Current - T0_T_Obj_Current;

               Converged  := Mag (Delta_Pose) <= Accuracy;

               exit when Converged and J > 1;

               Iterations        := J;

               Inverse_Jacobian  := Inverse (Slow_Jacobian (Ti_T_Obj));
               Joint_Pos_Current := Joint_Pos_Current + Inverse_Jacobian * Delta_Pose;
            end loop;
         exception
         when Singular =>
            Converged := False;
         end;

         if Converged then
            if Final_Goal then
               Joint := Joint_Pos_Current;
               return;
            else
               Final_Goal          := True;
               Joint_Guess_Current := Joint_Pos_Current;
               T0_T_Goal_Current   := T0_T_Obj;
            end if;
         else
            --  Cut current step in half, try again
            Final_Goal        := False;
            T0_T_Goal_Current := T0_T_Goal_Current - (T0_T_Goal_Current - T0_T_Guess_Current) / 2.0;
         end if;
      end loop;

      raise Singular;

   end Slow_To_Joint;

   --  Gravity and inertia

   function Slow_Inertia
     (Joint    : in Joint_Array_Real_Type;
      Den_Hart : in Joint_Array_Den_Hart_Type;
      Mass     : in Joint_Array_Mass_Type)
     return Inertia_Type
   is
      use Math_DOF_3;
      use Math_DOF_6;
      use Math_DOF_6_Left;
      use Math_Den_Hart;

      type Joint_Array_JAD_Type is array (Joint_Index_Type) of Joint_Array_DCV_Type;

      type Joint_Array_Wrench_Transform_Type is array (Joint_Index_Type) of Wrench_Transform_Type;
      type Joint_Array_JAWTS is array (Joint_Index_Type) of Joint_Array_Wrench_Transform_Type;

      Joint_Mass : Joint_Array_Mass_Type;
      --  Sum of link masses outboard of joint I

      Ti_W_Tj : Joint_Array_JAD_Type;
      --  wrench on joint I due to unit joint acceleration of joint J.
      --  only diag and upper triangle elements are set.

      Ti_PW_Tj : Joint_Array_JAWTS;
      --  Propagator from frame I to frame J. only upper triangle
      --  elements are set.

      Result : Inertia_Type;

      Last : constant Joint_Index_Type := Joint_Index_Type'Last;

      function Link_Wrench (Mass : in Mass_Type) return Dual_Cart_Vector_Type
      --  Return wrench exerted on link at link frame, due to unit
      --  acceleration of the joint at the link frame.
      is
         Unit_Z_Acceleration : constant Dual_Cart_Vector_Type := (0.0, 0.0, 0.0, 0.0, 0.0, 1.0);
      begin
         return (Mass * Unit_Z_Acceleration);
      end Link_Wrench;

      function Succ (Item : in Joint_Index_Type) return Joint_Index_Type renames Joint_Index_Type'Succ;
      function Pred (Item : in Joint_Index_Type) return Joint_Index_Type renames Joint_Index_Type'Pred;
   begin
      for I in Succ (Joint_Index_Type'First) .. Last
      loop
         --  Link_Wrench assumes revolute joints, so check that here.
         if Den_Hart (I).Class /= Math_Den_Hart.Revolute then
            raise Constraint_Error;
         end if;
         Ti_PW_Tj (Pred (I)) (I) := Math_Den_Hart.To_Inverse_Wrench_Transform (Den_Hart (I), Joint (I));
      end loop;

      for I in Joint_Index_Type'First .. Last
      loop
         Ti_PW_Tj (I) (I) := To_Wrench_Transform (Zero_Pose);
      end loop;

      Joint_Mass (Last)     := Mass (Last);
      Ti_W_Tj (Last) (Last) := Link_Wrench (Joint_Mass (Last));
      Result (Last) (Last)  := Ti_W_Tj (Last) (Last) (RZ);

      for I in reverse Joint_Index_Type'First .. Pred (Last)
      loop
         Joint_Mass (I) := Add
           (Mass (I),
            Joint_Mass (Succ (I)),
            Math_Den_Hart_Left.To_Pose (Den_Hart (Succ (I)), Joint (Succ (I))));

         Ti_W_Tj (I) (I) := Link_Wrench (Joint_Mass (I));
         Result (I) (I)  := Ti_W_Tj (I) (I) (RZ);

         for J in reverse Succ (I) .. Last
         loop
            Ti_PW_Tj (I) (J) := Ti_PW_Tj (I) (Succ (I)) * Ti_PW_Tj (Succ (I)) (J);
            Ti_W_Tj (I) (J)  := Ti_PW_Tj (I) (J) * Ti_W_Tj (J) (J);
            Result (I) (J)   := Ti_W_Tj (I) (J) (RZ);
            Result (J) (I)   := Result (I) (J);
         end loop;
      end loop;
      return Result;
   end Slow_Inertia;

   function Slow_Gravity_Torque
     (T0_T_Ti   : in Joint_Array_Pose_Type;
      T0_A_Grav : in Math_DOF_3.Cart_Vector_Type;
      Mass      : in Joint_Array_Mass_Type)
     return Joint_Array_Real_Type
   is
      use Math_Scalar;
      use Math_DOF_3;
      use Math_DOF_3_Left;
      use Math_DOF_3.Cart_Vector_Ops;
      use Math_DOF_6;
      use Math_DOF_6_Left;
      use Math_DOF_6.DCV_Ops;

      Result      : Joint_Array_Real_Type := (others => 0.0);
      Link_Wrench : Joint_Array_DCV_Type;

      function Succ (Item : in Joint_Index_Type) return Joint_Index_Type renames Joint_Index_Type'Succ;
      function Pred (Item : in Joint_Index_Type) return Joint_Index_Type renames Joint_Index_Type'Pred;

   begin

      Link_Wrench (Joint_Index_Type'Last) := Transform_Force
        (-Center (Mass (Joint_Index_Type'Last)),
         Total (Mass (Joint_Index_Type'Last)) *
           Inverse_Times (T0_T_Ti (Joint_Index_Type'Last).Rotation, T0_A_Grav));

      for Joint in reverse Joint_Index_Type'First .. Pred (Joint_Index_Type'Last) loop
         Link_Wrench (Joint) := Transform_Force
           (-Center (Mass (Joint)),
            Total (Mass (Joint)) * Inverse_Times (T0_T_Ti (Joint).Rotation, T0_A_Grav)) +
           Transform_Wrench (Inverse_Times (T0_T_Ti (Succ (Joint)), T0_T_Ti (Joint)), Link_Wrench (Succ (Joint)));

         Result (Joint) := Link_Wrench (Joint)(RZ);
      end loop;

      return Result;
   end Slow_Gravity_Torque;

end SAL.Gen_Math.Gen_Manipulator.Gen_Left;
