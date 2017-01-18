--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2007 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Test_Cases.Registration;
with Ada.Directories;
with Ada.Text_IO;
with SAL.AUnit;                   use SAL.AUnit;
with SAL.Config_Files;
with SAL.Math_Float.AUnit;
with SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_6.AUnit;  use SAL.Math_Float.DOF_6.AUnit;
with SAL.Math_Float.DOF_6.Config; use SAL.Math_Float.DOF_6.Config;
package body Test_Math_Float_DOF_6_Config is

   procedure Test_Read_Pose (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.Config_Files;
      use SAL.Math_Float.DOF_3;
      use SAL.Math_Float.DOF_6;

      File_Name : constant String := "test_math_float_dof_6_config.config";
      Config    : Configuration_Type;
      Iterator  : Iterator_Type;

      procedure Test_One
        (Key      : in String;
         Default  : in Pose_Type;
         Expected : in Pose_Type)
      is
         Computed : constant Pose_Type := Read (Config, Iterator, "Pose", Default, Missing_Key => Ignore);
      begin
         Check ("key", Key, Current (Iterator));
         Check (Key, Computed, Expected);
         Next (Iterator);
      end Test_One;

      procedure Create_Config
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         begin
            Ada.Directories.Delete_File (File_Name);
         exception
         when Name_Error =>
            null;
         end;

         Create (File, Out_File, File_Name);

         Put_Line (File, "1.Pose.Tran.X = 1.0");
         Put_Line (File, "1.Pose.Tran.Y = 2.0");
         Put_Line (File, "1.Pose.Tran.Z = 3.0");
         --  Quat is defaulted

         Put_Line (File, "2.Pose.Quat.X = 0.0");
         Put_Line (File, "2.Pose.Quat.Y = 1.0");
         Put_Line (File, "2.Pose.Quat.Z = 0.0");
         Put_Line (File, "2.Pose.Quat.S = 0.0");
         --  Tran is defaulted

         Put_Line (File, "3.Pose.Tran.X = 4.0");
         Put_Line (File, "3.Pose.Tran.Y = 5.0");
         Put_Line (File, "3.Pose.Tran.Z = 6.0");
         Put_Line (File, "3.Pose.Quat.X = 0.0");
         Put_Line (File, "3.Pose.Quat.Y = 0.0");
         Put_Line (File, "3.Pose.Quat.Z = 1.0");
         Put_Line (File, "3.Pose.Quat.S = 0.0");

         Close (File);
      end Create_Config;

   begin

      SAL.Math_Float.AUnit.Default_Tolerance := 1.0e-5;

      Create_Config;

      Open (Config, File_Name);

      Iterator := First (Config);

      Test_One
        ("1",
         Default  => Zero_Pose,
         Expected => ((1.0, 2.0, 3.0), Zero_Unit_Quaternion));

      Test_One
        ("2",
         Default  => Zero_Pose,
         Expected => ((0.0, 0.0, 0.0), Unchecked_Unit_Quaternion (0.0, 1.0, 0.0, 0.0)));

      Test_One
        ("3",
         Default  => Zero_Pose,
         Expected => ((4.0, 5.0, 6.0), Unchecked_Unit_Quaternion (0.0, 0.0, 1.0, 0.0)));

      Close (Config, Unread_Key => Raise_Exception);
   end Test_Read_Pose;

   --------------------
   --  Public subprograms

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Read_Pose'Access, "Test_Read_Pose");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_DOF_6_Config");
   end Name;

end Test_Math_Float_DOF_6_Config;
