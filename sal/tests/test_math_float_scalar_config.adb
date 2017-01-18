--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2005, 2009 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
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
with SAL.AUnit; use SAL.AUnit;
with SAL.Config_Files;
with SAL.Math_Float.AUnit;
with SAL.Math_Float.Config;
with SAL.Math_Float.Scalar.AUnit; use SAL.Math_Float.Scalar.AUnit;
with SAL.Math_Float.Scalar.Config; use SAL.Math_Float.Scalar.Config;
package body Test_Math_Float_Scalar_Config is

   use SAL.Math_Float;
   use SAL.Math_Float.Scalar;

   procedure Test_Read_Scale_Limit (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SAL.Config_Files;

      Config   : Configuration_Type;
      Iterator : Iterator_Type;

      procedure Test_One
        (Key      : in String;
         Default  : in Scale_Limit_Type;
         Expected : in Scale_Limit_Type)
      is
         Computed : constant Scale_Limit_Type := Read (Config, Iterator, "Scale_Limit", Default, Missing_Key => Ignore);
      begin
         Check ("key check", Key, Current (Iterator));
         Check (Key, Computed, Expected);
         Next (Iterator);
      end Test_One;

      procedure Create_Config
      is
         use SAL.Math_Float.Config;
      begin
         Open (Config, "test_math_float_scalar_config.config", Read_Only => False);

         Write (Config, "1.Scale_Limit.Scale", 2.0);
         --  Offset is defaulted

         Write (Config, "2.Scale_Limit.Offset", 2.0);
         --  Scale is defaulted

         Write (Config, "3.Scale_Limit.Scale", -2.0);
         Write (Config, "3.Scale_Limit.Offset", 1.0);

         Write (Config, "4.Scale_Limit.Scale", -2.0);
         Write (Config, "4.Scale_Limit.Offset", 1.0);
         Write (Config, "4.Scale_Limit.High", 9.0);
         --  Override High

         Write (Config, "5.Scale_Limit.Scale", -2.0);
         Write (Config, "5.Scale_Limit.Offset", 1.0);
         Write (Config, "5.Scale_Limit.Low", -9.0);
         --  Override Low

         Write (Config, "6.Scale_Limit.Scale", -2.0);
         Write (Config, "6.Scale_Limit.Offset", 1.0);
         Write (Config, "6.Scale_Limit.High", 9.0);
         Write (Config, "6.Scale_Limit.Low", -9.0);
         --  Override both

         Write (Config, "7.Scale_Limit.Scale", 1.0);
         Write (Config, "7.Scale_Limit.Offset", 0.0);
         --  Use with alternate default

      end Create_Config;

   begin

      SAL.Math_Float.AUnit.Default_Tolerance := 1.0e-5;

      Create_Config;

      Iterator := First (Config);

      Test_One
        ("1",
         Default  => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected => (Scale => 2.0, Offset => 0.0, Limit => To_Limit (High => 5.0, Low => -5.0)));

      Test_One
        ("2",
         Default  => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected => (Scale => 1.0, Offset => 2.0, Limit => To_Limit (High => 8.0, Low => -12.0)));

      Test_One
        ("3",
         Default  => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected => (Scale => -2.0, Offset => 1.0, Limit => To_Limit (High => 5.5, Low => -4.5)));

      Test_One
        ("4",
         Default  => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected => (Scale => -2.0, Offset => 1.0, Limit => To_Limit (High => 9.0, Low => -4.5)));

      Test_One
        ("5",
         Default  => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected => (Scale => -2.0, Offset => 1.0, Limit => To_Limit (High => 5.5, Low => -9.0)));

      Test_One
        ("6",
         Default  => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)),
         Expected => (Scale => -2.0, Offset => 1.0, Limit => To_Limit (High => 9.0, Low => -9.0)));

      Test_One
        ("7",
         Default  => (Scale => 2.0, Offset => 1.0, Limit => To_Limit (High => 4.5, Low => -5.5)),
         Expected => (Scale => 1.0, Offset => 0.0, Limit => To_Limit (High => 10.0, Low => -10.0)));

      Close (Config, Unread_Key => Raise_Exception);
   end Test_Read_Scale_Limit;

   --------------------
   --  Public subprograms

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Read_Scale_Limit'Access, "Test_Read_Scale_Limit");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_Scalar_Config");
   end Name;

end Test_Math_Float_Scalar_Config;
