--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2005 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with SAL.Math_Float.Stats.Image;
package body Test_Math_Float_Stats is

   use SAL.Math_Float;

   Default_Mean_Threshold : constant := 10.0e-5;
   Default_SD_Threshold   : constant := 10.0e-7;

   procedure Check
     (Message        : in String;
      Computed       : in Real_Type;
      Expected       : in Real_Type;
      Threshold : in SAL.Math_Float.Real_Type)
   is begin
      AUnit.Assertions.Assert
        (abs (Computed - Expected) < Threshold,
         Message &
           " failed; expected " & Real_Type'Image (Expected) &
           " got " & Real_Type'Image (Computed));
   end Check;

   procedure Check
     (Message        : in String;
      Computed       : in Stats.Stats_Type;
      Mean           : in Real_Type;
      SD             : in Real_Type;
      Min            : in Real_Type;
      Max            : in Real_Type;
      Mean_Threshold : in SAL.Math_Float.Real_Type := Default_Mean_Threshold;
      SD_Threshold   : in SAL.Math_Float.Real_Type := Default_SD_Threshold)
   is
      Computed_Mean : constant Real_Type := Stats.Mean (Computed);
      Computed_SD   : constant Real_Type := Stats.Standard_Deviation (Computed);
      Computed_Min  : constant Real_Type := Stats.Min (Computed);
      Computed_Max  : constant Real_Type := Stats.Max (Computed);
   begin
      Check (Message & ".Mean", Computed_Mean, Mean, Mean_Threshold);
      Check (Message & ".SD", Computed_SD, SD, SD_Threshold);
      Check (Message & ".Min", Computed_Min, Min, Mean_Threshold);
      Check (Message & ".Max", Computed_Max, Max, Mean_Threshold);
   end Check;

   procedure Check
     (Message : in String;
      Computed : in String;
      Expected : in String)
   is begin
      AUnit.Assertions.Assert
        (Computed = Expected,
         Message &
           " failed; expected '" & Expected &
           "' got '" & Computed);
   end Check;

   procedure Test_Stats (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Stats;
      Stats : Stats_Type;
   begin
      for I in 1 .. 10 loop
         if I <= 5 then
            Accumulate (Stats, 1.1);
         else
            Accumulate (Stats, 0.9);
         end if;
      end loop;
      Check ("case_one", Stats, Mean => 1.0, SD => 0.105411, Min => 0.9, Max => 1.1);

      declare
         Stats_Image : constant String := Image.Image
           (Display (Stats),
            Mean_Fore => 1,
            Mean_Aft  => 1,
            Mean_Exp  => 0,
            SD_Fore   => 1,
            SD_Aft    => 4,
            SD_Exp    => 3);
      begin
         Check ("case_one.image", Stats_Image, "( 1.0, 1.0541E-01,  0.9,  1.1)");
      end;

      Reset (Stats);

      for I in 1 .. 10 loop
         if I <= 5 then
            Accumulate (Stats, -10.1);
         else
            Accumulate (Stats, -9.9);
         end if;
      end loop;

      declare
         Stats_Image : constant String := Image.Image
           (Display (Stats),
            Mean_Fore => 1,
            Mean_Aft  => 1,
            Mean_Exp  => 0,
            SD_Fore   => 1,
            SD_Aft    => 4,
            SD_Exp    => 3);
      begin
         Check ("overflow.image", Stats_Image, "(****, 1.0538E-01, ****, -9.9)");
      end;

      declare
         Stats_Image : constant String := Image.Image
           (Display (Stats),
            Mean_Fore => 2,
            Mean_Aft  => 2,
            Mean_Exp  => 0,
            SD_Fore   => 1,
            SD_Aft    => 4,
            SD_Exp    => 3);
      begin
         Check ("case_two.image", Stats_Image, "(-10.00, 1.0538E-01, -10.10,  -9.90)");
      end;
   end Test_Stats;

   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float_Stats");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Stats'Access, "Test_Stats");
   end Register_Tests;

end Test_Math_Float_Stats;
