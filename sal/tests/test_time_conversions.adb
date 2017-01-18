--  Abstract :
--
--  See spec
--
--  Copyright (C) 2001, 2004 - 2009 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with Interfaces;
with SAL.AUnit;                  use SAL.AUnit;
with SAL.Interfaces_More.AUnit;
with SAL.Math_Double.AUnit;
with SAL.Time_Conversions.AUnit; use SAL.Time_Conversions.AUnit;
package body Test_Time_Conversions is
   use SAL.Math_Double;
   use SAL.Time_Conversions;

   --  File_Name is relative to build directory.
   Leap_Table_File_Name : constant String := "../../Source_Common/Test/test_time_conversions-history.txt";
   Bad_Leap_Table_File_Name : constant String := "../../Source_Common/Test/bad_history.txt";
   ----------
   --  Test procedures

   procedure Test_Leap_Year (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Check ("Leap_Year (1)", Leap_Year (1), False);
      Check ("Leap_Year (2)", Leap_Year (2), False);
      Check ("Leap_Year (3)", Leap_Year (3), False);
      Check ("Leap_Year (4)", Leap_Year (4), True);
      Check ("Leap_Year (5)", Leap_Year (5), False);
      Check ("Leap_Year (6)", Leap_Year (6), False);
      Check ("Leap_Year (7)", Leap_Year (7), False);
      Check ("Leap_Year (8)", Leap_Year (8), True);
      Check ("Leap_Year (9)", Leap_Year (9), False);
   end Test_Leap_Year;

   procedure Test_Year_Day_Seconds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Message      : in String;
         Year         : in Integer;
         Day          : in Integer;
         Seconds      : in Time_Type;
         Expected_TAI : in Time_Type)
      is
         TAI_Time    : constant Time_Type := To_TAI_Time (Year, Day, Seconds, Absolute => True);
         Out_Year    : Integer;
         Out_Day     : Integer;
         Out_Seconds : Time_Type;
      begin
         Check (Message & " TAI", TAI_Time, Expected_TAI);

         To_Year_Day_Seconds (TAI_Time, Out_Year, Out_Day, Out_Seconds);

         Check (Message & ".year", Out_Year, Year);
         Check (Message & ".day", Out_Day, Day);
         Check (Message & ".Seconds", Out_Seconds, Seconds);

      end Check;
   begin

      Check
        ("1",
         Year         => 1958 + 0,
         Day          => 1,
         Seconds      => 0.0,
         Expected_TAI => 0.00);

      Check
        ("2",
         Year         => 1958 + 0,
         Day          => 20,
         Seconds      => 10.01,
         Expected_TAI => 1641610.01);

      Check
        ("3",
         Year         => 1958 + 5,
         Day          => 120,
         Seconds      => 20.01,
         Expected_TAI => 168048020.01);

   end Test_Year_Day_Seconds;

   procedure Test_Days_Seconds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Exp_Days    : Integer;
      Exp_Seconds : Time_Type;

      procedure Check
        (Message      : in String;
         Days         : in Integer;
         Seconds      : in Time_Type;
         Expected_TAI : in Time_Type)
      is
         TAI_Time    : constant Time_Type := To_TAI_Time (Days, Seconds);
         Out_Days    : Integer;
         Out_Seconds : Time_Type;
      begin
         Check (Message & " TAI", TAI_Time, Expected_TAI);

         To_Days_Seconds (TAI_Time, Out_Days, Out_Seconds);

         Check (Message & ".day", Out_Days, Days);
         Check (Message & ".Seconds", Out_Seconds, Seconds);

      end Check;
   begin

      Check
        ("1",
         Days         => 1,
         Seconds      => 0.0,
         Expected_TAI => 86400.00);

      Check
        ("2",
         Days         => 20,
         Seconds      => 10.01,
         Expected_TAI => 1728010.01);

      Check
        ("3",
         Days         => 1946,
         Seconds      => 20.01,
         Expected_TAI => 168134420.01);

      Check
        ("4",
         Days         => 106_000,
         Seconds      => 0.01,
         Expected_TAI => 9158400000.01);

      To_Days_Seconds (Time_Type'Last, Exp_Days, Exp_Seconds);

      Check
        ("5",
         Days         => Exp_Days,
         Seconds      => Exp_Seconds,
         Expected_TAI => Time_Type'Last);

      Check
        ("6",
         Days         => 0,
         Seconds      => 839.0,
         Expected_TAI => 839.0);

   end Test_Days_Seconds;

   procedure Test_Hour_Minute_Seconds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Message          : in String;
         Hour             : in Integer;
         Minute           : in Integer;
         Seconds          : in Time_Type;
         Expected_TAI     : in Time_Type;
         Expected_Hour    : in Integer;
         Expected_Minute  : in Integer;
         Expected_Seconds : in Time_Type)
      is
         TAI_Time    : constant Time_Type := To_TAI_Time (Hour, Minute, Seconds);
         Out_Hour    : Integer;
         Out_Minute  : Integer;
         Out_Seconds : Time_Type;
      begin
         Check (Message & " TAI", TAI_Time, Expected_TAI);

         To_Hour_Minute_Seconds (TAI_Time, Out_Hour, Out_Minute, Out_Seconds);

         Check (Message & ".Hour", Out_Hour, Expected_Hour);
         Check (Message & ".Minute", Out_Minute, Expected_Minute);
         Check (Message & ".Seconds", Out_Seconds, Expected_Seconds);

      end Check;
   begin

      Check
        ("1",
         Hour             => 0,
         Minute           => 0,
         Seconds          => 0.0,
         Expected_TAI     => 0.0,
         Expected_Hour    => 0,
         Expected_Minute  => 0,
         Expected_Seconds => 0.0);

      Check
        ("2",
         Hour             => 0,
         Minute           => 20,
         Seconds          => 10.01,
         Expected_TAI     => 1210.01,
         Expected_Hour    => 0,
         Expected_Minute  => 20,
         Expected_Seconds => 10.01);

      Check
        ("3",
         Hour             => 5,
         Minute           => 120,
         Seconds          => 20.01,
         Expected_TAI     => 25220.01,
         Expected_Hour    => 7,
         Expected_Minute  => 0,
         Expected_Seconds => 20.01);
   end Test_Hour_Minute_Seconds;

   procedure Test_String_TAI (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (ASIST        : in String;
         Expected_TAI : in Time_Type)
      is
         TAI_Time : constant Time_Type := To_TAI_Time (ASIST, Absolute => True);
      begin
         Check (ASIST & " TAI", TAI_Time, Expected_TAI);

         if ASIST'Length = 19 then
            Check (ASIST & " ASIST", To_ASIST_String (TAI_Time), ASIST);
         else
            Check (ASIST & " ASIST", To_ASIST_String (TAI_Time), ASIST (ASIST'First + 2 .. ASIST'Last));
         end if;
      end Check;
   begin

      Check ("70-001-00:00:00.000", 378691200.00);
      Check ("71-001-00:20:10.010", 410228410.01);
      Check ("69-001-05:40:20.010", 3502935620.01);
      Check ("00-021-05:40:20.010", 1327124420.01);
      Check ("10-021-05:40:20.010", 1642743620.01);
   end Test_String_TAI;

   procedure Test_TAI_Julian_Convert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Input_TAI                 : in Time_Type;
         Expected_Julian_Centuries : in SAL.Math_Double.Real_Type)
      is
         use SAL.Math_Double.AUnit;
         Days_Tolerance : constant := 2.0e-6;
         --  There are 8.64e+4 seconds in a day.

         Centuries_Tolerance : constant := 5.0e-11;
         --  There are 3.15576e+9 seconds in a Julian century.

      begin
         Check ("Day", To_Julian_Day (Input_TAI), Expected_Julian_Centuries * Days_Per_Julian_Century, Days_Tolerance);
         Check ("Century", To_Julian_Century (Input_TAI), Expected_Julian_Centuries, Centuries_Tolerance);
      end Check;

   begin
      Check (Time_Type (378691200), 66.8196440793977);  --  1970-001-00:00:00
      Check (Time_Type (410228410), 66.8296376181966);  --  1971-001-00:20:10
      Check (Time_Type (1327124420), 67.1201844310087); --  2000-021-05:40:20
      Check (Time_Type (1642743620), 67.2201981202626); --  2010-021-05:40:20
      Check (Time_Type (2272167620), 67.4196505501052); --  2030-001-05:40:20
      Check (Time_Type (3502935620), 67.8096573947322); --  2069-001-05:40:20
   end Test_TAI_Julian_Convert;

   procedure Test_Julian_TAI_Convert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Input_Julian_Day : in SAL.Math_Double.Real_Type;
         Expected_TAI     : in Time_Type)
      is begin
         Check ("", Julian_Days_TT_To_Seconds_TAI (Input_Julian_Day), Expected_TAI);
      end Check;

   begin
      Check (Julian_Days_1958_TAI + TT_Offset_Days, 0.0);
   end Test_Julian_TAI_Convert;

   procedure Test_J2000_Julian_Centuries (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Tolerance : constant Real_Type := 5.0e-11;
      --  There are 3.15576e+9 seconds in a Julian century.

      procedure Check (TAI_Time : in String; Expected_J2000_Julian_Centuries : in Real_Type)
      is begin
         SAL.Math_Double.AUnit.Check
           (TAI_Time,
            To_J2000_Julian_Centuries (To_TAI_Time (TAI_Time, Absolute => True)),
            Expected_J2000_Julian_Centuries,
            Tolerance);
      end Check;

      TT_Offset_Centuries : constant Real_Type := TT_Offset * Julian_Centuries_Per_Second;
   begin
      Check ("2000-001-12:00:00.000", 0.0 + TT_Offset_Centuries);
      Check ("2000-001-18:00:00.000", 0.25 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-002-12:00:00.000", 1.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-101-12:00:00.000", 100.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-365-12:00:00.000", 364.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-366-12:00:00.000", 365.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2000-367-12:00:00.000", 366.0 / 36525.0 + TT_Offset_Centuries);
      Check ("2001-001-12:00:00.000", 366.0 / 36525.0 + TT_Offset_Centuries); -- 2000 is a leap year
   end Test_J2000_Julian_Centuries;

   procedure Test_Extended_String_TAI (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (ASIST        : in String;
         Expected_TAI : in Time_Type)
      is
         TAI_Time : constant Time_Type := To_TAI_Time (ASIST, Absolute => True);
      begin
         Check (ASIST & " TAI", TAI_Time, Expected_TAI);

         Check (ASIST & " ASIST", To_Extended_ASIST_String (TAI_Time), ASIST);
      end Check;
   begin

      Check ("1970-001-00:00:00.000", 378691200.00);
      Check ("1971-001-00:20:10.010", 410228410.01);
      Check ("2069-001-05:40:20.010", 3502935620.01);
      Check ("2000-021-05:40:20.010", 1327124420.01);
      Check ("2010-021-05:40:20.010", 1642743620.01);
   end Test_Extended_String_TAI;

   procedure Test_Floor_Unsigned_16 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Interfaces_More.AUnit, Interfaces, AUnit.Assertions;
   begin
      Check ("1.0", Floor_Unsigned_16 (1.0), 1);
      Check ("1.1", Floor_Unsigned_16 (1.1), 1);
      Check ("0.000_000_001", Floor_Unsigned_16 (0.000_000_001), 0);
      Check ("1.000_000_001", Floor_Unsigned_16 (1.000_000_001), 1);
      Check ("~Unsigned_16'last", Floor_Unsigned_16 (65534.1), 65534);

      declare
         Temp : Unsigned_16;
         pragma Unreferenced (Temp);
      begin
         Temp := Floor_Unsigned_16 (65535.01);
         Assert (False, "> Unsigned_16'last did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

      declare
         Temp : Unsigned_16;
         pragma Unreferenced (Temp);
      begin
         Temp := Floor_Unsigned_16 (-0.01);
         Assert (False, "< 0 did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

   end Test_Floor_Unsigned_16;

   procedure Test_To_Microseconds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Interfaces_More.AUnit, Interfaces, AUnit.Assertions;
   begin
      Check ("1.0", Unsigned_32'(To_Microseconds (1.0)), 1_000_000);
      Check ("0.000_001", Unsigned_32'(To_Microseconds (0.000_001)), 1);

      declare
         Temp : Unsigned_32;
         pragma Unreferenced (Temp);
      begin
         Temp := To_Microseconds (Time_Type (Integer_64'Last / 2e9) + 0.000_001);
         Assert (False, "> Integer_64/2e9 did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

      declare
         Temp : Unsigned_32;
         pragma Unreferenced (Temp);
      begin
         Temp := To_Microseconds (Time_Type (Unsigned_32'Last) / 1_000_000 + 0.000_001);
         Assert (False, "> Unsigned_32'last did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

      Check ("1.0", Unsigned_16'(To_Microseconds (0.05)), 50_000);
      Check ("0.000_001", Unsigned_16'(To_Microseconds (0.000_001)), 1);

      declare
         Temp : Unsigned_16;
         pragma Unreferenced (Temp);
      begin
         Temp := To_Microseconds (Time_Type (Integer_64'Last / 2e9) + 0.000_001);
         Assert (False, "> Integer_64/2e9 did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

      declare
         Temp : Unsigned_16;
         pragma Unreferenced (Temp);
      begin
         Temp := To_Microseconds (Time_Type (Unsigned_16'Last) / 1_000_000 + 0.000_001);
         Assert (False, "> Unsigned_16'last did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

   end Test_To_Microseconds;

   procedure Test_Create_Leap_Second_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Leap_Table : constant Leap_Second_Table_Type := Create (Leap_Table_File_Name);
      procedure Check
        (Message              : in String;
         Index_Num            : in Integer;
         Expected_Start_Year  : in Integer;
         Expected_Start_Month : in Month_Type;
         Expected_Leap_Second : in Integer)
      is
      begin
         Check
           (Message & ".Start_TAI",
            Leap_Table (Index_Num).Start_TAI_Time,
            To_TAI_Time (Expected_Start_Year,
                         Day_Of_Year (Expected_Start_Year, Expected_Start_Month),
                         0.0,
                         Absolute => True));

         Check
           (Message & ".Leap_Second",
            Leap_Table (Index_Num).Leap_Second,
            Expected_Leap_Second);
      end Check;
   begin
      Check ("",
             Index_Num            => 1,
             Expected_Start_Year  => 1961,
             Expected_Start_Month => Jan,
             Expected_Leap_Second => 1);

      Check ("",
             Index_Num            => 13,
             Expected_Start_Year  => 1968,
             Expected_Start_Month => Feb,
             Expected_Leap_Second => 4);

      Check ("",
             Index_Num            => 30,
             Expected_Start_Year  => 1991,
             Expected_Start_Month => Jan,
             Expected_Leap_Second => 26);

      Check ("",
             Index_Num            => 36,
             Expected_Start_Year  => 1999,
             Expected_Start_Month => Jan,
             Expected_Leap_Second => 32);
   end Test_Create_Leap_Second_Table;

   procedure Test_Bad_Create_Leap_Second_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      declare
         Leap_Table : constant Leap_Second_Table_Type := Create (Bad_Leap_Table_File_Name);
         pragma Unreferenced (Leap_Table);
      begin
         null;
      end;
      AUnit.Assertions.Assert (False, "did not get exception");
   exception
   when E : SAL.Initialization_Error =>
      Check
        ("",
         Ada.Exceptions.Exception_Message (E),
         Bad_Leap_Table_File_Name &
           ": 9: start time not equal to previous end time.");
   end Test_Bad_Create_Leap_Second_Table;

   procedure Test_Calculate_Day_Of_Year (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      procedure Check
        (Message      : in String;
         Year         : in Integer;
         Month        : in Month_Type;
         Expected_Day : in Integer)
      is
         Out_Month : Integer;
      begin
         Out_Month := Day_Of_Year (Year, Month);
         Check (Message & ".Month", Out_Month, Expected_Day);
      end Check;
   begin
      Check ("",
             Year         => 1961,
             Month        => May,
             Expected_Day => 121);
      Check ("",
             Year         => 1960,
             Month        => Dec,
             Expected_Day => 336);

   end Test_Calculate_Day_Of_Year;

   procedure Test_TAI_To_UTC (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Leap_Table : constant Leap_Second_Table_Type := Create (Leap_Table_File_Name);
      procedure Check
        (Message      : in String;
         Table        : in Leap_Second_Table_Type;
         Input_Year   : in Integer;
         Input_Month  : in Month_Type;
         Expected_UTC : in Time_Type)
      is
         Out_UTC : Time_Type;
      begin
         Out_UTC :=
           TAI_To_UTC
           (To_TAI_Time (Input_Year,
                         Day_Of_Year (Input_Year, Input_Month),
                         0.0,
                         Absolute => True),
           Table);
         Check (Message & ".UTC", Out_UTC, Expected_UTC);
      end Check;
   begin

      Check ("Before table",
             Leap_Table,
             Input_Year   => 1960,
             Input_Month  => Jan,
             Expected_UTC => To_TAI_Time (1960, 1, 0.0, True));

      Check ("First line",
             Leap_Table,
             Input_Year   => 1961,
             Input_Month  => Jan,
             Expected_UTC => To_TAI_Time (1961, 1, 0.0, True) + 1.0);

      --   1970, Jan 1: TAI = 378691200.0, Leap seconds = 4.
      Check ("",
             Leap_Table,
             Input_Year   => 1970,
             Input_Month  => Jan,
             Expected_UTC => 378691204.000000000);

      --   1958, Jan 1: TAI = 0.0, Leap seconds = 0.
      Check ("",
             Leap_Table,
             Input_Year   => 1958,
             Input_Month  => Jan,
             Expected_UTC => 0.000000000);

      Check ("Last line",
             Leap_Table,
             Input_Year   => 2006,
             Input_Month  => Jan,
             Expected_UTC => To_TAI_Time (2006, 1, 0.0, True) + 33.0);

      --   2069, Jan 1: TAI = 3502915200.0, Leap seconds = 33.
      Check ("After table",
             Leap_Table,
             Input_Year   => 2069,
             Input_Month  => Jan,
             Expected_UTC => 3502915233.000000000);
   end Test_TAI_To_UTC;


   ----------
   --  Public subprograms

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Leap_Year'Access, "Test_Leap_Year");
      Register_Routine (T, Test_Year_Day_Seconds'Access, "Test_Year_Day_Seconds");
      Register_Routine (T, Test_Days_Seconds'Access, "Test_Days_Seconds");
      Register_Routine (T, Test_Hour_Minute_Seconds'Access, "Test_Hour_Minute_Seconds");
      Register_Routine (T, Test_String_TAI'Access, "Test_String_TAI");
      Register_Routine (T, Test_TAI_Julian_Convert'Access, "Test_TAI_Julian_Convert");
      Register_Routine (T, Test_Julian_TAI_Convert'Access, "Test_Julian_TAI_Convert");
      Register_Routine (T, Test_J2000_Julian_Centuries'Access, "Test_J2000_Julian_Centuries");
      Register_Routine (T, Test_Extended_String_TAI'Access, "Test_Extended_String_TAI");
      Register_Routine (T, Test_Floor_Unsigned_16'Access, "Test_Floor_Unsigned_16");
      Register_Routine (T, Test_To_Microseconds'Access, "Test_To_Microseconds");
      Register_Routine (T, Test_Calculate_Day_Of_Year'Access, "Test_Calculate_Day_Of_Year");
      Register_Routine (T, Test_Create_Leap_Second_Table'Access, "Test_Create_Leap_Second_Table");
      Register_Routine (T, Test_Bad_Create_Leap_Second_Table'Access, "Test_Bad_Create_Leap_Second_Table");
      Register_Routine (T, Test_TAI_To_UTC'Access, "Test_TAI_To_UTC");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Time_Conversions");
   end Name;

end Test_Time_Conversions;
