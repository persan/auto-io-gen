--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2005 Stephen Leake.  All Rights Reserved.
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


with SAL.Gen_Math.Gen_Polynomial_Fit;
with SAL.Gen_Math.Gen_Square_Array.Gen_Inverse;
with SAL.Math_Float.AUnit;
with SAL.Math_Float.Elementary;
package body Test_Math_Float_Polynomial_Fit is

   use SAL.Math_Float;

   procedure Test_First_Order_Fit (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      type Index_Type is range 0 .. 1;
      type Index_Array_Real_Type is array (Index_Type) of Real_Type;
      type Square_Array_Type is array (Index_Type) of Index_Array_Real_Type;

      package Square_Array is new SAL.Math_Float.Gen_Square_Array
        (Index_Type, Index_Array_Real_Type, Square_Array_Type);

      function Inverse is new Square_Array.Gen_Inverse;

      package Polynomial_Fit is new SAL.Math_Float.Gen_Polynomial_Fit
        (Index_Type            => Index_Type,
         Index_Array_Real_Type => Index_Array_Real_Type,
         Square_Array_Type     => Square_Array_Type,
         Square_Array          => Square_Array,
         Inverse               => Inverse,
         Sqrt                  => SAL.Math_Float.Elementary.Sqrt);
      use Polynomial_Fit;

      procedure Check
        (Message        : in String;
         Computed       : in Data_Type;
         Expected_Coeff : in Index_Array_Real_Type;
         Expected_Sigma : in Index_Array_Real_Type)
      is
         use SAL.Math_Float.AUnit;
         Computed_Coeff : Index_Array_Real_Type;
         Computed_Sigma : Index_Array_Real_Type;
      begin
         Fit (Computed, Computed_Coeff, Computed_Sigma);
         for I in Index_Array_Real_Type'Range loop
            Check
              (Message & ".coeff" & Index_Type'Image (I),
               Computed_Coeff (I),
               Expected_Coeff (I),
               Tolerance => 2.0e-5);
            Check
              (Message & ".sigma" & Index_Type'Image (I),
               Computed_Sigma (I),
               Expected_Sigma (I),
               Tolerance => 1.0); -- round off error is large for 32 bit floats.
         end loop;
      end Check;

      Data : Data_Type;
   begin

      for I in 1 .. 10 loop
         Accumulate (Data, X => Real_Type (I), Y => 2.0 * Real_Type (I));
      end loop;
      Check ("case_one", Data, Expected_Coeff => (0.0, 2.0), Expected_Sigma => (0.0, 0.0));

      Reset (Data);

      for I in 1 .. 10 loop
         Accumulate (Data, X => Real_Type (I), Y => -3.5 - 4.0 * Real_Type (I));
      end loop;
      Check ("case_two", Data, Expected_Coeff => (-3.5, -4.0), Expected_Sigma => (0.0, 0.0));

   end Test_First_Order_Fit;

   procedure Test_Second_Order_Fit (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      type Index_Type is range 0 .. 2;
      type Index_Array_Real_Type is array (Index_Type) of Real_Type;
      type Square_Array_Type is array (Index_Type) of Index_Array_Real_Type;

      package Square_Array is new SAL.Math_Float.Gen_Square_Array
        (Index_Type, Index_Array_Real_Type, Square_Array_Type);

      function Inverse is new Square_Array.Gen_Inverse;

      package Polynomial_Fit is new SAL.Math_Float.Gen_Polynomial_Fit
        (Index_Type            => Index_Type,
         Index_Array_Real_Type => Index_Array_Real_Type,
         Square_Array_Type     => Square_Array_Type,
         Square_Array          => Square_Array,
         Inverse               => Inverse,
         Sqrt                  => SAL.Math_Float.Elementary.Sqrt);
      use Polynomial_Fit;

      procedure Check
        (Message        : in String;
         Computed       : in Data_Type;
         Expected_Coeff : in Index_Array_Real_Type;
         Expected_Sigma : in Index_Array_Real_Type)
      is
         use SAL.Math_Float.AUnit;
         Computed_Coeff : Index_Array_Real_Type;
         Computed_Sigma : Index_Array_Real_Type;
      begin
         Fit (Computed, Computed_Coeff, Computed_Sigma);
         for I in Index_Array_Real_Type'Range loop
            Check
              (Message & ".coeff" & Index_Type'Image (I),
               Computed_Coeff (I),
               Expected_Coeff (I),
               Tolerance => 1.0e-3);
            Check
              (Message & ".sigma" & Index_Type'Image (I),
               Computed_Sigma (I),
               Expected_Sigma (I),
               Tolerance => 2.0); -- round off error is large for 32 bit floats.
         end loop;
      end Check;

      Data : Data_Type;
   begin

      for I in 1 .. 10 loop
         Accumulate (Data, X => Real_Type (I), Y => 2.0 * Real_Type (I));
      end loop;

      Check ("Linear", Data, Expected_Coeff => (0.0, 2.0, 0.0), Expected_Sigma => (0.0, 0.0, 0.0));

      Reset (Data);

      for I in 1 .. 10 loop
         Accumulate
           (Data,
            X => Real_Type (I),
            Y => -3.5 - 4.0 * Real_Type (I) + 3.0 * Real_Type (I) * Real_Type (I));
      end loop;

      Check ("Linear", Data, Expected_Coeff => (-3.5, -4.0, 3.0), Expected_Sigma => (0.0, 0.0, 0.0));

   end Test_Second_Order_Fit;

   ----------
   --  Public routines

   function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Math_Float.Polynomial_Fit");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_First_Order_Fit'Access, "Test_First_Order_Fit");
      Register_Routine (T, Test_Second_Order_Fit'Access, "Test_Second_Order_Fit");
   end Register_Tests;

end Test_Math_Float_Polynomial_Fit;
