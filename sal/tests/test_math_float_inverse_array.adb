--  Abstract:
--
--  Test for SAL.Gen_Math.Gen_Inverse_Array. Assumes
--  SAL.Gen_Math.Gen_Square_Array is fully tested.
--
--  Design Decisions:
--
--  Compare results to Macsyma; see test_generic_inverse_array_math.mac
--
--  Copyright (C) 2002 - 2005, 2009 Stephen Leake.  All Rights Reserved.
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
--
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen_Math.Gen_Inverse_Array;
with SAL.Gen_Math.Gen_Square_Array.Gen_Inverse;
with SAL.Math_Float.Text_IO;
with Common_Test_Generic_Math; use Common_Test_Generic_Math;
procedure Test_Math_Float_Inverse_Array
is
   use SAL.Math_Float;

   Three_Vector : constant Three_Array_Real_Type := (2.0, -3.0, 4.0);
   Four_Vector : constant Four_Array_Real_Type := (2.0, 3.0, 4.0, 5.0);

   Three_By_Three : constant Three_By_Three_Type :=
     ((1.0, 2.0, 3.0),
      (5.0, 6.0, 7.0),
      (9.0, -1.0, -2.0));

   Three_By_Four : constant Three_By_Four_Type :=
     ((1.0, 2.0, 3.0, 4.0),
      (5.0, 6.0, 7.0, 8.0),
      (9.0, -1.0, -2.0, -3.0));
   Four_By_Three : constant Four_By_Three_Type :=
     ((1.0, 2.0, 3.0),
      (4.0, 5.0, 6.0),
      (7.0, 8.0, 9.0),
      (-1.0, -2.0, -3.0));

   package Three_By_Three_Math is new SAL.Math_Float.Gen_Square_Array
     (Index_Type => Three_Index_Type,
      Row_Type   => Three_Array_Real_Type,
      Array_Type => Three_By_Three_Type);

   function Inverse is new Three_By_Three_Math.Gen_Inverse;

   package Three_By_Four_Math is new SAL.Math_Float.Gen_Inverse_Array
     (A_Index_Type      => Three_Index_Type,
      B_Index_Type      => Four_Index_Type,
      A_Array_Real_Type => Three_Array_Real_Type,
      B_Array_Real_Type => Four_Array_Real_Type,
      A_Array_BAR_Type  => Three_By_Four_Type,
      B_Array_AAR_Type  => Four_By_Three_Type,
      A_Array_AAR_Type  => Three_By_Three_Type,
      B_Array_BAR_Type  => Four_By_Four_Type,
      Inverse           => Inverse);
   use Three_By_Four_Math;

begin
   SAL.Math_Float.Text_IO.Real_Text_IO.Default_Fore := 3;  -- space, sign, 1 digit.
   SAL.Math_Float.Text_IO.Real_Text_IO.Default_Aft  := 5;  -- total of 6 digits.
   SAL.Math_Float.Text_IO.Real_Text_IO.Default_Exp  := 0;
   Put_Line ("Testing Generic_Inverse_Array_Math");

   Put_Line ("Three_Vector => "); Three_Array_Float_IO.Put (Three_Vector); New_Line;
   Put_Line ("Four_Vector => "); Four_Array_Float_IO.Put (Four_Vector); New_Line;
   Put_Line ("Three_By_Three => "); Three_By_Three_IO.Put (Three_By_Three); New_Line;
   Put_Line ("Three_By_Four => "); Three_By_Four_IO.Put (Three_By_Four); New_Line;
   Put_Line ("Four_By_Three => "); Four_By_Three_IO.Put (Four_By_Three); New_Line;

   New_Line;

   Put_Line ("- Three_By_Four => "); Three_By_Four_IO.Put (-Three_By_Four); New_Line;
   Put_Line ("Three_By_Four - Three_By_Four => "); Three_By_Four_IO.Put (Three_By_Four - Three_By_Four); New_Line;
   Put_Line ("Three_By_Four + Three_By_Four => "); Three_By_Four_IO.Put (Three_By_Four + Three_By_Four); New_Line;
   Put_Line ("5.0 * Three_By_Four => "); Three_By_Four_IO.Put (5.0 * Three_By_Four); New_Line;

   New_Line;

   Put_Line ("- Four_By_Three => "); Four_By_Three_IO.Put (-Four_By_Three); New_Line;
   Put_Line ("Four_By_Three - Four_By_Three => "); Four_By_Three_IO.Put (Four_By_Three - Four_By_Three); New_Line;
   Put_Line ("Four_By_Three + Four_By_Three => "); Four_By_Three_IO.Put (Four_By_Three + Four_By_Three); New_Line;
   Put_Line ("5.0 * Four_By_Three => "); Four_By_Three_IO.Put (5.0 * Four_By_Three); New_Line;

   New_Line;

   Put ("Three_By_Four * Four_Vector => "); Three_Array_Float_IO.Put (Three_By_Four * Four_Vector); New_Line;
   Put ("Transpose (Three_By_Four) * Three_Vector => "); Four_Array_Float_IO.Put (Three_By_Four * Three_Vector);
   New_Line;

   Put ("Four_By_Three * Three_Vector => "); Four_Array_Float_IO.Put (Four_By_Three * Three_Vector); New_Line;
   Put ("Transpose (Four_By_Three) * Four_Vector => "); Three_Array_Float_IO.Put (Four_By_Three * Four_Vector);
   New_Line;

   Put ("Square_Right (Three_By_Four) => "); Three_By_Three_IO.Put (Square_Right (Three_By_Four)); New_Line;

   Put ("Three_By_Four * Four_By_Three => "); Three_By_Three_IO.Put (Three_By_Four * Four_By_Three); New_Line;
   Put ("Four_By_Three * Three_By_Four => "); Four_By_Four_IO.Put (Four_By_Three * Three_By_Four); New_Line;

   Put ("Transpose (Three_By_Four) * Three_By_Three => "); Four_By_Three_IO.Put (Three_By_Four * Three_By_Three);
   New_Line;

   Put ("Inverse (Three_By_Four) => "); Four_By_Three_IO.Put (Inverse (Three_By_Four)); New_Line;

end Test_Math_Float_Inverse_Array;
