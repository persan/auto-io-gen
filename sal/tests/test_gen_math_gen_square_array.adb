--  Abstract:
--
--  Test for Gen_Square_Array_Math
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen_Math.Gen_Square_Array;
with SAL.Gen_Array_Text_IO;
with SAL.Math_Float.Text_IO; use SAL.Math_Float.Text_IO;
procedure Test_Gen_Math_Gen_Square_Array
is
   use SAL.Math_Float;

   type Index_Type is (X, Y, Z);
   pragma Unreferenced (X, Y, Z);

   type Row_Type is array (Index_Type) of Real_Type;
   type Array_Type is array (Index_Type) of Row_Type;

   package Square_Array_Math is new SAL.Math_Float.Gen_Square_Array
      (Index_Type => Index_Type,
       Row_Type   => Row_Type,
       Array_Type => Array_Type);
   use Square_Array_Math;

   package Row_IO is new SAL.Gen_Array_Text_IO.Float_1D
      (Element_Type             => Real_Type,
       Index_Type               => Index_Type,
       Index_Array_Element_Type => Row_Type,
       Element_Put              => Put,
       Element_Get              => Get,
       Init_Default_Fore        => 3,
       Init_Default_Aft         => 5,
       Init_Default_Exp         => 0);
   use Row_IO;

   procedure Row_Put
     (File              : in Ada.Text_IO.File_Type;
      Item              : in Row_Type;
      Single_Line       : in Boolean               := False;
      Named_Association : in Boolean               := False)
   is
      pragma Unreferenced (Single_Line);
      pragma Unreferenced (Named_Association);
   begin
      Row_IO.Put (File, Item);
   end Row_Put;

   procedure Row_Get
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out Row_Type;
      Named_Association : in     Boolean               := False)
   is
      pragma Unreferenced (Named_Association);
   begin
      Row_IO.Get (File, Item);
   end Row_Get;

   package Array_IO is new SAL.Gen_Array_Text_IO.Private_1D
      (Element_Type                   => Row_Type,
       Index_Type                     => Index_Type,
       Index_Array_Element_Type       => Array_Type,
       Element_Put                    => Row_Put,
       Element_Get                    => Row_Get,
       Init_Default_Single_Line_Array => False);
   use Array_IO;

   A : constant Array_Type :=
      ((1.0,  2.0,  3.0),
       (5.0,  6.0,  7.0),
       (9.0, -1.0, -2.0));

   B : constant Row_Type := (2.0, -3.0, 4.0);


begin
   Put_Line ("Testing Gen_Math.Gen_Square_Array");

   Put_Line ("Identity => "); Put (Identity); New_Line;
   Put_Line ("A => "); Put (A); New_Line;
   Put_Line ("B => "); Put (B); New_Line;

   New_Line;

   Put_Line ("- A => "); Put (-A); New_Line;

   Put_Line ("A - A => "); Put (A - A); New_Line;
   Put_Line ("A + A => "); Put (A + A); New_Line;
   Put_Line ("A * A => "); Put (A * A); New_Line;

   Put_Line ("5.0 * A => "); Put (5.0 * A); New_Line;
   Put ("A * B => "); Put (A * B); New_Line;

   Put_Line ("Square_Right (A) => "); Put (Square_Right (A)); New_Line;
   Put_Line ("Square_Left  (A) => "); Put (Square_Left (A)); New_Line;
   Put_Line ("Times_Diag (A, B) => ");
   Put (Times_Diag (A, B)); New_Line;
   Put_Line ("Diag_Times (B, A) => ");
   Put (Diag_Times (B, A)); New_Line;
   Put_Line ("Transpose_Times (A, A) => ");
   Put (Transpose_Times (A, A)); New_Line;
   Put_Line ("Times_Transpose (A, A) => ");
   Put (Times_Transpose (A, A)); New_Line;

end Test_Gen_Math_Gen_Square_Array;
