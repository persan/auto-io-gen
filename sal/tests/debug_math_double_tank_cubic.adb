--  Abstract :
--
--  debug parent
--
--  Notice
--
--  Copyright (C) 2006, 2007, 2009 National Aeronautics and Space Administration.
--  No copyright is claimed in the United States under Title 17, U.S.
--  Code. All Foreign Rights are Reserved to the U.S. Government.
--
--  Disclaimer
--
--  This software is provided "as is" without any warranty of any,
--  kind either express, implied, or statutory, including, but not
--  limited to, any warranty that the software will conform to,
--  specifications any implied warranties of merchantability, fitness
--  for a particular purpose, and freedom from infringement, and any
--  warranty that the documentation will conform to the program, or
--  any warranty that the software will be error free.
--
--  In no event shall NASA be liable for any damages, including, but
--  not limited to direct, indirect, special or consequential damages,
--  arising out of, resulting from, or in any way connected with the
--  software or its documentation.  Whether or not based upon warranty,
--  contract, tort or otherwise, and whether or not loss was sustained
--  from, or arose out of the results of, or use of, the software,
--  documentation or services provided hereunder.
--
--  Export Control
--
--  The recipient of this software from NASA shall not export or
--  re-export directly or indirectly (including via remote access,
--  i.e. Internet) any part of this software or its documentation to
--  any country for which a validated license is required for such
--  export or re-export under the EXPORT LAWS without first obtaining
--  such a validated license.

pragma License (Unrestricted);

with Ada.Numerics;                    use Ada.Numerics;
with Ada.Text_IO;                     use Ada.Text_IO;
with SAL.Math_Double.Complex_Text_IO; use SAL.Math_Double.Complex_Text_IO;
with SAL.Math_Double.Complex_Types;
with SAL.Math_Double.Cubic;           use SAL.Math_Double.Cubic;
with SAL.Math_Double.Text_IO;         use SAL.Math_Double.Text_IO;
procedure Debug_Math_Double_Tank_Cubic
is
   use SAL.Math_Double;
   use SAL.Math_Double.Complex_Types;

   Rho : constant Real_Type := 1000.0;
   R   : constant Real_Type := 0.5;

   --  In standard cubic eqn form, the equation we are solving is:
   --  0 = - pi rho/3 h**3 + pi R rho h**2 - m
   --
   --  We vary m and find h, using Cubic

   Coef : Coefficients_Type :=
     (0 => 0.0, --  -M
      1 => 0.0,
      2 => Pi * R * Rho,
      3 => -Pi * Rho / 3.0);

   function Mass (H : in Real_Type) return Real_Type
   is begin
      return Pi * Rho * (3.0 * H**2 * R - H**3) / 3.0;
   end Mass;

   M : Real_Type;
   H : Real_Type;

   Root_1 : Complex;
   Root_2 : Complex;
   Root_3 : Complex;

begin
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Fore := 5;
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Aft  := 4;
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Exp  := 0;

   SAL.Math_Double.Complex_Text_IO.Default_Fore := 5;
   SAL.Math_Double.Complex_Text_IO.Default_Aft  := 4;
   SAL.Math_Double.Complex_Text_IO.Default_Exp  := 0;

   Put_Line (" H        Discrim       Mass    root_1                 root_2                 root_3");

   for I in 0 .. 10 loop
      H := 2.0 * R * Real_Type (I) / 10.0;
      M := Mass (H);

      Coef (0) := -M;

      Solve (Coef, Root_1, Root_2, Root_3);

      Put (H, Fore => 2, Exp => 0, Aft => 4);
      Put (Discriminant (Coef), Exp => 2);
      Put (M, Fore => 5, Exp => 0, Aft => 4);
      Put (Root_1);
      Put (Root_2);
      Put (Root_3);
      New_Line;
   end loop;

end Debug_Math_Double_Tank_Cubic;

