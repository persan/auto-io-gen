--  Abstract :
--
--  debug SAL.Gen_Math.Gen_Tank
--
--  Notice
--
--  Copyright (C) 2006, 2007 National Aeronautics and Space Administration.
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

with Ada.Numerics;            use Ada.Numerics;
with Ada.Text_IO;             use Ada.Text_IO;
with SAL.Math_Double.Text_IO; use SAL.Math_Double.Text_IO;
with SAL.Math_Double.Tank;    use SAL.Math_Double.Tank;
procedure Debug_Math_Double_Tank
is
   use SAL.Math_Double;

   Rho : constant Real_Type := 1000.0;
   R   : constant Real_Type := 0.5;

   function Mass (H : in Real_Type) return Real_Type
   is begin
      return Pi * Rho * (3.0 * H**2 * R - H**3) / 3.0;
   end Mass;

   M          : Real_Type;
   H          : Real_Type;
   H_Computed : Real_Type;
   CM_Z       : Real_Type;
   I_zz       : Real_Type;
   I_xx       : Real_Type;
begin

   Put ("Rho => ");
   Put (Rho, Fore => 5, Exp => 0, Aft => 1);
   New_Line;

   Put ("R   => ");
   Put (R, Fore => 5, Exp => 0, Aft => 1);
   New_Line (2);

   Put_Line (" H         Mass     CM_x      Ixx       Izz");

   for I in 0 .. 10 loop
      H := 2.0 * R * Real_Type (I) / 10.0;
      Put (H, Fore => 2, Exp => 0, Aft => 4);

      M := Mass (H);
      Put (M, Fore => 5, Exp => 0, Aft => 4);

      Compute_Sphere_Sect (Rho, R, Mass (H), H_Computed, CM_Z, I_zz, I_xx);

      Put (CM_Z, Fore => 4, Exp => 0, Aft => 4);
      Put (I_zz, Fore => 5, Exp => 0, Aft => 4);
      Put (I_xx, Fore => 5, Exp => 0, Aft => 4);
      New_Line;
   end loop;

end Debug_Math_Double_Tank;
