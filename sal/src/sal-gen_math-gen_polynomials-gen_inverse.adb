--  Abstract :
--
--  See spec
--
--  Notice
--
--  Copyright (C) 2009 National Aeronautics and Space Administration.
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

package body SAL.Gen_Math.Gen_Polynomials.Gen_Inverse is

   function Derivative (Polynomial : in Coefficients_Type) return Coefficients_Type
   is
      Poly_Out : Coefficients_Type (Integer'Max (0, Polynomial'First - 1) .. Polynomial'Last - 1);
   begin
      for I in Poly_Out'Range loop
         Poly_Out (I) := Polynomial (I + 1) * Real_Type (I + 1);
      end loop;
      return Poly_Out;
   end Derivative;

   function Compute_Inverse
     (Argument       : in Real_Type;
      Coefficients   : in Coefficients_Type;
      Guess          : in Real_Type;
      Accuracy       : in Real_Type;
      Max_Iterations : in Integer)
     return Real_Type
   is
      Equation : Coefficients_Type (0 .. Coefficients'Last);

      function F (X : in Real_Type; Coefficients : in Coefficients_Type)return Real_Type renames Compute;

      function Root
        (Polynomial     : in Coefficients_Type;
         Initial_Guess  : in Real_Type;
         Epsilon        : in Real_Type;
         Max_Iterations : in Integer)
        return Real_Type
      is
         Derivative         : constant Coefficients_Type := Gen_Inverse.Derivative (Polynomial);
         Current_Value      : Real_Type;
         Current_Derivative : Real_Type;
         Guess              : Real_Type         := Initial_Guess;
         Iterations         : Integer           := 0;
      begin
         Current_Value         := F (Guess, Polynomial);
         while abs (Current_Value) > Epsilon and Iterations < Max_Iterations loop
            Current_Derivative := Compute (Guess, Derivative);

            --  if Current_Derivative is 0.0 (or small enough), this
            --  will raise Constraint_Error, indicating no solution.
            Guess := Guess - Current_Value / Current_Derivative;

            Iterations := Iterations + 1;
            if Iterations >= Max_Iterations then
               raise Range_Error with "Max Iterations" &Integer'Image (Max_Iterations)&" exceeded";
            end if;
            Current_Value := F (Guess, Polynomial);
         end loop;
         return Guess;
      end Root;

   begin
      if Coefficients'First = 0 then
         Equation := Coefficients;
         Equation (0) := Equation (0) - Argument;
      else
         Equation (0)                           := -Argument;
         Equation (1 .. Coefficients'First - 1) := (others => 0.0);
         Equation (Coefficients'Range)          := Coefficients;
      end if;

      return Root (Equation, Guess, Accuracy, Max_Iterations);

   end Compute_Inverse;

end SAL.Gen_Math.Gen_Polynomials.Gen_Inverse;


