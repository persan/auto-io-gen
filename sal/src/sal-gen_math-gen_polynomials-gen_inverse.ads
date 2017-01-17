--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

generic
package SAL.Gen_Math.Gen_Polynomials.Gen_Inverse is
   pragma Pure;

   function Compute_Inverse
     (Argument       : in Real_Type;
      Coefficients   : in Coefficients_Type;
      Guess          : in Real_Type;
      Accuracy       : in Real_Type;
      Max_Iterations : in Integer)
     return Real_Type;
   --  returns a value X such that:
   --
   --  |Compute (X, Coefficients) - Argument| < Accuracy
   --
   --  using Newton-Raphson method.
   --
   --  Guess is an initial value for X
   --
   --  Exception Range_Error is raised if Max_Iteration iterations
   --  occur before a result is found
   --
   --  Exception Range_Error is raised if First Derivative is zero
   --  during the search for the inverse
   --
   --  Coefficients defines a function Y = F(X)
   --
   --  Inverse implements the function X = G(Y)

   function Derivative (Polynomial : in Coefficients_Type) return Coefficients_Type;
   --  Compute first derivative coefficients

end SAL.Gen_Math.Gen_Polynomials.Gen_Inverse;

