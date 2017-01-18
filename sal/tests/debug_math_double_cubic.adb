with Ada.Text_IO;                     use Ada.Text_IO;
with SAL.Math_Double.Complex_Types;   use SAL.Math_Double.Complex_Types;
with SAL.Math_Double.Cubic;           use SAL.Math_Double.Cubic;
with SAL.Math_Double.Complex_Text_IO; use SAL.Math_Double.Complex_Text_IO;
with SAL.Math_Double.Text_IO;         use SAL.Math_Double.Text_IO;
procedure Debug_Math_Double_Cubic
is
   Coef : Coefficients_Type := (0.0, 1.0, 2.0, 3.0);
   --  We vary coef (0), and solve for x

   function Compute (Root : in Complex) return Complex
   is begin
      return Coef (0) + Root * (Coef (1) + Root * (Coef (2) + Root * Coef (3)));
   end Compute;

   Root_1 : Complex;
   Root_2 : Complex;
   Root_3 : Complex;

   --  These should all be zero
   Result_1 : Complex;
   Result_2 : Complex;
   Result_3 : Complex;
begin
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Fore := 5;
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Aft  := 4;
   SAL.Math_Double.Text_IO.Real_Text_IO.Default_Exp  := 0;

   SAL.Math_Double.Complex_Text_IO.Default_Fore := 5;
   SAL.Math_Double.Complex_Text_IO.Default_Aft  := 4;
   SAL.Math_Double.Complex_Text_IO.Default_Exp  := 0;

   Put_Line
     ("Discrim       coef(0)    root_1                 root_2                 root_3" &
        "              result_1              result_2                result_3");

   for I in 0 .. 10 loop
      Coef (0) := SAL.Math_Double.Real_Type (I);

      Put (Discriminant (Coef));

      Solve (Coef, Root_1, Root_2, Root_3);

      Result_1 := Compute (Root_1);
      Result_2 := Compute (Root_2);
      Result_3 := Compute (Root_3);

      Put (Coef (0));
      Put (Root_1);
      Put (Root_2);
      Put (Root_3);
      Put (Result_1);
      Put (Result_2);
      Put (Result_3);
      New_Line;
   end loop;
end Debug_Math_Double_Cubic;
