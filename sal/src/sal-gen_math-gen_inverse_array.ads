--  Abstract:
--
--  Operations on non-square matrices with different indices on rows
--  and columns, and real elements, and on the inverse matrices.
--
--  Design Decisions:
--
--  The array types are generic formal parameters to allow users to --
--  specify their own names without subtypes.
--
--  The restriction A_Index_Type'length <= B_Index_Type'length ensures
--  that A_Array_B is redundant, rather than over-constrained, so
--  it has a psuedo-inverse.
--

generic
   type A_Index_Type is (<>);
   type B_Index_Type is (<>); -- A_Index_Type'length <= B_Index_Type'length
   type A_Array_Real_Type is array (A_Index_Type) of Real_Type;
   type B_Array_Real_Type is array (B_Index_Type) of Real_Type;
   type A_Array_BAR_Type is array (A_Index_Type) of B_Array_Real_Type;
   type B_Array_AAR_Type is array (B_Index_Type) of A_Array_Real_Type;
   type A_Array_AAR_Type is array (A_Index_Type) of A_Array_Real_Type;
   type B_Array_BAR_Type is array (B_Index_Type) of B_Array_Real_Type;
   with function Inverse (Right : in A_Array_AAR_Type) return A_Array_AAR_Type;
package SAL.Gen_Math.Gen_Inverse_Array is
   pragma Pure;

   function "-" (Right : in A_Array_BAR_Type) return A_Array_BAR_Type;
   function "-" (Left, Right : in A_Array_BAR_Type) return A_Array_BAR_Type;
   function "+" (Left, Right : in A_Array_BAR_Type) return A_Array_BAR_Type;
   function "*" (Left : in Real_Type; Right : in A_Array_BAR_Type) return A_Array_BAR_Type;

   function "-" (Right : in B_Array_AAR_Type) return B_Array_AAR_Type;
   function "-" (Left, Right : in B_Array_AAR_Type) return B_Array_AAR_Type;
   function "+" (Left, Right : in B_Array_AAR_Type) return B_Array_AAR_Type;
   function "*" (Left : in Real_Type; Right : in B_Array_AAR_Type) return B_Array_AAR_Type;

   function "*" (Left : in A_Array_BAR_Type; Right : in B_Array_Real_Type) return A_Array_Real_Type;

   function "*" (Left : in A_Array_BAR_Type; Right : in A_Array_Real_Type) return B_Array_Real_Type;
   --  Transpose (Left) * Right

   function "*" (Left : in B_Array_AAR_Type; Right : in A_Array_Real_Type) return B_Array_Real_Type;

   function "*" (Left : in B_Array_AAR_Type; Right : in B_Array_Real_Type) return A_Array_Real_Type;
   --  Transpose (Left) * Right

   function Square_Right (Right : in A_Array_BAR_Type) return A_Array_AAR_Type;
   --  Right * Transpose (Right)

   function "*" (Left : in A_Array_BAR_Type; Right : in B_Array_AAR_Type) return A_Array_AAR_Type;
   function "*" (Left : in B_Array_AAR_Type; Right : in A_Array_BAR_Type) return B_Array_BAR_Type;

   function "*" (Left : in A_Array_BAR_Type; Right : in A_Array_AAR_Type) return B_Array_AAR_Type;
   --  Transpose (Left) * Right

   function Inverse (Right : in A_Array_BAR_Type) return B_Array_AAR_Type;
   --  Invert Right, using Moore-Penrose psuedo-inverse.
   --
   --  raises SINGULAR if inverse cannot be found.

end SAL.Gen_Math.Gen_Inverse_Array;
