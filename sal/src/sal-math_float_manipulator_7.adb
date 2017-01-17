--  Abstract:
--
--  see spec
--

package body SAL.Math_Float_Manipulator_7 is

   function "&"
     (Left  : in SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
      Right : in SAL.Math_Float.Real_Type)
     return Joint_Array_Real_Type
   is
      use SAL.Math_Float.DOF_6;
   begin
      return (Left (TX), Left (TY), Left (TZ), Left (RX), Left (RY), Left (RZ), Right);
   end "&";

   function Cart (Right : in Joint_Array_Real_Type) return SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type
   is
      use SAL.Math_Float.DOF_6;
   begin
      return (Right (1), Right (2), Right (3), Right (4), Right (5), Right (6));
   end Cart;

   function Extend_Jacobian
     (Jacobian              : in Math.Jacobian_Type;
      Redundant_Joint_Index : in Joint_Index_Type   := 1)
     return Math.Joint_Array_JAR_Type
   is
      use Math, SAL.Math_Float.DOF_6;
      Extended_Jacobian : Joint_Array_JAR_Type;
   begin
      for I in Joint_Index_Type
      loop
         if I < 7 then
            Extended_Jacobian (I) := Jacobian (Dual_Cart_Axis_Type'Val (Joint_Index_Type'Pos (I) - 1));
         else
            Extended_Jacobian (I) := (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
            Extended_Jacobian (I)(Redundant_Joint_Index) := 1.0;
         end if;
      end loop;
      return Extended_Jacobian;
   end Extend_Jacobian;

end SAL.Math_Float_Manipulator_7;
