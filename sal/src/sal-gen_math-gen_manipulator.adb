--  Abstract:
--
--  see spec
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

package body SAL.Gen_Math.Gen_Manipulator is

   --  Jacobians

   function "*" (Left : in Jacobian_Type; Right : in Joint_Array_Real_Type) return Math_DOF_6.Dual_Cart_Vector_Type
   is
      use Joint_Array_DCV_Ops;
   begin
      return DC_Array_JAR_Type (Left) * Right;
   end "*";

   function "*" (Left : in Jacobian_Type; Right : in Math_DOF_6.Dual_Cart_Vector_Type) return Joint_Array_Real_Type
   is
      use Joint_Array_DCV_Ops;
   begin
      return DC_Array_JAR_Type (Left) * Right;
   end "*";

   function "*"
     (Left  : in Inverse_Jacobian_Type;
      Right : in Math_DOF_6.Dual_Cart_Vector_Type)
     return Joint_Array_Real_Type
   is
      use Joint_Array_DCV_Ops;
   begin
      return Joint_Array_DCV_Type (Left) * Right;
   end "*";

   function "*"
     (Left  : in Inverse_Jacobian_Type;
      Right : in Joint_Array_Real_Type)
     return Math_DOF_6.Dual_Cart_Vector_Type
   is
      use Joint_Array_DCV_Ops;
   begin
      return Joint_Array_DCV_Type (Left) * Right;
   end "*";

   function Inverse (Right : in Jacobian_Type) return Inverse_Jacobian_Type
   is begin
      return Inverse_Jacobian_Type (Joint_Array_DCV_Ops.Inverse (DC_Array_JAR_Type (Right)));
   end Inverse;

   --  Projectors

   function "*" (Left : in Projector_Type; Right : in Joint_Array_Real_Type) return Joint_Array_Real_Type
   is
      use Joint_Array_JAR_Ops;
   begin
      return Joint_Array_JAR_Type (Left) * Right;
   end "*";

   function Null_Space_Projector
     (Forward : in Jacobian_Type;
      Inverse : in Inverse_Jacobian_Type)
     return Projector_Type
   is
      use Joint_Array_JAR_Ops, Joint_Array_DCV_Ops;
   begin
      return Projector_Type (Joint_Array_JAR_Type'(Identity - Joint_Array_DCV_Type (Inverse) *
                                                     DC_Array_JAR_Type (Forward)));
   end Null_Space_Projector;

end SAL.Gen_Math.Gen_Manipulator;
