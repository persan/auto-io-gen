--  Abstract:
--
--  Types and operations common to all 7-joint manipulators.
--
--  Copyright (C) 2005, 2007 Stephen Leake.  All Rights Reserved.
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

with SAL.Gen_Math.Gen_Manipulator;
with SAL.Math_Float.Den_Hart;
with SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_6.DC_Array_DCV_Inverse;
with SAL.Math_Float.Elementary;
with SAL.Math_Float.Scalar;
package SAL.Math_Float_Manipulator_7 is
   --  Not SAL.Math_Float . Manipulator_7 because this is not a
   --  generic instantiation.

   type Joint_Index_Type is range 1 .. 7;
   type Joint_Array_Real_Type     is array (Joint_Index_Type) of SAL.Math_Float.Real_Type;
   type Joint_Array_Pose_Type     is array (Joint_Index_Type) of SAL.Math_Float.DOF_6.Pose_Type;
   type Joint_Array_Mass_Type     is array (Joint_Index_Type) of SAL.Math_Float.DOF_6.Mass_Type;
   type Joint_Array_Den_Hart_Type is array (Joint_Index_Type) of SAL.Math_Float.Den_Hart.Den_Hart_Type;

   package Math is new SAL.Math_Float.Gen_Manipulator
     (Elementary                      => SAL.Math_Float.Elementary,
      Math_Scalar                     => SAL.Math_Float.Scalar,
      Math_Dof_3                      => SAL.Math_Float.DOF_3,
      Math_DOF_6                      => SAL.Math_Float.DOF_6,
      Math_Den_Hart                   => SAL.Math_Float.Den_Hart,
      Math_DOF_6_DC_Array_DCV_Inverse => SAL.Math_Float.DOF_6.DC_Array_DCV_Inverse,
      Joint_Index_Type                => Joint_Index_Type,
      Joint_Array_Real_Type           => Joint_Array_Real_Type,
      Joint_Array_Pose_Type           => Joint_Array_Pose_Type,
      Joint_Array_Mass_Type           => Joint_Array_Mass_Type,
      Joint_Array_Den_Hart_Type       => Joint_Array_Den_Hart_Type);

   function "&"
     (Left  : in SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
      Right : in SAL.Math_Float.Real_Type)
     return Joint_Array_Real_Type;
   --  Concatenate a Cartesian vector with a redundant scalar to form
   --  an extended vector.

   function Cart (Right : in Joint_Array_Real_Type) return SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
   --  Extract the Cartesian part of an extended vector.

   function Extend_Jacobian
     (Jacobian              : in Math.Jacobian_Type;
      Redundant_Joint_Index : in Joint_Index_Type   := 1)
     return Math.Joint_Array_JAR_Type;
   --  Extend a 6x7 Jacobian into a 7x7 Jacobian by declaring the
   --  redundant joint index to be the 7th degree of freedom.

end SAL.Math_Float_Manipulator_7;
