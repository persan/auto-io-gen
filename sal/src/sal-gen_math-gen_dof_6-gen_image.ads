--  Abstract :
--
--  Image functions for types in DOF_6.
--
--  Copyright (C) 2002, 2003, 2006 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--

with SAL.Gen_Array_Image;
with SAL.Gen_Math.Gen_DOF_3.Gen_Image;
generic
   with package Math_DOF_3_Image is new Math_DOF_3.Gen_Image;
package SAL.Gen_Math.Gen_DOF_6.Gen_Image is
   pragma Elaborate_Body; -- parent is

   function Dual_Cart_Vector_Image (Item : in Dual_Cart_Vector_Type) return String;
   function Image (Item : in Dual_Cart_Vector_Type) return String
     renames Dual_Cart_Vector_Image;

   function Image is new SAL.Gen_Array_Image
     (Index_Type               => Dual_Cart_Axis_Type,
      Element_Type             => Dual_Cart_Vector_Type,
      Index_Array_Element_Type => DC_Array_DCV_Type,
      Image                    => Dual_Cart_Vector_Image);

   function Image is new SAL.Gen_Array_Image
     (Index_Type               => Dual_Axis_Type,
      Element_Type             => Real_Type,
      Index_Array_Element_Type => Dual_Real_Type,
      Image                    => Real_Type'Image);

   function Image (Item : in Pose_Type) return String;

   function Image (Item : in CM_Mass_Type) return String;

end SAL.Gen_Math.Gen_DOF_6.Gen_Image;
