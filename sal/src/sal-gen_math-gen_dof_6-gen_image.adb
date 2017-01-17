--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 Stephen Leake.  All Rights Reserved.
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
package body SAL.Gen_Math.Gen_DOF_6.Gen_Image is

   function Dual_Cart_Vector_Image (Item : in Dual_Cart_Vector_Type) return String
   is begin
      return "(" &
        Real_Type'Image (Item (TX)) &
        ", " &
        Real_Type'Image (Item (TY)) &
        ", " &
        Real_Type'Image (Item (TZ)) &
        ", " &
        Real_Type'Image (Item (RX)) &
        ", " &
        Real_Type'Image (Item (RY)) &
        ", " &
        Real_Type'Image (Item (RZ)) &
        ")";

   end Dual_Cart_Vector_Image;

   function Image (Item : in Pose_Type) return String
   is begin
      return "(" &
        Math_DOF_3_Image.Image (Item.Translation) &
        ", " &
        Math_DOF_3_Image.Image (Item.Rotation) &
        ")";
   end Image;

   function Image (Item : in CM_Mass_Type) return String
   is begin
      return "(" &
        Real_Type'Image (Item.Total) &
        ", " &
        Math_DOF_3_Image.Image (Item.Center_Inertia) &
        ")";
   end Image;

end SAL.Gen_Math.Gen_DOF_6.Gen_Image;

