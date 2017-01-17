--  Abstract:
--
--  Operations for parent that use Wertz convention.
--
--  Copyright (C) 2007 Stephen Leake.  All Rights Reserved.
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

pragma License (Modified_GPL);

with SAL.Gen_Math.Gen_Den_Hart.Gen_Wertz;
with SAL.Gen_Math.Gen_DOF_3.Gen_Wertz;
with SAL.Gen_Math.Gen_DOF_6.Gen_Wertz;
generic
   with package Math_DOF_3_Wertz is new Math_DOF_3.Gen_Wertz;
   with package Math_DOF_6_Wertz is new Math_DOF_6.Gen_Wertz (Math_DOF_3_Wertz);
   with package Math_Den_Hart_Wertz is new Math_Den_Hart.Gen_Wertz;
package SAL.Gen_Math.Gen_Manipulator.Gen_Wertz is

   procedure Slow_Mass_Pose
     (Joint           : in     Joint_Array_Real_Type;
      DH_Pose_Base    : in     Math_DOF_6.Pose_Type;
      Den_Hart        : in     Joint_Array_Den_Hart_Type;
      Tool_Pose_Last  : in     Math_DOF_6.Pose_Type;
      Mass_Link       : in     Joint_Array_Mass_Type;
      Tool_Mass_Tool  : in     Math_DOF_6.Mass_Type;
      Tool_Pose_Base  :    out Math_DOF_6.Pose_Type;
      Total_Mass_Base :    out Math_DOF_6.Mass_Type);

end SAL.Gen_Math.Gen_Manipulator.Gen_Wertz;
