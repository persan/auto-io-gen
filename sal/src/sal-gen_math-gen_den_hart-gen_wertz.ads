--  Abstract:
--
--  Operations for types in parent using right multiply quaternions,
--  left multiply matrices.
--
--  References:
--
--  [1] "Introduction to Robotics Mechanics & Control" John J. Craig,
--      Addison-Wesley, 1986
--
--  [2] spacecraft_math.pdf, Stephen Leake
--
--  Copyright (C) 2007 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

generic
package SAL.Gen_Math.Gen_Den_Hart.Gen_Wertz is

   function To_Pose (Param : in Den_Hart_Type; Position : in Real_Type) return Math_DOF_6.Pose_Type;
   --  [2] 5.1.1-2 eqn:den_hart_to_pose_right; return Ti_Pose_Ti-1

   function To_Inverse_Pose (Param : in Den_Hart_Type; Position : in Real_Type) return Math_DOF_6.Pose_Type;
   --  [2] 5.1.1-2 eqn:den_hart_to_pose_right, result inverted; return Ti-1_Pose_Ti

   pragma Inline (To_Pose);
   pragma Inline (To_Inverse_Pose);

end SAL.Gen_Math.Gen_Den_Hart.Gen_Wertz;
