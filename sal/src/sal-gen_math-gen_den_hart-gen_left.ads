--  Abstract:
--
--  Operations on types in parent using left multiply rotation operations
--
--  References:
--
--  [1] "Introduction to Robotics Mechanics & Control" John J. Craig,
--      Addison-Wesley, 1986
--
--  [2] spacecraft_math.pdf, Stephen Leake
--
--  Copyright (C) 2001 - 2003, 2005 - 2007 Stephen Leake.  All Rights Reserved.
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
--
--  Modification History:
--     (kept for sentimental reasons; see CVS for more recent history)
--
--  Jan. 17, 1991 Dana Miller
--     Created
--  27 Jan 1992   Stephe Leake
--     change Den_Hart to To_Pose, add To_Propagator
--  15 June 1992        Stephe Leake
--     change DEN_HART_PARAMS to DEN_HARTS. Add
--     To_Inverse_Wrench_Propagators, To_Inverse_Velocity_Propagators. Make
--     all conversion subprogram names plural.
--   8 July 1992        Stephe Leake
--     match changes to Math_6_DOF spec
--  30 Nov 1993 Stephe Leake
--     convert to _TYPE naming convention, match changes to Math_Scalar,
--  18 Jul 1994     Victoria Buckland
--     added both Mult functions
--  11 Oct 1994 Stephe Leake
--     add ANALOG_SERVO_TYPE
--  21 May 2002
--     changed to SAL.Gen_Math.Gen_Den_Hart.
with SAL.Gen_Math.Gen_DOF_3.Gen_Left;
generic
   --  Auto_Io_Gen : ignore
   with package Math_DOF_3_Left is new Math_DOF_3.Gen_Left;
package SAL.Gen_Math.Gen_Den_Hart.Gen_Left is

   function To_Pose (Param : in Den_Hart_Type; Position : in Real_Type) return Math_DOF_6.Pose_Type;
   --  [2] 5.1.1-1 eqn:den_hart_to_pose_left; return Ti-1_Pose_Ti

   function To_Inverse_Pose (Param : in Den_Hart_Type; Position : in Real_Type) return Math_DOF_6.Pose_Type;
   --  [2] 5.1.1-2 eqn:den_hart_to_pose_right, result inverted; return Ti_Pose_Ti-1

   pragma Inline (To_Pose);
   pragma Inline (To_Inverse_Pose);

   function Partial_Jacobian (Ti_T_Obj : Math_DOF_6.Pose_Type) return Math_DOF_6.Dual_Cart_Vector_Type;
   --  Ti_T_Obj is the transform from a manipulator joint i to the
   --  object, expressed in the joint i frame; result is the column
   --  of the manipulator Jacobian corresponding to joint i. The
   --  Jacobian is in the object frame.

   function Mult
     (Left           : in Math_DOF_6.Pose_Type;
      Right          : in Den_Hart_Type;
      Right_Position : in Real_Type)
     return Math_DOF_6.Pose_Type;
   --  Equivalent to Left * To_Pose(Right, Right_Position) but faster

   function Mult
     (Left          : in Den_Hart_Type;
      Left_Position : in Real_Type;
      Right         : in Math_DOF_6.Pose_Type)
     return Math_DOF_6.Pose_Type;
   --  Equivalent to To_Pose(Left, Left_Position) * Right but faster

end SAL.Gen_Math.Gen_Den_Hart.Gen_Left;
