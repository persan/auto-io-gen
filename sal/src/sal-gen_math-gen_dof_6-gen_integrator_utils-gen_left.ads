--  Abstract :
--
--  Operations on parent types that require left-multiply rotations.
--
--  Copyright (C) 2002, 2003, 2005, 2006 Stephen Leake.  All Rights Reserved.
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

with SAL.Gen_Math.Gen_DOF_3.Gen_Left;
with SAL.Gen_Math.Gen_DOF_6.Gen_Left;
generic
   with package Math_DOF_3_Left is new Math_DOF_3.Gen_Left;
   with package Math_DOF_6_Left is new SAL.Gen_Math.Gen_DOF_6.Gen_Left (Math_DOF_3_Left);
package SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils.Gen_Left is
   pragma Elaborate_Body;

   --  Rotation in State_Type is active left multiply, following
   --  robotics usage. Thus the name of the pose is Base_Pose_CM.
   --
   --  Rotation in State_Dot_Type is active left multiply; the name
   --  is CM_1_Derivative_CM_2.

   function State_Plus_Derivative (State : in State_Type; Derivative : in State_Dot_Type) return State_Type;
   function "+" (State : in State_Type; Derivative : in State_Dot_Type) return State_Type renames State_Plus_Derivative;
   --  Frames as defined in parent; Derivative should be from
   --  Derivative_Times_Time.

end SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils.Gen_Left;
