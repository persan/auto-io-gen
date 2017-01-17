--  Abstract:
--
--  see spec
--
--  References:
--
--  [1] "Introduction to Robotics Mechanics & Control" John J. Craig,
--      Addison-Wesley, 1986
--

with SAL.Math_Float.Den_Hart; use SAL.Math_Float.Den_Hart;
with SAL.Math_Float.Scalar;   use SAL.Math_Float.Scalar;
package body SAL.Math_Float_Kraft_HC_Nominal is

   --  The joint angle origins are defined such that the nominal
   --  position (0, -PI/2, PI/2, 0, -PI/2, 0) puts the upper arm
   --  normal to the base mounting plate, the fore arm parallel with
   --  the base mounting plate, and the three wrist joints near the
   --  middle of their ranges. Joints are numbered from the base,
   --  starting with joint 1. The base frame is at the intersection of
   --  the first two joints, with +Z towards the base mounting plate
   --  and -Y along the second joint axis (with all joints at zero).
   --  The final frame is in the center of the grip handle, called the
   --  handle frame. In the nominal position, the handle frame has +X
   --  parallel to the upper arm pointing away from the base plate, +Z
   --  parallel to the fore arm, away from the base plate.

   function Geometry return SAL.Math_Float_Manipulator_6.Joint_Array_Den_Hart_Type
   is begin
      return
        (1 => (REVOLUTE, 0.0, Sin_Cos (Pi), 0.0),
         2 => (REVOLUTE, 0.0, Sin_Cos (-Pi/2.0), 0.075),
         3 => (REVOLUTE, 0.180, Sin_Cos (0.0), 0.055),
         4 => (REVOLUTE, 0.205, Sin_Cos (-Pi/2.0), 0.085),
         5 => (REVOLUTE, 0.0, Sin_Cos (Pi / 2.0), 0.005),
         6 => (REVOLUTE, 0.0, Sin_Cos (-Pi/2.0), 0.0));
   end Geometry;

end SAL.Math_Float_Kraft_HC_Nominal;
