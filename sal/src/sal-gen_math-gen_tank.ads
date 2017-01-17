--  Abstract :
--
--  Different ways to compute mass distributions in tanks.
--
--  References :
--
--  [1] spacecraft_math.pdf
--
--  Copyright (C) 2006 - 2007 Stephen Leake.  All Rights Reserved.
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

with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Elementary_Functions;
generic
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real_Type);
   with package Complex_Elementary is new Ada.Numerics.Generic_Complex_Elementary_Functions (Complex_Types);
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
package SAL.Gen_Math.Gen_Tank is
   pragma Pure;

   procedure Compute_Sphere
     (Rho            : in     Real_Type;
      Mass           : in     Real_Type;
      Radius         :    out Real_Type;
      Center_Inertia :    out Real_Type);
   --  Compute mass moments of a sphere containing Mass. The center of
   --  mass is at the center of the sphere.

   procedure Compute_Sphere_Sect
     (Rho           : in     Real_Type;
      Radius        : in     Real_Type;
      Mass          : in     Real_Type;
      Height        :    out Real_Type;
      CM_Sym        :    out Real_Type;
      I_Sym         :    out Real_Type;
      I_Non_Sym_Bot :    out Real_Type);
   --  Compute height and mass moments of spherical section radius
   --  Radius containing Mass, of density Rho.
   --
   --  I_Sym is the moment of inertia about the axis of symmetry.
   --  I_Non_Sym_Bot is the moment of inertia about an axis orthogonal
   --  to the axis of symmetry, centered at the bottom of the tank.
   --  See the function I_Non_Sym_CM below.
   --
   --  The functions are split this way to be easier to test.

   function I_Non_Sym_CM
     (Mass          : in Real_Type;
      CM_Sym        : in Real_Type;
      I_Non_Sym_Bot : in Real_Type)
     return Real_Type;
   --  Given values computed by Compute_Spher_Sect, return the moment
   --  of inertia about an axis orthogonal to the axis of symmetry,
   --  centered at the center of mass.


end SAL.Gen_Math.Gen_Tank;
