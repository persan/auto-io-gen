--  Abstract :
--
--  See spec
--
--  Copyright (C) 2005 - 2006 Stephen Leake.  All Rights Reserved.
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

package body SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils.Gen_AUnit is

   procedure Check
     (Label     : in String;
      Computed  : in State_Type;
      Expected  : in State_Type;
      Tolerance : in Real_Type := Math_AUnit.Default_Tolerance)
   is begin
      Math_DOF_3_AUnit.Check (Label   & ".Translation", Computed.Translation, Expected.Translation, Tolerance);
      Math_DOF_3_AUnit.Check (Label   & ".Rotation", Computed.Rotation, Expected.Rotation, Tolerance);
      Math_DOF_3_AUnit.Check (Label   & ".CM_Tran_Mom", Computed.CM_Tran_Mom, Expected.CM_Tran_Mom, Tolerance);
      Math_DOF_3_AUnit.Check (Label   & ".CM_Rot_Mom", Computed.CM_Rot_Mom, Expected.CM_Rot_Mom, Tolerance);
   end Check;

   procedure Check
     (Label     : in String;
      Computed  : in State_Dot_Type;
      Expected  : in State_Dot_Type;
      Tolerance : in Real_Type := Math_AUnit.Default_Tolerance)
   is begin
      Math_DOF_3_AUnit.Check (Label   & ".Tran_Dot", Computed.Tran_Dot, Expected.Tran_Dot, Tolerance);
      Math_DOF_3_AUnit.Check (Label   & ".Rot_Dot", Computed.Rot_Dot, Expected.Rot_Dot, Tolerance);
      Math_DOF_3_AUnit.Check (Label   & ".Tran_Mom_Dot", Computed.CM_Tran_Mom_Dot, Expected.CM_Tran_Mom_Dot, Tolerance);
      Math_DOF_3_AUnit.Check (Label   & ".Rot_Mom_Dot", Computed.CM_Rot_Mom_Dot, Expected.CM_Rot_Mom_Dot, Tolerance);
   end Check;

end SAL.Gen_Math.Gen_DOF_6.Gen_Integrator_Utils.Gen_AUnit;
