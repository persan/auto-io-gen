--  Abstract :
--
--  Test instantation in sal.math_double.dof_6.network_order
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with SAL.Math_Double.DOF_6.Network_Order;
with Ada.Streams; use Ada.Streams;
procedure Debug_Math_Double_DOF_6_Network_Order
is
   use SAL.Math_Double.DOF_6.Network_Order;

   Buffer      : Stream_Element_Array (1 .. 100);
   Buffer_Last : Stream_Element_Count := 0;

begin
   To_Network (SAL.Math_Double.DOF_6.Zero_Pose, Buffer, Buffer_Last);
end Debug_Math_Double_DOF_6_Network_Order;
