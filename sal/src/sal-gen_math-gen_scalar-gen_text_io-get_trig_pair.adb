--  Abstract :
--
--  Get Trig_Pair_Type as a single value in radians.
--
--  Copyright (C) 2003 Stephen Leake.  All Rights Reserved.
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

separate (SAL.Gen_Math.Gen_Scalar.Gen_Text_IO)
procedure Get_Trig_Pair
  (File                        : in     Ada.Text_IO.File_Type;
   Item                        :    out Trig_Pair_Type;
   Named_Association_Record    : in     Boolean := False;
   Named_Association_Component : in     Boolean := False)
is
   pragma Unreferenced (Named_Association_Component);
   Temp_Item : Real_Type;
begin
   Check (File, "(");
   if Named_Association_Record then Check (File, "Radians => "); end if;
   Math_Text_IO.Get (File, Temp_Item);
   Check (File, ")");
   Item := Sin_Cos (Temp_Item);
end Get_Trig_Pair;
