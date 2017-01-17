--  Abstract :
--
--  Don't put redundant Inertia. Otherwise copied from Auto_Io_Gen
--  generated body.
--
--  Copyright (C) 2003, 2004 Stephen Leake.  All Rights Reserved.
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
separate (SAL.Gen_Math.Gen_DOF_6.Gen_Text_IO)
procedure Put_Mass
  (File                        : in Ada.Text_IO.File_Type;
   Item                        : in Mass_Type;
   Single_Line_Record          : in Boolean := True;
   Named_Association_Record    : in Boolean := False;
   Single_Line_Component       : in Boolean := True;
   Named_Association_Component : in Boolean := False)
--  Inertia member is redundant and will not be reported.

is begin
   Put (File, "(");
   if Named_Association_Record then
      Put (File, "Total => ");
      if not Single_Line_Record then New_Line (File); end if;
   end if;
   Math_Text_IO.Put (File, Item.Total);
   Put (File, ','); if not Single_Line_Record then New_Line (File); end if;
   Put (File, ' ');
   if Named_Association_Record then
      Put (File, "Center => ");
      if not Single_Line_Record then New_Line (File); end if;
   end if;
   Math_DOF_3_Text_IO.Put_Item
     (File, Item.Center, Single_Line => Single_Line_Component, Named_Association => Named_Association_Component);
   Put (File, ','); if not Single_Line_Record then New_Line (File); end if;
   Put (File, ' ');
   if Named_Association_Record then
      Put (File, "Center_Inertia => ");
      if not Single_Line_Record then New_Line (File); end if;
   end if;
   Math_DOF_3_Text_IO.Put_Item
     (File, Item.Center_Inertia,
      Single_Line => Single_Line_Component, Named_Association => Named_Association_Component);
   Put (File, ")");
end Put_Mass;
