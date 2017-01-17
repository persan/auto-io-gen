--  Abstract :
--
--  Generate Put procedure body for one type.
--
--  Copyright (C) 2001, 2003 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Text_IO;
with Auto_Text_IO.Lists;
private package Auto_Text_IO.Generate.Put_Body is
   pragma Elaborate_Body; --  Ada.Text_IO, Asis

   procedure Generate
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Text_IO.Lists.Type_Descriptor_Type);
   --  Generate body code for a Put procedure for one type.

end Auto_Text_IO.Generate.Put_Body;
