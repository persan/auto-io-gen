--  Abstract :
--
--  Do processing for In_Type state in Build.Process_Element.
--
--  Copyright (C) 2001 Stephen Leake.  All Rights Reserved.
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

private procedure Auto_Text_IO.Build.Process_Element_Do_Type
   (Element : in     Asis.Element;
    Control : in out Asis.Traverse_Control;
    State   : in out State_Type);
