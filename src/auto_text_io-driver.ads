--  Abstract :
--
--  Main driver procedure for Auto_Text_IO
--
--  Copyright (C) 2001 Stephen Leake.  All Rights Reserved.
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

procedure Auto_Text_IO.Driver;
--  This is the top-level Auto_Text_IO driver. It reads the
--  command-line arguments, checks the relations between options and
--  existing files and creates the Text_IO child package if possible
--  and necessary.

