--  Abstract :
--
--  Like the name says
--
--  Copyright (C) 1999, 2000 Stephen Leake.  All Rights Reserved.
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

with SAL.Memory_Streams.Bounded.Test;
procedure Test_Memory_Streams_Bounded
is begin
   SAL.Memory_Streams.Bounded.Test.Run_Test;
end Test_Memory_Streams_Bounded;
