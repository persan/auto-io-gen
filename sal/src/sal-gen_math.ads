--  Abstract:
--
--  Root of generic Math library.
--
--  Design:
--
--  Generic to allow instantiating with Float or Double.
--
--  Copyright (C) 2001, 2009 Stephen Leake.  All Rights Reserved.
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
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

generic
   type Num_Type is digits <>;
package SAL.Gen_Math is
   pragma Pure;

   --  GNAT doesn't do overflow checks for unconstrained types; they
   --  can have the value IEEE Infinity. This confuses some algorithms
   --  in SAL. Constrained types are checked for overflow, so make
   --  sure the type we use is constrained.
   subtype Real_Type is Num_Type range Num_Type'First .. Num_Type'Last;

end SAL.Gen_Math;
