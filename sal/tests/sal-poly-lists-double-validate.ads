--  Abstract :
--
--  Check all link constraints in a list.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.

generic
package SAL.Poly.Lists.Double.Validate is
   pragma Elaborate_Body; -- Ada.Exceptions.

   Validation_Error : exception;

   procedure Validate (List : in List_Type);

end SAL.Poly.Lists.Double.Validate;
