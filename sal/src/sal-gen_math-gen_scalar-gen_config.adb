--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2005 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions;
package body SAL.Gen_Math.Gen_Scalar.Gen_Config is

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Iterator    : in SAL.Config_Files.Iterator_Type;
      Leaf        : in String;
      Default     : in Scale_Limit_Type;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
      return Scale_Limit_Type
   is
      Scale  : constant Real_Type := Math_Config.Read (Config, Iterator, Leaf & ".Scale", Default.Scale, Missing_Key);
      Offset : constant Real_Type := Math_Config.Read (Config, Iterator, Leaf & ".Offset", Default.Offset, Missing_Key);

      Limit : constant Limit_Type := Compute_Limit (Scale, Offset, Default);
      High  : constant Real_Type  := Math_Config.Read (Config, Iterator, Leaf & ".High", Limit.High, Missing_Key);
      Low   : constant Real_Type  := Math_Config.Read (Config, Iterator, Leaf & ".Low", Limit.Low, Missing_Key);
   begin
      return
        (Scale  => Scale,
         Offset => Offset,
         Limit  => To_Limit (High => High, Low => Low));
   exception
   when Invalid_Limit =>
      Ada.Exceptions.Raise_Exception
        (SAL.Config_File_Error'Identity,
         SAL.Config_Files.File_Line_Column (Config, Iterator) & "invalid limits");
   end Read;

end SAL.Gen_Math.Gen_Scalar.Gen_Config;
