--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 Stephen Leake.  All Rights Reserved.
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

pragma License (Modified_GPL);

package body SAL.Gen_Math.Gen_DOF_6.Gen_Config is

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Leaf        : in String;
      Default     : in Pose_Type                           := Zero_Pose;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
      return Pose_Type
   is
      use SAL.Config_Files;
      use Math_DOF_3_Config;
      Result : Pose_Type := Default;
   begin
      if Is_Present (Config, Leaf & ".Tran") then
         Result.Translation := Read (Config, Leaf & ".Tran", Default.Translation, Missing_Key);
      end if;

      if Is_Present (Config, Leaf & ".Quat") then
         Result.Rotation := Read (Config, Leaf & ".Quat", Default.Rotation, Missing_Key);
      end if;

      return Result;

   end Read;

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Iterator    : in SAL.Config_Files.Iterator_Type;
      Leaf        : in String;
      Default     : in Pose_Type                           := Zero_Pose;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
      return Pose_Type
   is
      use SAL.Config_Files;
      use Math_DOF_3_Config;
      Result : Pose_Type := Default;
   begin
      if Is_Present (Config, Iterator, Leaf & ".Tran") then
         Result.Translation := Read (Config, Iterator, Leaf & ".Tran", Default.Translation, Missing_Key);
      end if;

      if Is_Present (Config, Iterator, Leaf & ".Quat") then
         Result.Rotation := Read (Config, Iterator, Leaf & ".Quat", Default.Rotation, Missing_Key);
      end if;

      return Result;

   end Read;

end SAL.Gen_Math.Gen_DOF_6.Gen_Config;
