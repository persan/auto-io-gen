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

package body SAL.Gen_Math.Gen_DOF_3.Gen_Config is

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Leaf        : in String;
      Default     : in Cart_Vector_Type                    := Zero_Cart_Vector;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Cart_Vector_Type
   is
      use SAL.Config_Files;
      use Math_Config;
      Result : Cart_Vector_Type := Default;
   begin
      if Is_Present (Config, Leaf & ".X") then
         Result (X) := Read (Config, Leaf & ".X", Default (X), Missing_Key);
      end if;

      if Is_Present (Config, Leaf & ".Y") then
         Result (Y) := Read (Config, Leaf & ".Y", Default (Y), Missing_Key);
      end if;

      if Is_Present (Config, Leaf & ".Z") then
         Result (Z) := Read (Config, Leaf & ".Z", Default (Z), Missing_Key);
      end if;

      return Result;

   end Read;

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Iterator    : in SAL.Config_Files.Iterator_Type;
      Leaf        : in String;
      Default     : in Cart_Vector_Type                    := Zero_Cart_Vector;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Cart_Vector_Type
   is
      use SAL.Config_Files;
      use Math_Config;
      Result : Cart_Vector_Type := Default;
   begin
      if Is_Present (Config, Iterator, Leaf & ".X") then
         Result (X) := Read (Config, Iterator, Leaf & ".X", Default (X), Missing_Key);
      end if;

      if Is_Present (Config, Iterator, Leaf & ".Y") then
         Result (Y) := Read (Config, Iterator, Leaf & ".Y", Default (Y), Missing_Key);
      end if;

      if Is_Present (Config, Iterator, Leaf & ".Z") then
         Result (Z) := Read (Config, Iterator, Leaf & ".Z", Default (Z), Missing_Key);
      end if;

      return Result;

   end Read;

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Leaf        : in String;
      Default     : in Unit_Quaternion_Type                := Zero_Unit_Quaternion;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Unit_Quaternion_Type
   is
      use SAL.Config_Files;
      use Math_Config;
      Result : Unit_Quaternion_Type := Default;
   begin
      if Is_Present (Config, Leaf & ".X") then
         Result.X := Read (Config, Leaf & ".X", Default.X, Missing_Key);
      end if;

      if Is_Present (Config, Leaf & ".Y") then
         Result.Y := Read (Config, Leaf & ".Y", Default.Y, Missing_Key);
      end if;

      if Is_Present (Config, Leaf & ".Z") then
         Result.Z := Read (Config, Leaf & ".Z", Default.Z, Missing_Key);
      end if;

      if Is_Present (Config, Leaf & ".S") then
         Result.S := Read (Config, Leaf & ".S", Default.S, Missing_Key);
      end if;

      return To_Unit_Quaternion (X => Result.X, Y => Result.Y, Z => Result.Z, S => Result.S);

   end Read;

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Iterator    : in SAL.Config_Files.Iterator_Type;
      Leaf        : in String;
      Default     : in Unit_Quaternion_Type                := Zero_Unit_Quaternion;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Unit_Quaternion_Type
   is
      use SAL.Config_Files;
      use Math_Config;
      Result : Unit_Quaternion_Type := Default;
   begin
      if Is_Present (Config, Iterator, Leaf & ".X") then
         Result.X := Read (Config, Iterator, Leaf & ".X", Default.X, Missing_Key);
      end if;

      if Is_Present (Config, Iterator, Leaf & ".Y") then
         Result.Y := Read (Config, Iterator, Leaf & ".Y", Default.Y, Missing_Key);
      end if;

      if Is_Present (Config, Iterator, Leaf & ".Z") then
         Result.Z := Read (Config, Iterator, Leaf & ".Z", Default.Z, Missing_Key);
      end if;

      if Is_Present (Config, Iterator, Leaf & ".S") then
         Result.S := Read (Config, Iterator, Leaf & ".S", Default.S, Missing_Key);
      end if;

      return To_Unit_Quaternion (X => Result.X, Y => Result.Y, Z => Result.Z, S => Result.S);

   end Read;

end SAL.Gen_Math.Gen_DOF_3.Gen_Config;
