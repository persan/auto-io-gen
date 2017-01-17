--  Abstract :
--
--  Config file operations for parent
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

with SAL.Gen_Math.Gen_Config;
with SAL.Config_Files;
generic
   with package Math_Config is new SAL.Gen_Math.Gen_Config;
package SAL.Gen_Math.Gen_DOF_3.Gen_Config is
   pragma Elaborate_Body; --  Ada 2005: SAL.Config_Files

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Leaf        : in String;
      Default     : in Cart_Vector_Type                    := Zero_Cart_Vector;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Cart_Vector_Type;

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Iterator    : in SAL.Config_Files.Iterator_Type;
      Leaf        : in String;
      Default     : in Cart_Vector_Type                    := Zero_Cart_Vector;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Cart_Vector_Type;

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Leaf        : in String;
      Default     : in Unit_Quaternion_Type                := Zero_Unit_Quaternion;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Unit_Quaternion_Type;

   function Read
     (Config      : in SAL.Config_Files.Configuration_Type;
      Iterator    : in SAL.Config_Files.Iterator_Type;
      Leaf        : in String;
      Default     : in Unit_Quaternion_Type                := Zero_Unit_Quaternion;
      Missing_Key : in SAL.Config_Files.Missing_Key_Type   := SAL.Config_Files.Raise_Exception)
     return Unit_Quaternion_Type;

end SAL.Gen_Math.Gen_DOF_3.Gen_Config;
