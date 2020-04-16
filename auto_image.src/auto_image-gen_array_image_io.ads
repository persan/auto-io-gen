--  Abstract :
--
--  Generic Text Output packages for various types of arrays, using Ada
--  positional or named association aggregate syntax, based on
--  Ada.Text_IO.
--
--  Design :
--
--  Each package declares a Put_Item that matches
--  Private_1D.Put_Element, for composing into higher-dimension
--  arrays. It uses the package defaults for the missing parameters.
--  It is not named Put, to avoid ambiguity with the other Put with
--  defaulted parameters. If Named_Association is True, it outputs
--  named association for components; otherwise it outputs positional
--  association. If Single_Line is False, each element is output on a
--  separate line.
--
--  Similarly for Get; the Get_Item routine passes a Width of 0 to
--  lower level Get routines. Either all names must be present in the
--  input (if Named_Association is True), or no names are allowed (if
--  Named_Association is False), unlike standard Ada aggregate syntax.
--  In addition, names must be in declaration order. Any Get routine
--  may propagate End_Error or Auto_Io_Gen_Utils.Syntax_Error.
--
--  Based on similar package by Stephen Leake.
--  Copyright (C) 2020       Oliver Kellogg  <okellogg@users.sf.net>
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
--

with Ada.Strings.Unbounded;

package Auto_Image.Gen_Array_Image_IO is
   pragma Elaborate_Body; -- Ada.Text_IO is

   --  Integer_1D
   generic
      type Element_Type is range <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Integer_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Index_Array_Element_Type);

   end Integer_1D;

   --  Unconstrained_Integer_1D
   generic
      type Element_Type is range <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type range <>) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Unconstrained_Integer_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Index_Array_Element_Type);

   end Unconstrained_Integer_1D;

   --  Modular_1D
   generic
      type Element_Type is mod <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Modular_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type);

   end Modular_1D;

   --  Unconstrained_Modular_1D
   generic
      type Element_Type is mod <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type range <>) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Unconstrained_Modular_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Index_Array_Element_Type);

   end Unconstrained_Modular_1D;

   --  Enumeration_1D array
   generic
      type Element_Type is (<>);
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Enumeration_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Index_Array_Element_Type);

   end Enumeration_1D;

   --  Unconstrained_Enumeration_1D
   generic
      type Element_Type is (<>);
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type range <>) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Unconstrained_Enumeration_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Index_Array_Element_Type);

   end Unconstrained_Enumeration_1D;

   --  Float_1D
   generic
      type Element_Type is digits <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Float_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Index_Array_Element_Type);

   end Float_1D;

   --  Unconstrained_Float_1D
   generic
      type Element_Type is digits <>;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type range <>) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Unconstrained_Float_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Index_Array_Element_Type);

   end Unconstrained_Float_1D;

   --  Private_1D
   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Private_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Index_Array_Element_Type);

   end Private_1D;

   --  Unconstrained_Private_1D
   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Index_Array_Element_Type is array (Index_Type range <>) of Element_Type;
      with procedure Element_Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Element_Type);

   package Unconstrained_Private_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in     Index_Array_Element_Type);

   end Unconstrained_Private_1D;

end Auto_Image.Gen_Array_Image_IO;
