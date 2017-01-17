--  Abstract :
--
--  Image functions for types in Ada.Streams
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

with SAL.Gen_Unconstrained_Array_Image;
with SAL.Generic_Binary_Image;
with SAL.Generic_Hex_Image;
package SAL.Streams_More.Images is
   pragma Preelaborate; -- SAL.Generic_Hex_Image

   function Hex_Image is new SAL.Generic_Hex_Image (2, Ada.Streams.Stream_Element);

   function Hex_Image (Item : in Ada.Streams.Stream_Element_Array) return String;

   function Binary_Image is new SAL.Generic_Binary_Image (2, Ada.Streams.Stream_Element);

   function Binary_Image is new SAL.Gen_Unconstrained_Array_Image
     (Index_Type               => Ada.Streams.Stream_Element_Offset,
      Element_Type             => Ada.Streams.Stream_Element,
      Index_Array_Element_Type => Ada.Streams.Stream_Element_Array,
      Image                    => Binary_Image);

end SAL.Streams_More.Images;
