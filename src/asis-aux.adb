--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2001, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Asis.Declarations;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Expressions;
package body Asis.Aux is
   --  private subprograms

   function Selected_Component_Image (Element : in Asis.Element) return String
   is
      use Asis.Elements, Asis.Expressions, Ada.Characters.Handling;
   begin
      case Element_Kind (Element) is
      when An_Expression =>
         case Expression_Kind (Element) is
         when An_Identifier | An_Operator_Symbol | A_Character_Literal | An_Enumeration_Literal =>
            return To_String (Name_Image (Element));

         when A_Selected_Component =>
            return Selected_Component_Image (Prefix (Element)) & "." & To_String (Name_Image (Selector (Element)));

         when others =>
            raise Asis.Exceptions.ASIS_Failed;

         end case;
      when others =>
         raise Asis.Exceptions.ASIS_Failed;

      end case;
   end Selected_Component_Image;

   ---------
   --  Public subprograms

   function Name (El : in Asis.Element) return String is
      use Asis.Elements, Ada.Characters.Handling;
   begin
      case Element_Kind (El) is
      when A_Defining_Name =>
         return To_String (Asis.Declarations.Defining_Name_Image (El));

      when A_Declaration =>
         return To_String (Asis.Declarations.Defining_Name_Image (Asis.Declarations.Names (El) (1)));

      when An_Expression =>
         case Expression_Kind (El) is
         when An_Identifier  | An_Operator_Symbol | A_Character_Literal | An_Enumeration_Literal =>
            return To_String (Asis.Expressions.Name_Image (El));

         when A_Selected_Component =>
            return Selected_Component_Image (El);

         when others =>
            raise Program_Error with  "Attempt to take ASIS Name of " & Image (El);

         end case;

      when others =>
            raise Program_Error with  "Attempt to take ASIS Name of " & Image (El);
      end case;
   end Name;

   function Image (El : in Asis.Element) return String
   is
      use Asis.Elements, Ada.Characters.Handling;
      Element_Kind_Image : constant String := Element_Kinds'Image (Element_Kind (El));
   begin
      case Element_Kind (El) is
      when A_Defining_Name =>
         return Element_Kind_Image & " " & Defining_Name_Kinds'Image (Defining_Name_Kind (El)) & " " & Name (El);

      when A_Declaration =>
         return Element_Kind_Image & " " & Declaration_Kinds'Image (Declaration_Kind (El)) & " " & Name (El);

      when A_Definition =>
         declare
            Definition_Kind_Image : constant String := Element_Kind_Image & " " &
               Definition_Kinds'Image (Definition_Kind (El));
         begin
            case Definition_Kind (El) is
            when A_Type_Definition =>
               return Definition_Kind_Image & " " & Type_Kinds'Image (Type_Kind (El));

            when others =>
               return Definition_Kind_Image;
            end case;
         end;

      when An_Expression =>
         declare
            Expression_Kind_Image : constant String := Element_Kind_Image & " " &
               Expression_Kinds'Image (Expression_Kind (El));
         begin
            case Expression_Kind (El) is
            when An_Identifier =>
               return Expression_Kind_Image & " " & Name (El);

            when A_Selected_Component =>
               return Expression_Kind_Image & " " & Selected_Component_Image (El);

            when others =>
               return Expression_Kind_Image;

            end case;
         end;

      when others =>
         return Element_Kind_Image;

      end case;

   end Image;

end Asis.Aux;
