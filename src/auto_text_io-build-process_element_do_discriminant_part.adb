--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2001 - 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Asis.Declarations;
with Asis.Elements;
with Auto_Text_IO.Build.Process_Element_Utils; use Auto_Text_IO.Build.Process_Element_Utils;
procedure Auto_Text_IO.Build.Process_Element_Do_Discriminant_Part
   (Element : in     Asis.Element;
    Control : in out Asis.Traverse_Control;
    State   : in out State_Type)
is
   use Asis;
begin
   case Elements.Element_Kind (Element) is
   when A_Declaration =>
      case Elements.Declaration_Kind (Element) is
      when A_Discriminant_Specification =>

         Debug_Put (Element, Processing);

         State.Private_State.Current_Type.Record_Constrained :=
            Not_An_Element = Elements.Element_Kind
            (Declarations.Initialization_Expression (Element));

         --  The children of discriminants include the discriminant
         --  name and type name, which we want.
         Control                   := Continue;
         State.Private_State.Label := In_Discriminant;

      when others =>
         Debug_Put (Element, Skipping);
         --  The current type has not yet been added to
         --  State.Type_List, so we don't need to delete it.
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;
      end case;

   when A_Definition =>
      case Elements.Definition_Kind (Element) is
      when A_Private_Type_Definition |
         A_Private_Extension_Definition |
         A_Tagged_Private_Type_Definition =>

         Debug_Put (Element, Processing);

         --  We created a Record_Label descriptor when we processed
         --  the discriminants; now it turns out this is a private
         --  type, so put it on the private type tree.
         Lists.Type_Iterator_Trees.Add
            (State.Private_State.Private_Type_Tree,
             Lists.Type_Descriptor_Lists.Last (State.Type_List));

         --  There are no children or siblings of this element; we'll
         --  get the components of the record when we process the full
         --  definition.
         Control                   := Abandon_Siblings;
         State.Private_State.Label := In_Package;

      when A_Type_Definition =>
         case Elements.Type_Kind (Element) is
         when A_Record_Type_Definition =>

            Debug_Put (Element, Processing);

            --  The children of this element are the components of the
            --  record, which we want.
            Control                   := Continue;
            State.Private_State.Label := In_Record_Type;

         when others =>
            Debug_Put (Element, Skipping);
            Report_Unsupported (State, Element);
            Control := Abandon_Siblings;

         end case;

      when others =>
         Debug_Put (Element, Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;

      end case;

   when others =>
      Debug_Put (Element, Skipping);
      Report_Unsupported (State, Element);
      Control := Abandon_Siblings;

   end case;

end Auto_Text_IO.Build.Process_Element_Do_Discriminant_Part;
