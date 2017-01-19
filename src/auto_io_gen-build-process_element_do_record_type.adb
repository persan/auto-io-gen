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

with Asis.Definitions;
with Asis.Elements;
with Auto_Io_Gen.Build.Process_Element_Utils; use Auto_Io_Gen.Build.Process_Element_Utils;
procedure Auto_Io_Gen.Build.Process_Element_Do_Record_Type
   (Element : in     Asis.Element;
    Control : in out Asis.Traverse_Control;
    State   : in out State_Type)
is
   use Asis;
begin
   Debug.Put_Line ("Element =>" & Elements.Element_Kind (Element)'Img & ", State => " & State.Private_State.Label'Img);
   case Elements.Element_Kind (Element) is
   when A_Declaration =>
      case Elements.Declaration_Kind (Element) is
      when A_Component_Declaration =>

         Debug.Put_line ( Processing);

         --  The children of components include the component
         --  name and type name, which we want.
         Control                   := Continue;
         State.Private_State.Label := In_Component;

      when others =>
         Debug.Put_line ( Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;
      end case;

   when A_Definition =>
      case Elements.Definition_Kind (Element) is
      when A_Record_Definition =>

         Debug.Put_line ( Processing);

         --  The children of this element are the components of
         --  the record, which we want.
         Control := Continue;

      when A_Subtype_Indication =>

         Debug.Put_line ( Skipping);

         --  This is a discriminant association for a derived type.
         --  The children of this element are the discriminants, which
         --  we don't need. The siblings are the record components,
         --  which we do need.
         Control := Abandon_Children;

      when A_Variant_Part =>
         Debug.Put_line ( Processing);

         State.Private_State.Current_Type.Record_Variant_Part.Discriminant :=
           Definitions.Discriminant_Direct_Name (Element);

         --  The children of this element include the discriminant
         --  expression, which we got via query, and the variant
         --  cases, which we want. There are no siblings.
         Control                   := Continue;
         State.Private_State.Label := In_Variant_Part;

      when others =>
         Debug.Put_line ( Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;

      end case;

   when others =>
      Debug.Put_line ( Skipping);
      Report_Unsupported (State, Element);
      Control := Abandon_Siblings;

   end case;

end Auto_Io_Gen.Build.Process_Element_Do_Record_Type;
