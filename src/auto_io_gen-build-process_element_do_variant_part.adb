--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003 - 2004 Stephen Leake.  All Rights Reserved.
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
with Asis.Definitions;
with Asis.Elements;
with Auto_Io_Gen.Build.Process_Element_Utils; use Auto_Io_Gen.Build.Process_Element_Utils;
procedure Auto_Io_Gen.Build.Process_Element_Do_Variant_Part
   (Element : in     Asis.Element;
    Control : in out Asis.Traverse_Control;
    State   : in out State_Type)
is
   use Asis;
begin
   case Elements.Element_Kind (Element) is
   when A_Declaration =>
      case Elements.Declaration_Kind (Element) is
      when A_Component_Declaration =>

         Debug_Put (Element, Processing);

         declare
            Component            : Lists.Component_Type;
            Component_Names      : constant Element_List := Declarations.Names (Element);
            Component_Definition : constant Asis.Element := Declarations.Object_Declaration_View (Element);
            Subtype_Indication   : constant Asis.Element :=
              Definitions.Component_Subtype_Indication (Component_Definition);
         begin
            if Component_Names'Length > 1 then
               Report_Unsupported (State, Element);
               Control := Abandon_Siblings;
            else
               Component.Component_Name := Component_Names (Component_Names'First);
               Component.Type_Name      := Definitions.Subtype_Mark (Subtype_Indication);
               Component.Scalar         := Is_Scalar (Component.Type_Name);

               Find_Defining_Package
                 (State,
                  Component.Type_Name,
                  Result_Package  => Component.Type_Package);

               Component.Invisible := Elements.Is_Nil (Component.Type_Package) and
                 Is_Invisible (State, Component.Type_Name);

               Lists.Component_Lists.Add (State.Private_State.Current_Variant.Components, Component);

               State.Private_State.Current_Type.Record_Structured_Components :=
                 State.Private_State.Current_Type.Record_Structured_Components or
                 (not Component.Scalar);

               --  The children of components include the component
               --  name and type name, which we got by query. The
               --  siblings are the other components.
               Control := Abandon_Children;
            end if;
         exception
         when Not_Supported =>
            Report_Unsupported (State, Element, "Record components of this type ");
            Control := Abandon_Siblings;
         end;

      when others =>
         Debug_Put (Element, Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;
      end case;

   when A_Definition =>
      case Elements.Definition_Kind (Element) is
      when A_Variant =>
         Debug_Put (Element, Processing);
         declare
            Choices  : constant Element_List := Definitions.Variant_Choices (Element);
            Variants : Lists.Variant_Lists.List_Type renames
              State.Private_State.Current_Type.Record_Variant_Part.Variants;
         begin
            if Choices'Length > 1 or
              Elements.Element_Kind (Choices (Choices'First)) /= An_Expression
            then
               Report_Unsupported (State, Element);
               Control := Abandon_Siblings;
            else
               Lists.Variant_Lists.Add (Variants, Choices (Choices'First));
               State.Private_State.Current_Variant := Lists.Variant_Lists.Current (Lists.Variant_Lists.Last (Variants));

               --  The children include the components, which we want.
               Control := Continue;
            end if;
         end;

      when A_Null_Component =>
         --  No components is ok.
         Control := Abandon_Children;

      when others =>
         Debug_Put (Element, Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;
      end case;

   when An_Expression =>
      --  Some expression, containing stuff that we got by query. The
      --  children include identifiers, which we don't need.
      Debug_Put (Element, Skipping);
      Control := Abandon_Children;

   when others =>
      Debug_Put (Element, Skipping);
      Report_Unsupported (State, Element);
      Control := Abandon_Siblings;

   end case;

end Auto_Io_Gen.Build.Process_Element_Do_Variant_Part;
