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

with Asis.Elements;
with Asis.Expressions;
with Auto_Io_Gen.Build.Process_Element_Utils; use Auto_Io_Gen.Build.Process_Element_Utils;
procedure Auto_Io_Gen.Build.Process_Element_Do_Component_Definition
   (Element : in     Asis.Element;
    Control : in out Asis.Traverse_Control;
    State   : in out State_Type)
is
   use Asis;
begin
   case Elements.Element_Kind (Element) is
   when A_Definition =>
      case Elements.Definition_Kind (Element) is
      when A_Subtype_Indication =>
         Debug_Put (Element, Processing);

         --  The children of this element include the subtype name.
         Control := Continue;

      when others =>
         Debug_Put (Element, Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Children;
      end case;

   when An_Expression =>
      case Elements.Expression_Kind (Element) is
      when An_Identifier =>
         --  This is the component type name.
         Debug_Put (Element, Processing);

         State.Private_State.Current_Component.Type_Name := Element;

         begin
            Find_Defining_Package
               (State,
                Element,
                Result_Package => State.Private_State.Current_Component.Type_Package);

            State.Private_State.Current_Component.Scalar := Is_Scalar (Element);

            State.Private_State.Current_Component.Invisible :=
              Elements.Is_Nil (State.Private_State.Current_Component.Type_Package) and
              Is_Invisible (State, Element);

            State.Private_State.Current_Type.Record_Structured_Components :=
              State.Private_State.Current_Type.Record_Structured_Components or
              (not State.Private_State.Current_Component.Scalar);

            Lists.Component_Lists.Insert_Tail
               (State.Private_State.Current_Type.Record_Components,
                State.Private_State.Current_Component);

            --  No children of this element. The siblings are
            --  constraints; we don't want to see them.
            Control := Abandon_Siblings;

         exception
         when Not_Supported =>
            Report_Unsupported (State, Element, "Record components of this type ");
            Control := Abandon_Siblings;
         end;

      when A_Selected_Component =>
         --  This is the component type name.
         Debug_Put (Element, Processing);

         State.Private_State.Current_Component.Type_Name := Expressions.Selector (Element);

         begin
            Find_Defining_Package
               (State,
                Element,
                Result_Package  => State.Private_State.Current_Component.Type_Package);

            State.Private_State.Current_Component.Scalar :=
              Is_Scalar (State.Private_State.Current_Component.Type_Name);

            State.Private_State.Current_Component.Invisible := False;

            State.Private_State.Current_Type.Record_Structured_Components :=
               State.Private_State.Current_Type.Record_Structured_Components or
               (not State.Private_State.Current_Component.Scalar);

            Lists.Component_Lists.Insert_Tail
               (State.Private_State.Current_Type.Record_Components,
                State.Private_State.Current_Component);

            --  The children of this element are the individual
            --  identifiers in the selected component; we don't need
            --  to see them individually. The siblings are
            --  constraints; we don't want to see them either.
            Control := Abandon_Siblings;
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

   when others =>
      Debug_Put (Element, Skipping);
      Report_Unsupported (State, Element);
      Control := Abandon_Children;

   end case;

end Auto_Io_Gen.Build.Process_Element_Do_Component_Definition;
