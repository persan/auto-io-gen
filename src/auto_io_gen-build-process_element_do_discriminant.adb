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
procedure Auto_Io_Gen.Build.Process_Element_Do_Discriminant
   (Element : in     Asis.Element;
    Control : in out Asis.Traverse_Control;
    State   : in out State_Type)
is
   use Asis;
begin
   Debug.Put_Line ("Element =>" & Elements.Element_Kind (Element)'Img & ", State => " & State.Private_State.Label'Img);
   case Elements.Element_Kind (Element) is
   when A_Defining_Name =>
      case Elements.Defining_Name_Kind (Element) is
      when A_Defining_Identifier =>
         Debug.Put_line ( Processing);
         State.Private_State.Current_Component.Component_Name := Element;

         --  There are no children of Defining_Identifiers, but the
         --  siblings include the type name and default expression.
         Control := Continue;

      when others =>
         Debug.Put_line ( Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;
      end case;

   when An_Expression =>
      case Elements.Expression_Kind (Element) is
      when An_Identifier =>
         --  The discriminant type.
         Debug.Put_line ( Processing);

         Find_Defining_Package
            (State,
             Element,
             Result_Package => State.Private_State.Current_Component.Type_Package);

         State.Private_State.Current_Component.Type_Name := Element;

         begin
            State.Private_State.Current_Component.Scalar := Is_Scalar (Element);

            State.Private_State.Current_Component.Invisible :=
              Elements.Is_Nil (State.Private_State.Current_Component.Type_Package) and
              Is_Invisible (State, Element);

            Lists.Component_Lists.Insert_Tail
               (State.Private_State.Current_Type.Record_Discriminants,
                State.Private_State.Current_Component);
         exception
         when Not_Supported =>
            Report_Unsupported (State, Element, "Record discriminants of this type ");
            Control := Abandon_Siblings;
         end;

         --  Siblings of this element include the default, if any,
         --  which we've already checked for.
         Control := Abandon_Siblings;

      when A_Selected_Component =>
         Debug.Put_line ( Processing);

         State.Private_State.Current_Component.Type_Name := Expressions.Selector (Element);

         begin
            Find_Defining_Package
               (State,
                Element,
                Result_Package  => State.Private_State.Current_Component.Type_Package);

            State.Private_State.Current_Component.Scalar :=
               Is_Scalar (State.Private_State.Current_Component.Type_Name);

            State.Private_State.Current_Component.Invisible := False;

            Lists.Component_Lists.Insert_Tail
               (State.Private_State.Current_Type.Record_Discriminants,
                State.Private_State.Current_Component);

         exception
         when Not_Supported =>
            Report_Unsupported (State, Element, "Record discriminants of this type ");
         end;

         --  The children of this element are the individual
         --  identifiers in the selected component; we don't need to
         --  see them individually. The siblings of this element
         --  include the default, if any, which we've already checked
         --  for.
         Control := Abandon_Siblings;

      when others =>
         Debug.Put_line ( Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;
      end case;

   when others =>
      Debug.Put_line ( Skipping);
      Report_Unsupported (State, Element);
      Control := Abandon_Children;

   end case;

end Auto_Io_Gen.Build.Process_Element_Do_Discriminant;
