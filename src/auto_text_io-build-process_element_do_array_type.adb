--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2001 - 2004, 2007 Stephen Leake.  All Rights Reserved.
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
with Auto_Text_IO.Build.Process_Element_Utils; use Auto_Text_IO.Build.Process_Element_Utils;
procedure Auto_Text_IO.Build.Process_Element_Do_Array_Type
   (Element : in     Asis.Element;
    Control : in out Asis.Traverse_Control;
    State   : in out State_Type)
is
   use Asis;
begin

   case Elements.Element_Kind (Element) is
   when A_Definition =>
      case Elements.Definition_Kind (Element) is
      when A_Component_Definition =>

         Debug_Put (Element, Processing);

         --  The child of this element is the component type.
         Control := Continue;

      when A_Discrete_Subtype_Definition |
         A_Subtype_Indication =>
         Debug_Put (Element, Processing);

         --  The child of this element is the index or component type name.
         Control := Continue;

      when others =>
         Debug_Put (Element, Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;
      end case;

   when An_Expression =>
      declare
         Expression_Kind : constant Expression_Kinds := Elements.Expression_Kind (Element);
      begin
         case Expression_Kind is
         when An_Identifier | A_Selected_Component =>
            --  This is the index or component type name

            Debug_Put (Element, Processing);

            --  Don't need any children; sibling of index type is
            --  array component type.
            Control := Abandon_Children;

            if Elements.Is_Nil (State.Private_State.Current_Type.Array_Index) then
               --  This is the index type name
               begin
                  case Expression_Kind is
                  when An_Identifier =>
                     State.Private_State.Current_Type.Array_Index := Element;

                  when A_Selected_Component =>
                     State.Private_State.Current_Type.Array_Index := Expressions.Selector (Element);

                  when others =>
                     null;
                  end case;

                  Find_Defining_Package
                    (State,
                     Element,
                     Array_Role     => Index,
                     Result_Package => State.Private_State.Current_Type.Array_Index_Package);
               exception
               when Not_Supported =>
                  Report_Unsupported (State, Element, "Array index of this type ");
                  Control := Abandon_Siblings;
               end;

            else
               --  This is the component type name
               begin
                  Set_Array_Component (State, Element, State.Private_State.Current_Type.all);
               exception
               when Not_Supported =>
                  Report_Unsupported (State, Element, "Array components of this type ");
                  Control := Abandon_Siblings;
               end;

            end if;

         when An_Integer_Literal =>

            Debug_Put (Element, Skipping);
            Report_Unsupported (State, Element, "Arrays with anonymous index type ");
            Control := Abandon_Siblings;

         when others =>
            Debug_Put (Element, Skipping);
            Report_Unsupported (State, Element);
            Control := Abandon_Siblings;
         end case;
      end;

   when others =>
      Debug_Put (Element, Skipping);

      Report_Unsupported (State, Element);
      Control := Abandon_Siblings;

   end case;

end Auto_Text_IO.Build.Process_Element_Do_Array_Type;
