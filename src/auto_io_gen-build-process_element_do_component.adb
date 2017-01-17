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
with Auto_Io_Gen.Build.Process_Element_Utils; use Auto_Io_Gen.Build.Process_Element_Utils;
procedure Auto_Io_Gen.Build.Process_Element_Do_Component
   (Element : in     Asis.Element;
    Control : in out Asis.Traverse_Control;
    State   : in out State_Type)
is
   use Asis;
begin

   case Elements.Element_Kind (Element) is
   when A_Defining_Name =>

      case Elements.Defining_Name_Kind (Element) is
      when A_Defining_Identifier =>
         Debug_Put (Element, Processing);

         State.Private_State.Current_Component.Component_Name := Element;

         --  There are no children of Defining_Identifiers, but
         --  the siblings contain the type name.
         Control := Continue;

      when others =>
         Debug_Put (Element, Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;
      end case;

   when A_Definition =>

      case Elements.Definition_Kind (Element) is
      when A_Component_Definition =>
         Debug_Put (Element, Processing);

         --  The children of this element include the component
         --  type name, which we want.
         Control                   := Continue;
         State.Private_State.Label := In_Component_Definition;

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

end Auto_Io_Gen.Build.Process_Element_Do_Component;
