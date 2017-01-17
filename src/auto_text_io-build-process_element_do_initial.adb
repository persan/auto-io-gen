--  Abstract :
--
--  Do processing for Initial state in Build.Process_Element.
--
--  Copyright (C) 2001 - 2003, 2007 Stephen Leake.  All Rights Reserved.
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

with Asis.Compilation_Units;
with Asis.Elements;
with Auto_Text_IO.Build.Create_Invisible_Type_List;
with Auto_Text_IO.Build.Process_Element_Utils; use Auto_Text_IO.Build.Process_Element_Utils;
procedure Auto_Text_IO.Build.Process_Element_Do_Initial
  (Element : in     Asis.Element;
   Control : in out Asis.Traverse_Control;
   State   : in out State_Type)
is
   use Asis;
begin

   case Elements.Element_Kind (Element) is
   when A_Declaration =>
      case Elements.Declaration_Kind (Element) is
      when A_Package_Declaration |
        A_Generic_Package_Declaration =>
         Debug_Put (Element, Processing);

         State.Parent_Package                := Element;
         State.Needs_Body                    := False;
         State.Needs_Text_IO_Utils           := False;
         State.Needs_Invisible_Spec          := False;
         State.Needs_Invisible_Body          := False;
         State.Needs_Invisible_Text_IO_Utils := False;
         State.Is_Generic                    := Elements.Declaration_Kind (Element) = A_Generic_Package_Declaration;

         if State.Is_Generic then
            --  We need generic formal package parameters for the
            --  text_io children of all our generic ancestors.
            declare
               Parent : Compilation_Unit := Elements.Enclosing_Compilation_Unit (Element);
            begin
               loop
                  Parent := Compilation_Units.Corresponding_Parent_Declaration (Parent);
                  exit when Compilation_Units.Unit_Kind (Parent) /= A_Generic_Package;
                  Lists.Formal_Package_Lists.Add (State.Formal_Package_List, Elements.Unit_Declaration (Parent));
                  Add_Spec_With
                    (State, Text_IO_Child_Name (Elements.Unit_Declaration (Parent), With_Clause), Invisible => False);
                  Add_Spec_With
                    (State, Text_IO_Child_Name (Elements.Unit_Declaration (Parent), With_Clause), Invisible => True);
               end loop;
            end;
         end if;


         --  Top level package. The name from this is used to build
         --  the child package name, but that is saved elsewhere, not
         --  in State. The children of this element include types,
         --  which we want.
         Control                   := Continue;
         State.Private_State.Label := In_Package;

         --  We also need the list of types in the private part, so we
         --  can tell if we need a private Text_IO child.
         Build.Create_Invisible_Type_List (State, Element);

      when others =>
         raise Program_Error;
      end case;
   when others =>
      raise Program_Error;
   end case;

end Auto_Text_IO.Build.Process_Element_Do_Initial;
