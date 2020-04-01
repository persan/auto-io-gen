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

with Asis.Aux;
with Asis.Elements;
with Asis.Expressions;
with Auto_Io_Gen.Build.Process_Element_Utils; use Auto_Io_Gen.Build.Process_Element_Utils;
procedure Auto_Io_Gen.Build.Process_Element_Do_Formal_Package
  (Element : in     Asis.Element;
   Control : in out Asis.Traverse_Control;
   State   : in out State_Type)
is
   use Asis;
   Ignore        : Boolean;
   Separate_Body : Boolean;
begin
   Debug.Put_Line ("Element =>" & Elements.Element_Kind (Element)'Img & ", State => " & State.Private_State.Label'Img);

   case Elements.Element_Kind (Element) is
   when An_Association =>
      --  An instantiation argument.

      Annotations (Element, Ignore, Separate_Body);

      if Ignore then
         Debug_Put (Element, Ignoring);
         Control := Abandon_Children;
      else
         Debug.Put_line ( Processing);

         --  We use a query to get the actual parameter, rather than
         --  waiting until the Asis iterator gets us there - because
         --  the formal parameter is optional, we can't tell when we
         --  get to the actual parameter.
         --
         --  We put the corresponding formal package declaration on
         --  the argument list, so Text_IO_Child_Name can be used to
         --  compute the name. This also determines if the parameter
         --  should be ignored because the formal package itself was
         --  ignored.

         declare
            Actual_Parameter    : constant Asis.Element := Expressions.Actual_Parameter (Element);
            Package_Declaration : constant Asis.Element :=
              Lists.Find_Package (State.Formal_Package_List, Aux.Name (Actual_Parameter));
         begin
            if not Elements.Is_Nil (Package_Declaration) then
               Lists.Element_Lists.Add
                 (List => Lists.Formal_Package_Lists.Current
                    (Iterator => State.Private_State.Current_Formal_Package).Arguments,
                  Item => Package_Declaration);
            end if;
         end;

         --  The children include the actual argument name, but we already got that.
         Control := Abandon_Children;
      end if;

   when A_Defining_Name =>

      case Elements.Defining_Name_Kind (Element) is
      when A_Defining_Identifier =>
         --  The formal package name.

         Debug.Put_line ( Processing);

         --  There are no children of this element. The siblings of
         --  this element include the instantiation arguments, which
         --  we need.
         Control := Abandon_Children;

      when others =>
         Debug.Put_line ( Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;

      end case;

   when An_Expression =>
      --  The Generic_Unit_Name; accessed by Text_IO_Child_Name via
      --  the Formal_Package_Declaration, so we don't need to save it.
      Debug.Put_line ( Processing);

      --  The children of this element are the identifiers of a
      --  selected component, which we don't want. The siblings
      --  include the instantiation arguments, which we do want.
      Control := Abandon_Children;

   when others =>
      Debug.Put_line ( Skipping);
      Report_Unsupported (State, Element);
      Control := Abandon_Siblings;
   end case;

end Auto_Io_Gen.Build.Process_Element_Do_Formal_Package;
