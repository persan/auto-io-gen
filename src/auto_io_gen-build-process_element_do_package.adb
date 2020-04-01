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
procedure Auto_Io_Gen.Build.Process_Element_Do_Package
  (Element : in     Asis.Element;
   Control : in out Asis.Traverse_Control;
   State   : in out State_Type)
is
   use Asis;
   Ignore : Boolean;
begin
   Debug.Put_Line ("Element =>" & Elements.Element_Kind (Element)'Img & ", State => " & State.Private_State.Label'Img);

   State.Private_State.Abandon_Current_Type := False;

   case Elements.Element_Kind (Element) is
   when
     A_Clause |
     A_Pragma =>
      --  don't need Text_IO

      Debug.Put_Line (Skipping);

      Control := Abandon_Children;

   when A_Declaration =>
      Annotations (Element, Ignore, State.Private_State.Separate_Body);

      if Ignore then
         Debug.Put_Line (Ignoring);
         Control := Abandon_Children;

      else
         case Elements.Declaration_Kind (Element) is
         --  List in alphabetical order (ignore a_, an_), except all
         --  that are skipped are in this first case.
         when
           A_Constant_Declaration |
           A_Deferred_Constant_Declaration |
           An_Exception_Declaration |
           A_Formal_Function_Declaration |
           A_Function_Declaration |
           A_Function_Instantiation |
           A_Function_Renaming_Declaration |
           A_Generic_Procedure_Declaration |
           An_Integer_Number_Declaration |
           A_Package_Instantiation |
           A_Package_Renaming_Declaration |
           A_Procedure_Declaration |
           A_Procedure_Instantiation |
           A_Procedure_Renaming_Declaration |
           A_Real_Number_Declaration |

           A_Subtype_Declaration | -- Text_IO for full type will be created or with'ed

           A_Variable_Declaration =>

            --  These don't need Text_IO

            Debug.Put_Line (Skipping);

            Control := Abandon_Children;

         when A_Formal_Package_Declaration =>
            Debug.Put_Line (Processing);

            Lists.Formal_Package_Lists.Add (State.Formal_Package_List, Element);

            State.Private_State.Current_Formal_Package := Lists.Formal_Package_Lists.Last (State.Formal_Package_List);

            Add_Spec_With (State, Text_IO_Child_Name (Element, With_Clause), Invisible => False);

            --  Add instantiation arguments for text_io children of
            --  common ancestor generic packages. Currently, we assume
            --  all ancestors are common.
            declare
               use Lists.Formal_Package_Lists;
               Arguments : Lists.Element_Lists.List_Type renames
                 Current (State.Private_State.Current_Formal_Package).Arguments;

               I : Iterator_Type := First (State.Formal_Package_List);
            begin
               loop
                  exit when Is_Null (I);
                  exit when Elements.Declaration_Kind (Current (I).Package_Declaration) /=
                    A_Generic_Package_Declaration;

                  Lists.Element_Lists.Add (Arguments, Current (I).Package_Declaration);

                  Next (I);
               end loop;
            end;

            --  The children of this element include the generic unit
            --  and arguments, which we need.
            Control                   := Continue;
            State.Private_State.Label := In_Formal_Package;

         when A_Formal_Type_Declaration |
           An_Ordinary_Type_Declaration |
           A_Private_Type_Declaration |
           A_Private_Extension_Declaration =>

            Debug.Put_Line ( Processing);

            --  The children of a type declaration include the type
            --  name and components, which we want.
            Control                          := Continue;
            State.Private_State.Label        := In_Type;
            State.Private_State.Current_Type := null;

         when others =>
            Debug.Put_Line (Processing);
            Report_Unsupported (State, Element);
            Control := Abandon_Children;
         end case;
      end if;

   when A_Defining_Name =>
      --  The package name.

      Debug.Put_Line (Skipping);

      Control := Abandon_Children;

   when others =>
      Debug.Put_Line (Skipping);

      Report_Unsupported (State, Element);

      Control := Abandon_Children;

   end case;

end Auto_Io_Gen.Build.Process_Element_Do_Package;
