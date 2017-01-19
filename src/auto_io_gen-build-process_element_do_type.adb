--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2001 - 2004, 2006 Stephen Leake.  All Rights Reserved.
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
with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Auto_Io_Gen.Build.Process_Element_Utils; use Auto_Io_Gen.Build.Process_Element_Utils;
procedure Auto_Io_Gen.Build.Process_Element_Do_Type
   (Element : in     Asis.Element;
    Control : in out Asis.Traverse_Control;
    State   : in out State_Type)
is
   use Asis;
   use type Lists.Type_Descriptor_Access_Type;
begin
   Debug.Put_Line ("Element =>" & Elements.Element_Kind (Element)'Img & ", State => " & State.Private_State.Label'Img);

   case Elements.Element_Kind (Element) is
   when A_Defining_Name =>

      case Elements.Defining_Name_Kind (Element) is
      when A_Defining_Identifier =>


         State.Private_State.Current_Type_Name := Element;

         Check_Private_Type_Completion (State);

         --  There are no children of this element, but we want the
         --  siblings, which include the discriminants and type
         --  definition.
         Control := Abandon_Children;

      when others =>
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;

      end case;

   when A_Definition =>
      Debug.Put_Line ("Definition_Kind:" & Elements.Definition_Kind (Element)'Img);

      case Elements.Definition_Kind (Element) is
      when A_Formal_Type_Definition =>
         Debug.Put_Line ("Formal_Kind:" & Elements.Formal_Type_Kind (Element)'Img);

         case Elements.Formal_Type_Kind (Element) is
         when A_Formal_Discrete_Type_Definition =>
            Debug.Put_line ( Processing);
            Add_Descriptor (State, Lists.Enumeration_Label);
            --  We don't need anything more than the name.
            Control := Abandon_Children;

         when A_Formal_Floating_Point_Definition =>
            Debug.Put_line ( Processing);
            Add_Descriptor (State, Lists.Float_Label);
            --  We don't need anything more than the name.
            Control := Abandon_Children;

         when A_Formal_Modular_Type_Definition =>
            Debug.Put_line ( Processing);
            Add_Descriptor (State, Lists.Modular_Integer_Label);
            --  We don't need anything more than the name.
            Control := Abandon_Children;

         when A_Formal_Signed_Integer_Type_Definition =>
            Debug.Put_line ( Processing);
            Add_Descriptor (State, Lists.Signed_Integer_Label);
            --  We don't need anything more than the name.
            Control := Abandon_Children;
--           when A_ASPECT_SPECIFICATION =>
--              Debug.Put_Line ( Skipping);

         when others =>
            Debug.Put_line ( Skipping);
            Report_Unsupported (State, Element);
            Control := Abandon_Siblings;

         end case;
      when AN_ASPECT_SPECIFICATION =>
         Debug.Put_Line ( Skipping);
         --  Control := Abandon_Siblings;

      when A_Known_Discriminant_Part =>
         Debug.Put_line ( Processing);

         if State.Private_State.Private_Type_Completion then
            --  The children of this element include the discriminants
            --  of the record, but we've already seen them in the
            --  public definition.
            Control                   := Abandon_Children;
            State.Private_State.Label := In_Type;
         else
            Add_Record_Descriptor (State);
            --  We need the discriminants.
            Control                   := Continue;
            State.Private_State.Label := In_Discriminant_Part;
         end if;

      when A_Private_Type_Definition |
        A_Private_Extension_Definition =>
         Debug.Put_line ( Processing);

         --  Add a descriptor now, even though we don't know what the
         --  full type is, so the order of Put and Get declarations in
         --  the Text_IO child matches the order of the type
         --  declarations in the parent; there may be a dependency for
         --  instantiating SAL.Gen_Array_Text_IO.
         Add_Private_Descriptor (State);

         --  There are no children or siblings of this element; we'll
         --  get the components of the record when we process the full
         --  definition.
         Control := Abandon_Siblings;

      when A_Type_Definition =>

         case Elements.Type_Kind (Element) is
         when A_Constrained_Array_Definition |
            An_Unconstrained_Array_Definition =>

            Debug.Put_line ( Processing);

            if Elements.Type_Kind (Element) = An_Unconstrained_Array_Definition then
               Add_Descriptor (State, Lists.Unconstrained_Array_Label);
            else
               Add_Descriptor (State, Lists.Constrained_Array_Label);
            end if;

            if State.Private_State.Current_Type.Private_Implementation then
               State.Needs_Body := True;
            end if;

               Add_Spec_With (State, "SAL.Gen_Array_Text_IO", State.Private_State.Current_Type.Invisible);

            --  The children of this element include the index and
            --  component types of the array, which we want.
            Control                   := Continue;
            State.Private_State.Label := In_Array_Type;

         when A_Derived_Record_Extension_Definition =>

            Debug.Put_line ( Processing);

            Add_Record_Descriptor (State);

            State.Private_State.Current_Type.Record_Tagged  := True;
            State.Private_State.Current_Type.Record_Derived := True;

            declare
               Parent_Type    : constant Asis.Element := Definitions.Corresponding_Parent_Subtype (Element);
               Parent_Package : constant Asis.Element := Elements.Enclosing_Element (Parent_Type);
            begin
               State.Private_State.Current_Type.Record_Parent_Type_Name    := Declarations.Names (Parent_Type) (1);
               State.Private_State.Current_Type.Record_Parent_Package_Name := Declarations.Names (Parent_Package)(1);
            end;

            --  The children of this element include the
            --  components of the record, which we want.
            Control                   := Continue;
            State.Private_State.Label := In_Record_Type;

         when A_Derived_Type_Definition =>
            --  We know the type is not tagged (that would be
            --  A_Derived_Record_Extension_Definition). See what
            --  the root type is; we only create a Derived_Label for
            --  array types.

            declare
               Root_Declaration : constant Asis.Element := Definitions.Corresponding_Root_Type (Element);
               Root_Definition  : constant Asis.Element := Declarations.Type_Declaration_View (Root_Declaration);
            begin

               case Elements.Type_Kind (Root_Definition) is
               when A_Constrained_Array_Definition |
                 An_Unconstrained_Array_Definition =>
                  Debug.Put_line ( Processing);

                  Add_Descriptor (State, Lists.Derived_Label);

                  State.Needs_Body := True;

                  declare
                     use Auto_Io_Gen.Lists;
                     Node : Type_Descriptor_Access_Type renames State.Private_State.Current_Type;
                  begin
                     Node.Derived_Root_Type_Declaration := Root_Declaration;

                     Node.Derived_Root_Package_Declaration :=
                       Elements.Unit_Declaration (Elements.Enclosing_Compilation_Unit (Root_Declaration));

                     if Elements.Type_Kind (Root_Definition) = A_Constrained_Array_Definition then
                        Node.Derived_Root_Label := Lists.Constrained_Array_Label;
                     else
                        Node.Derived_Root_Label := Lists.Unconstrained_Array_Label;
                     end if;

                     Process_Element_Utils.Add_To_Context
                       (State,
                        Package_Declaration  => Node.Derived_Root_Package_Declaration,
                        Type_Declaration     => Root_Declaration,
                        Original_Declaration => Element);

                     --  We don't need anything more than the name.
                     Control := Abandon_Children;
                  end;

               when An_Enumeration_Type_Definition =>
                  Debug.Put_line ( Processing);

                  Add_Descriptor (State, Lists.Enumeration_Label);

                  --  We don't need anything more than the name.
                  Control := Abandon_Children;

               when A_Floating_Point_Definition =>
                  Debug.Put_line ( Processing);

                  Add_Descriptor (State, Lists.Float_Label);

                  --  We don't need anything more than the name.
                  Control := Abandon_Children;

               when A_Signed_Integer_Type_Definition =>
                  Debug.Put_line ( Processing);

                  Add_Descriptor (State, Lists.Signed_Integer_Label);

                  --  We don't need anything more than the name.
                  Control := Abandon_Children;

               when others =>
                  Debug.Put_line ( Skipping);
                  Debug_Put ("           Root type " & Aux.Image (Root_Definition));
                  Report_Unsupported (State, Element);
                  Control := Abandon_Siblings;

               end case;

            end;

         when An_Enumeration_Type_Definition =>
            Debug.Put_line ( Processing);
            Add_Descriptor (State, Lists.Enumeration_Label);
            --  We don't need anything more than the name.
            Control := Abandon_Children;

         when A_Floating_Point_Definition =>
            Debug.Put_line ( Processing);
            Add_Descriptor (State, Lists.Float_Label);
            --  We don't need anything more than the name.
            Control := Abandon_Children;

         when A_Modular_Type_Definition =>
            Debug.Put_line ( Processing);
            Add_Descriptor (State, Lists.Modular_Integer_Label);
            --  We don't need anything more than the name.
            Control := Abandon_Children;

         when An_Ordinary_Fixed_Point_Definition =>
            Debug.Put_line ( Processing);
            Add_Descriptor (State, Lists.Fixed_Label);
            --  We don't need anything more than the name.
            Control := Abandon_Children;

         when A_Record_Type_Definition =>

            Debug.Put_line ( Processing);

            Add_Record_Descriptor (State);

            --  The children of this element include the
            --  components of the record, which we want.
            Control                   := Continue;
            State.Private_State.Label := In_Record_Type;

         when A_Signed_Integer_Type_Definition =>
            Debug.Put_line ( Processing);
            Add_Descriptor (State, Lists.Signed_Integer_Label);
            --  We don't need anything more than the name.
            Control := Abandon_Children;

         when A_Tagged_Record_Type_Definition =>

            Debug.Put_line ( Processing);

            Add_Record_Descriptor (State);

            State.Private_State.Current_Type.Record_Tagged := True;

            --  The children of this element include the
            --  components of the record, which we want.
            Control                   := Continue;
            State.Private_State.Label := In_Record_Type;

         when others =>
            Debug.Put_line ( Skipping);
            Report_Unsupported (State, Element);
            Control := Abandon_Siblings;

         end case;

      when others =>
         Debug.Put_line (Skipping);
         Report_Unsupported (State, Element);
         Control := Abandon_Siblings;

      end case; --  Definition_Kinds

   when others =>
      Debug.Put_line ( Skipping);
      Report_Unsupported (State, Element);
      Control := Abandon_Siblings;
   end case;

end Auto_Io_Gen.Build.Process_Element_Do_Type;
