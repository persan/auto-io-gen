--  Abstract :
--
--  See spec
--
--  Copyright (C) 2001 - 2006 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Asis.Aux;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Expressions;
with Asis.Extensions;
with Asis.Text;
with Auto_Io_Gen.Options;
package body Auto_Io_Gen.Build.Process_Element_Utils is

   --  Local subprogram bodies

   procedure Add_Text_IO_Body_With
     (State               : in out State_Type;
      Package_Declaration : in     Asis.Element;
      Need_Use            : in     Boolean      := False)
     --  Add 'with' for Package_Name text_io child to appropriate list
     --  in State.
   is
      use Asis;
   begin
      case Elements.Declaration_Kind (Package_Declaration) is
      when A_Formal_Package_Declaration |
           A_Generic_Package_Declaration =>

         if Lists.Is_Present (State.Formal_Package_List, Aux.Name (Package_Declaration)) then
            --  Already handled.
            null;
         else
            raise Not_Supported;
         end if;

      when others =>
         Add_Body_With (State, Text_IO_Child_Name (Package_Declaration), Need_Use);

      end case;
   end Add_Text_IO_Body_With;

   procedure Add_Text_IO_Spec_With
     (State               : in out State_Type;
      Package_Declaration : in     Asis.Element)
     --  Add 'with' for Package_Name text_io child to appropriate list
     --  in State.
   is
      use Asis;
   begin
      case Elements.Declaration_Kind (Package_Declaration) is
      when A_Formal_Package_Declaration |
           A_Generic_Package_Declaration =>

         if Lists.Is_Present (State.Formal_Package_List, Aux.Name (Package_Declaration)) then
            --  Already handled when formal parameter list was traversed.
            null;
         else
            raise Not_Supported;
         end if;

      when others =>
         Add_Spec_With
           (State, Text_IO_Child_Name (Package_Declaration, With_Clause), State.Private_State.Current_Type.Invisible);

      end case;

   end Add_Text_IO_Spec_With;

   procedure Corresponding_Type
     (El                        : in     Asis.Element;
      Corresponding_Definition  :    out Asis.Element;
      Corresponding_Declaration :    out Asis.Element)
     --  El must be An_Identifier or A_Selected_Component from a type
     --  name. Return the corresponding type declaration and
     --  definition, _not_ any intermediate corresponding subtype
     --  declarations. The result may be a derived type declaration or
     --  a generic formal.
     --
     --  Raises Not_Supported if El is not supported; caller should
     --  output an error message.
   is
      use Asis;
   begin
      case Elements.Element_Kind (El) is
      when An_Expression =>

         case Elements.Expression_Kind (El) is
         when An_Identifier =>
            Corresponding_Declaration := Expressions.Corresponding_Name_Declaration (El);

         when A_Selected_Component =>
            Corresponding_Declaration := Expressions.Corresponding_Name_Declaration (Expressions.Selector (El));

         when others =>
            raise Program_Error;

         end case;

      when others =>
         raise Program_Error;

      end case;

      case Elements.Element_Kind (Corresponding_Declaration) is
      when A_Declaration =>

         case Elements.Declaration_Kind (Corresponding_Declaration) is
         when A_Formal_Type_Declaration |
              An_Ordinary_Type_Declaration |
              A_Private_Type_Declaration |
              A_Subtype_Declaration =>

            Corresponding_Definition := Declarations.Type_Declaration_View (Corresponding_Declaration);

            case Elements.Definition_Kind (Corresponding_Definition) is
            when A_Subtype_Indication =>

               Corresponding_Type
                 (Asis.Definitions.Subtype_Mark (Corresponding_Definition),
                  Corresponding_Definition, Corresponding_Declaration);
               return;

            when others =>
               return;

            end case;

         when others =>
            raise Not_Supported;

         end case;

      when others =>
         raise Program_Error;

      end case;

   end Corresponding_Type;

   function Corresponding_Root_Type_Definition (El : in Asis.Element) return Asis.Element
   --  El must be An_Identifier or A_Selected_Component from a type
   --  name. Return the corresponding root type declaration, _not_
   --  any intermediate corresponding subtype declaration or derived
   --  type declaration.
   --
   --  Raises Not_Supported if El is not supported; caller should
   --  output an error message.

   is
      use Asis;
      Corresponding_Definition  : Element;
      Corresponding_Declaration : Element;
   begin
      Corresponding_Type (El, Corresponding_Definition, Corresponding_Declaration);

      if Elements.Type_Kind (Corresponding_Definition) = A_Derived_Type_Definition then
         Corresponding_Declaration := Definitions.Corresponding_Root_Type (Corresponding_Definition);
         Corresponding_Definition  := Declarations.Type_Declaration_View (Corresponding_Declaration);
      end if;

      return Corresponding_Definition;
   end Corresponding_Root_Type_Definition;

   procedure Replace_Descriptor
     (State    : in out State_Type;
      Label    : in     Lists.Type_Labels_Type;
      Replaced :    out Boolean)
     --  If Label does not match Private_Type_Iterator label, replace
     --  Private_Type_Iterator descriptor with a Label descriptor,
     --  copying the current data. Set State.Private_Type.Current_Type.
   is
      use Lists;
      use Type_Descriptor_Lists;
      I              : Lists.Type_Descriptor_Lists.Iterator_Type renames State.Private_State.Private_Type_Iterator;
      Old_Descriptor : constant Type_Descriptor_Access_Type := Current (I);
   begin
      Replaced := Label /= Old_Descriptor.Label;

      if Replaced then
         declare
            New_Descriptor : Type_Descriptor_Type (Label);
         begin
            New_Descriptor.Type_Name              := Old_Descriptor.Type_Name;
            New_Descriptor.Invisible              := Old_Descriptor.Invisible;
            New_Descriptor.Private_Implementation := Old_Descriptor.Private_Implementation;
            New_Descriptor.Separate_Body          := Old_Descriptor.Separate_Body;

            Replace (State.Type_List, I, New_Descriptor);

            State.Private_State.Current_Type := Current (I);
         end;
      else
         State.Private_State.Current_Type := Old_Descriptor;
      end if;

   end Replace_Descriptor;

   -------------
   --  Public subprograms

   procedure Add_Body_With
     (State        : in out State_Type;
      Package_Name : in     String;
      Need_Use     : in     Boolean    := False)
   is begin
      if State.Private_State.Current_Type.Invisible then
         if not Lists.Context_Trees.Is_Present (State.Invisible_Spec_With_List, Package_Name) then
            Lists.Context_Trees.Add (State.Invisible_Body_With_List, (new String'(Package_Name), Need_Use));
            Debug_Put ("add 'with " & Package_Name & "' to invisible body");
         end if;
      else
         if not Lists.Context_Trees.Is_Present (State.Spec_With_List, Package_Name) then
            Lists.Context_Trees.Add (State.Body_With_List, (new String'(Package_Name), Need_Use));
            Debug_Put ("add 'with " & Package_Name & "' to visible body");
         end if;
      end if;
   end Add_Body_With;

   procedure Add_Descriptor
     (State : in out State_Type;
      Label : in     Lists.Type_Labels_Type)
   is
      Replaced : Boolean;
   begin
      if State.Private_State.Private_Type_Completion then
         Replace_Descriptor (State, Label, Replaced);
      else
         declare
            use Lists;
            Descriptor : Type_Descriptor_Type (Label);
         begin
            Descriptor.Type_Name     := State.Private_State.Current_Type_Name;
            Descriptor.Invisible     := Is_Invisible (State, Descriptor.Type_Name);
            Descriptor.Separate_Body := State.Private_State.Separate_Body;

            Type_Descriptor_Lists.Insert_Tail (State.Type_List, Descriptor);

            State.Private_State.Current_Type := Type_Descriptor_Lists.Tail (State.Type_List);

            if Descriptor.Invisible then
               State.Needs_Invisible_Spec := True;
            end if;
         end;
      end if;

      State.Private_State.Current_Type.Private_Implementation := State.Private_State.Private_Type_Completion;

   end Add_Descriptor;

   procedure Add_Private_Descriptor (State : in out State_Type)
   is
      use Lists;
      Descriptor : constant Type_Descriptor_Type :=
                     (Label                  => Lists.Private_Label,
                      Type_Name              => State.Private_State.Current_Type_Name,
                      Invisible              => False,
                      Private_Implementation => False,
                      Separate_Body          => State.Private_State.Separate_Body);
   begin
      Type_Descriptor_Lists.Insert_Tail (State.Type_List, Descriptor);
      Type_Iterator_Trees.Add
        (State.Private_State.Private_Type_Tree, Type_Descriptor_Lists.Last (State.Type_List));
   end Add_Private_Descriptor;

   procedure Add_Record_Descriptor (State : in out State_Type)
   is
      use Auto_Io_Gen.Lists;
      Replaced : Boolean;
   begin
      if State.Private_State.Private_Type_Completion then
         Replace_Descriptor (State, Record_Label, Replaced);
      else
         declare
            use Type_Descriptor_Lists;
            Descriptor : Type_Descriptor_Type (Record_Label);
         begin
            Replaced                          := True;
            Descriptor.Type_Name              := State.Private_State.Current_Type_Name;
            Descriptor.Invisible              := Is_Invisible (State, State.Private_State.Current_Type_Name);
            Descriptor.Private_Implementation := False;
            Descriptor.Separate_Body          := State.Private_State.Separate_Body;

            Insert_Tail (State.Type_List, Descriptor);

            State.Private_State.Current_Type := Tail (State.Type_List);
         end;
      end if;

      if Replaced then
         --  Finish initializing
         declare
            Descriptor : Type_Descriptor_Access_Type renames State.Private_State.Current_Type;
         begin
            --  These may be overridden later
            Descriptor.Record_Constrained           := True;
            Descriptor.Record_Structured_Components := False;
            Descriptor.Record_Tagged                := False;
            Descriptor.Record_Derived               := False;

            if Descriptor.Invisible then
               State.Needs_Invisible_Spec          := True;
               State.Needs_Invisible_Body          := True;
               State.Needs_Invisible_Text_IO_Utils := True;
            else
               State.Needs_Body          := True;
               State.Needs_Text_IO_Utils := True;
            end if;
         end;
      end if;
   end Add_Record_Descriptor;

   procedure Add_Spec_With
     (State        : in out State_Type;
      Package_Name : in     String;
      Invisible    : in     Boolean;
      Need_Use     : in     Boolean    := False)
   is begin
      if Invisible then
         if Lists.Context_Trees.Is_Present (State.Invisible_Body_With_List, Package_Name) then
            Lists.Context_Trees.Delete (State.Invisible_Body_With_List, Package_Name);
         end if;
         Lists.Context_Trees.Add (State.Invisible_Spec_With_List, (new String'(Package_Name), Need_Use));
         Debug_Put ("add 'with " & Package_Name & "' to invisible spec");
      else
         if Lists.Context_Trees.Is_Present (State.Body_With_List, Package_Name) then
            Lists.Context_Trees.Delete (State.Body_With_List, Package_Name);
         end if;
         Lists.Context_Trees.Add (State.Spec_With_List, (new String'(Package_Name), Need_Use));
         Debug_Put ("add 'with " & Package_Name & "' to visible spec");
      end if;
   end Add_Spec_With;

   procedure Add_To_Context
     (State                : in out State_Type;
      Package_Declaration  : in out Asis.Element;
      Type_Declaration     : in     Asis.Element;
      Original_Declaration : in     Asis.Element;
      Array_Role           : in     Array_Role_Type := None)
   is
      pragma Unreferenced (Original_Declaration);
      use Asis, Asis.Elements;

      function To_Lower (Item : in String) return String renames Ada.Characters.Handling.To_Lower;

      Type_Name : constant String := To_Lower (Aux.Name (Type_Declaration));



      procedure Do_Standard_Type
      is begin
         case Array_Role is
         when None =>
            Add_Body_With
              (State,
               Auto_Io_Gen.Options.Current.Standard (Type_Name), Need_Use => True);
         when Index =>
            --  No "with" clause needed
            null;
         when Component  =>
            Add_Spec_With
              (State,
               Auto_Io_Gen.Options.Current.Standard (Type_Name),
               Need_Use  => True,
               Invisible => State.Private_State.Current_Type.Invisible);
         end case;

         Package_Declaration := Nil_Element; -- Standard package

      end Do_Standard_Type;

   begin
      if Is_Equal (Package_Declaration, State.Parent_Package) then
         --  In Ada 95, El is directly visible; no with clauses
         --  needed. In Ada 83, we've already added a "with ; use ;"
         --  clause.
         Package_Declaration := Nil_Element;

      else

         --  Add appropriate package to context clauses.
         --
         --  In Ada 95, the Text_IO child has visibility of the parent
         --  package and it's context clauses, but not in Ada 83.
         --
         --  If El is an array component, it will be instantiated with
         --  SAL.Gen_Array_Text_IO, and we need visibility of the Put
         --  and Get for the component in the spec.
         --
         --  If El is an array index, we need visibility of the type
         --  for instantiation. In Ada 83, this requires a "with"
         --  clause.
         --
         --  For other elements, we only need visibility of Put
         --  and Get in the body.
         --
         --  First, handle (subtypes of) types declared in Standard.

         declare
            Package_Name : constant String := Aux.Name (Package_Declaration);
         begin
            if Package_Name = "Standard" then
               Do_Standard_Type;
            else
               --  Defining package is not predefined.

               case Array_Role is
               when None =>
                  Add_Text_IO_Body_With (State, Package_Declaration);
               when Index =>
                  null;
               when Component =>
                  Add_Text_IO_Spec_With (State, Package_Declaration);
               end case;

            end if;
         end;
      end if;
   end Add_To_Context;

   procedure Annotations
     (Element       : in     Asis.Element;
      Ignore        :    out Boolean;
      Separate_Body :    out Boolean)
   is
      use Asis.Text;
      First_Line        : constant Line_Number := First_Line_Number (Element);
      Annotation_String : constant String := "Auto_Io_Gen";

   begin
      Ignore        := False;
      Separate_Body := False;

      if First_Line <= Line_Number_Positive'First then
         --  When does this happen?
         return;
      else
         declare
            use Ada.Strings.Fixed, Ada.Characters.Handling;
            Element_Lines    : constant Line_List := Lines (Element, First_Line - 1, First_Line);
            Comment          : constant String    :=
                                 To_Lower (To_String (Comment_Image (Element_Lines (Element_Lines'First))));
            Comment_Start    : Natural;
            Non_Blank_Start  : Natural;
            Annotation_Start : Natural;
         begin
            if Comment'Length = 0 then
               return;
            end if;

            Comment_Start   := Index (Comment, "--");
            Non_Blank_Start := Index_Non_Blank (Comment (Comment_Start + 2 .. Comment'Last));

            if Non_Blank_Start = 0 then
               return;
            end if;

            Annotation_Start :=
              Index (Source => Comment (Non_Blank_Start .. Comment'Last), Pattern => Annotation_String);

            if Annotation_Start = 0 then
               return;
            end if;

            if Annotation_Start + Annotation_String'Length - 1 > Comment'Last then
               return;
            end if;

            Non_Blank_Start := Index_Non_Blank (Comment (Annotation_Start + Annotation_String'Length .. Comment'Last));

            if Non_Blank_Start = 0 then
               return;
            end if;

            if Comment (Non_Blank_Start) /= ':' then
               return;
            end if;

            Ignore        := 0 /= Index (Source => Comment (Non_Blank_Start + 1 .. Comment'Last), Pattern => "ignore");
            Separate_Body := 0 /=
              Index (Source => Comment (Non_Blank_Start + 1 .. Comment'Last), Pattern => "separate");
         end;
      end if;
   end Annotations;

   procedure Check_Private_Type_Completion (State : in out State_Type)
   is
      use Lists;
      Name  : constant String := Asis.Aux.Name (State.Private_State.Current_Type_Name);
   begin
      if Type_Iterator_Trees.Is_Present (State.Private_State.Private_Type_Tree, Name) then
         State.Private_State.Private_Type_Iterator := Type_Iterator_Trees.Retrieve
           (State.Private_State.Private_Type_Tree, Name);

         --  Retrieve raises SAL.Not_Found if Name not in tree.

         State.Private_State.Private_Type_Completion := True;

         State.Private_State.Current_Type :=
           Type_Descriptor_Lists.Current (State.Private_State.Private_Type_Iterator);

      else
         State.Private_State.Private_Type_Iterator   := Type_Descriptor_Lists.Null_Iterator;
         State.Private_State.Private_Type_Completion := False;
      end if;

   end Check_Private_Type_Completion;

   procedure Find_Defining_Package
     (State          : in out State_Type;
      El             : in     Asis.Element;
      Result_Package :    out Asis.Element;
      Array_Role     : in     Array_Role_Type := None)
   is
      use Asis, Asis.Elements;
      Result_Declaration : Element;
      Result_Definition  : Element;
      Temp               : Element;
   begin
      Corresponding_Type (El, Result_Definition, Result_Declaration);

      Temp := Enclosing_Element (Result_Declaration);

      case Declaration_Kind (Temp) is
      when A_Generic_Package_Declaration =>
         Result_Package := Temp;

      when A_Package_Declaration =>
         Result_Package := Temp;

         Temp := Enclosing_Element (Temp);
         case Declaration_Kind (Temp) is
         when A_Formal_Package_Declaration =>
            Result_Package := Temp;
         when others =>
            null;
         end case;

      when others =>
         raise Program_Error;

      end case;

      case Declaration_Kind (Result_Package) is
      when A_Formal_Package_Declaration =>
         Add_Text_IO_Spec_With (State, Result_Package);

      when A_Generic_Package_Declaration |
           A_Package_Declaration =>

         if Asis.Aux.Name (Result_Package) = Asis.Aux.Name (State.Parent_Package) then
            --  We still might need to with the other (Private/public)
            --  Text_IO child.
            if State.Private_State.Current_Type.Invisible then
               if not Is_Invisible (State, El) then
                  case Array_Role is
                  when None =>
                     Add_Text_IO_Body_With (State, Result_Package, Need_Use => True);
                  when Index =>
                     null;
                  when Component =>
                     Add_Text_IO_Spec_With (State, Result_Package);
                  end case;
               end if;
            else
               if Is_Invisible (State, El) then
                  case Array_Role is
                  when None =>
                     Add_Body_With (State, Private_Text_IO_Child_Name (Result_Package), Need_Use => True);
                  when Index =>
                     null;
                  when Component =>
                     Add_Spec_With
                       (State,
                        Private_Text_IO_Child_Name (Result_Package),
                        Invisible => State.Private_State.Current_Type.Invisible);
                  end case;
               end if;
            end if;
         end if;

         Add_To_Context
           (State,
            Package_Declaration  => Result_Package,
            Type_Declaration     => Result_Declaration,
            Original_Declaration => El,
            Array_Role           => Array_Role);

      when others =>
         --  Can't get here.
         null;
      end case;
   end Find_Defining_Package;

   function Is_Invisible (State : in State_Type; Element : in Asis.Element) return Boolean
   is begin
      return Lists.Name_Trees.Is_Present (State.Private_State.Invisible_Type_Tree, Asis.Aux.Name (Element));
   end Is_Invisible;

   function Is_Scalar (El : in Asis.Element) return Boolean
   is
      use Asis;
      Corresponding_Definition : constant Asis.Element := Corresponding_Root_Type_Definition (El);
   begin

      case Elements.Element_Kind (Corresponding_Definition) is
      when A_Definition =>

         case Elements.Definition_Kind (Corresponding_Definition) is
         when A_Formal_Type_Definition =>

            case Elements.Formal_Type_Kind (Corresponding_Definition) is
            when A_Formal_Discrete_Type_Definition |
                 A_Formal_Signed_Integer_Type_Definition |
                 A_Formal_Modular_Type_Definition |
                 A_Formal_Floating_Point_Definition =>
               return True;

            when A_Formal_Private_Type_Definition |
                 A_Formal_Constrained_Array_Definition |
                 A_Formal_Unconstrained_Array_Definition =>
               return False;

            when others =>
               raise Not_Supported;

            end case;

         when A_Type_Definition =>

            case Elements.Type_Kind (Corresponding_Definition) is
            when A_Constrained_Array_Definition |
                 An_Unconstrained_Array_Definition =>
               return False;

            when An_Enumeration_Type_Definition |
                 An_Ordinary_Fixed_Point_Definition |
                 A_Decimal_Fixed_Point_Definition |
                 A_Floating_Point_Definition |
                 A_Signed_Integer_Type_Definition |
                 A_Modular_Type_Definition =>
               return True;

            when An_Access_Type_Definition =>
               return True;

            when A_Record_Type_Definition |
                 A_Tagged_Record_Type_Definition =>

               return False;

            when others =>
               raise Not_Supported;

            end case;

         when A_Private_Type_Definition |
              A_Tagged_Private_Type_Definition |
              A_Private_Extension_Definition =>
            return False;

         when others =>
            raise Not_Supported;

         end case;

      when others =>
         raise Not_Supported;

      end case;

   end Is_Scalar;

   procedure Report_Unsupported
     (State   : in out State_Type;
      El      : in Asis.Element;
      Message : in String := "")
   is
      use Ada.Text_IO, Asis.Text, Ada.Strings, Ada.Strings.Fixed;
      El_Span    : constant Span := Element_Span (El);
      --  In GNAT 3.14a and later ASIS implementations, if a
      --  preprocessor is used, Element_Span gives the line numbers in
      --  the intermediate file. Asis.Extensions.Original_Line_Number
      --  converts these to the user's file line numbers, which is
      --  what we want in error messages.
      First_Line : constant Asis.Text.Line_Number := Asis.Extensions.Original_Line_Number (El, El_Span.First_Line);
   begin
      Traceback;
      Put_Line
        (Standard_Error,
         Options.Report_File_Name.all & ":" &
           Trim (Line_Number_Positive'Image (First_Line), Left) & ":" &
           Trim (Character_Position_Positive'Image (El_Span.First_Column), Left) & ": " &
           Program_Name & ": " &
           Message &
           " not supported");

      if Options.Error_On_Warn then
         State.Error_Count := State.Error_Count + 1;
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

   end Report_Unsupported;

   procedure Set_Array_Component
     (State : in out State_Type;
      El    : in     Asis.Element;
      Desc  : in out Lists.Type_Descriptor_Type)
   is
      use Asis;
      Corresponding : Asis.Element;
      Temp          : Asis.Element;
   begin
      if Desc.Label not in Lists.Array_Labels_Type then
         raise Program_Error;
      end if;

      --  Desc.Array_Component_Label must be set based on the ancestor
      --  derived type, so we can tell if it is float, integer, etc.
      --  Desc.Array_Component_Subtype and _Package can be set just
      --  based on El. Desc.Array_Component_Type_Package needs the
      --  type. We could probably do this better ...

      case Elements.Element_Kind (El) is
      when An_Expression =>

         Desc.Array_Component_Subtype := El;

         case Elements.Expression_Kind (El) is
         when An_Identifier =>
            Corresponding := Expressions.Corresponding_Name_Declaration (El);

         when A_Selected_Component =>


            Desc.Array_Component_Subtype := Expressions.Selector (El);

            --  In general, we can't rely on the prefix to be the full
            --  package name, due to child or "use" visibility of some
            --  ancestor package. In addition, a direct "use" clause
            --  means there will be no prefix for this type, so
            --  Find_Defining_Package needs to handle this case
            --  anyway. So we just use Find_Defining_Package, and
            --  ignore the prefix here.

            Corresponding := Expressions.Corresponding_Name_Declaration (Desc.Array_Component_Subtype);

         when others =>
            raise Program_Error;

         end case;

      when A_Declaration =>
         --  This is a recursive call for a derived type. Keep the leaf
         --  type in Desc.Array_Component_Subtype.
         Corresponding := El;

      when others =>
         raise Program_Error;

      end case;

      case Elements.Element_Kind (Corresponding) is
      when A_Declaration =>

         case Elements.Declaration_Kind (Corresponding) is
         when A_Formal_Type_Declaration |
              An_Ordinary_Type_Declaration |
              A_Subtype_Declaration =>

            Corresponding := Declarations.Type_Declaration_View (Corresponding);

            case Elements.Definition_Kind (Corresponding) is
            when A_Formal_Type_Definition =>
               --     Desc.Array_Component_Formal := True;

               Desc.Array_Component_Subtype_Package :=
                 Asis.Elements.Unit_Declaration (Asis.Elements.Enclosing_Compilation_Unit (Corresponding));

               Desc.Array_Component_Type_Package := Desc.Array_Component_Subtype_Package;

               Add_Text_IO_Spec_With (State, Desc.Array_Component_Type_Package);

               case Elements.Formal_Type_Kind (Corresponding) is
               when A_Formal_Discrete_Type_Definition =>
                  Desc.Array_Component_Label := Lists.Enumeration_Label;

               when A_Formal_Signed_Integer_Type_Definition =>
                  Desc.Array_Component_Label := Lists.Signed_Integer_Label;

               when A_Formal_Modular_Type_Definition =>
                  Desc.Array_Component_Label := Lists.Modular_Integer_Label;

               when A_Formal_Floating_Point_Definition =>
                  Desc.Array_Component_Label := Lists.Float_Label;

               when A_Formal_Private_Type_Definition |
                    A_Formal_Constrained_Array_Definition |
                    A_Formal_Unconstrained_Array_Definition =>

                  Desc.Array_Component_Label := Lists.Private_Label;

               when others =>
                  raise Not_Supported;

               end case;

            when A_Type_Definition =>

               Find_Defining_Package
                 (State,
                  Desc.Array_Component_Subtype,
                  Array_Role     => Component,
                  Result_Package => Desc.Array_Component_Subtype_Package);

               Desc.Array_Component_Type_Package := Desc.Array_Component_Subtype_Package;

               case Elements.Type_Kind (Corresponding) is
               when A_Derived_Type_Definition =>
                  Corresponding := Definitions.Corresponding_Root_Type (Corresponding);

                  Set_Array_Component (State, Corresponding, Desc);

               when A_Derived_Record_Extension_Definition =>
                  Desc.Array_Component_Label := Lists.Private_Label;

               when An_Enumeration_Type_Definition =>
                  Desc.Array_Component_Label := Lists.Enumeration_Label;

               when A_Signed_Integer_Type_Definition =>
                  Desc.Array_Component_Label := Lists.Signed_Integer_Label;

               when A_Modular_Type_Definition =>
                  Desc.Array_Component_Label := Lists.Modular_Integer_Label;

               when A_Floating_Point_Definition =>
                  Desc.Array_Component_Label := Lists.Float_Label;

               when A_Constrained_Array_Definition |
                    A_Record_Type_Definition |
                    A_Tagged_Record_Type_Definition |
                    An_Unconstrained_Array_Definition =>

                  Desc.Array_Component_Label := Lists.Private_Label;

               when others =>
                  raise Not_Supported;

               end case;

            when A_Private_Type_Definition |
                 A_Tagged_Private_Type_Definition |
                 A_Private_Extension_Definition =>

               Find_Defining_Package
                 (State,
                  Desc.Array_Component_Subtype,
                  Array_Role     => Component,
                  Result_Package => Desc.Array_Component_Subtype_Package);

               Desc.Array_Component_Type_Package := Desc.Array_Component_Subtype_Package;
               Desc.Array_Component_Label        := Lists.Private_Label;

            when A_Subtype_Indication =>

               --  We don't declare Text_IO routines for subtypes, so
               --  find the type's package. Label is also based on the
               --  type, so we have to recurse.

               Corresponding := Asis.Definitions.Subtype_Mark (Corresponding);
               Temp          := Desc.Array_Component_Subtype_Package;

               Set_Array_Component (State, Corresponding, Desc);

               --  Restore the subtype values to the original.
               Desc.Array_Component_Subtype         := El;
               Desc.Array_Component_Subtype_Package := Temp;

            when others =>
               raise Not_Supported;

            end case;

         when A_Private_Type_Declaration =>
            Find_Defining_Package
              (State,
               Desc.Array_Component_Subtype,
               Array_Role                => Component,
               Result_Package            => Desc.Array_Component_Subtype_Package);

            Desc.Array_Component_Type_Package := Desc.Array_Component_Subtype_Package;
            Desc.Array_Component_Label        := Lists.Private_Label;

         when others =>
            raise Not_Supported;

         end case;

      when others =>
         raise Program_Error;

      end case;

   end Set_Array_Component;

end Auto_Io_Gen.Build.Process_Element_Utils;
