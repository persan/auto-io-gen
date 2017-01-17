--  Abstract :
--
--  Utility functions for Build.Process_Element_Do_*. These could be
--  in Build, but separating them out saves recompilation time when
--  they change.
--
--  Copyright (C) 2001 - 2005 Stephen Leake.  All Rights Reserved.
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
--
private package Auto_Io_Gen.Build.Process_Element_Utils is
   pragma Elaborate_Body; --  parent is

   type Array_Role_Type is (None, Index, Component);

   procedure Add_Descriptor
     (State : in out State_Type;
      Label : in     Lists.Type_Labels_Type);
   --  If State.Private_State.Private_Type_Completion, replace current
   --  descriptor with one for Label. Otherwise, add a new descriptor
   --  for Label.

   procedure Add_Private_Descriptor (State : in out State_Type);
   --  Add a Private_Label descriptor to State.Type_List, using
   --  State.Private_State.Current_Type_Name. Also add it to
   --  State.Private_State.Discriminated_Private_Type_Tree.

   procedure Add_Record_Descriptor (State : in out State_Type);
   --  If State.Private_State.Private_Type_Completion, replace current
   --  descriptor with one for Record_Label. Otherwise, add a new descriptor
   --  for Label.
   --  Add a new Record_Label descriptor for
   --  State.Private_State.Current_Type_Name to State.Type_List.
   --
   --  Record_Element is assumed to be a record type declaration or
   --  definition.

   procedure Add_Spec_With
     (State        : in out State_Type;
      Package_Name : in     String;
      Invisible    : in     Boolean;
      Need_Use     : in     Boolean    := False);
   procedure Add_Body_With
     (State        : in out State_Type;
      Package_Name : in     String;
      Need_Use     : in     Boolean    := False);
   --  Add Package_Name to the appropriate 'with' list. Add_Body_With
   --  doesn't add Package_Name if it is already in the spec list.

   procedure Add_To_Context
     (State                : in out State_Type;
      Package_Declaration  : in out Asis.Element;
      Type_Declaration     : in     Asis.Element;
      Original_Declaration : in     Asis.Element;
      Array_Role           : in     Array_Role_Type := None);
   --  Add Package_Declaration and corresponding Text_IO child to
   --  appropriate lists in state. Package_Declaration must contain
   --  Type_Declaration; that is checked to see if it is a predefined
   --  type, which has a different Text_IO package naming convention.
   --  Package_Declaration is set to Nil_Element if it is
   --  State.Parent_Package or Standard, since those are directly
   --  visible. Array_Role determines whether the "with" is on
   --  the spec or the body.
   --
   --  Original_Declaration is used to build an error message if
   --  Type_Declaration is not supported; Original_Declaration must be
   --  from State.Parent_Package.

   procedure Annotations
     (Element       : in     Asis.Element;
      Ignore        :    out Boolean;
      Separate_Body :    out Boolean);
   --  Check for comment lines preceding Element starting with "--
   --  Auto_Io_Gen : " (ignoring whitespace). The word following the
   --  colon is the annotation.
   --
   --  Ignore is True if annotation is "ignore".
   --  Separate_Body is True if annotation is "separate".

   procedure Check_Private_Type_Completion (State : in out State_Type);
   --  Search for State.Private_State.Current_Type_Name in
   --  State.Private_State.Private_Type_List. If present, retrieve
   --  descriptor pointer into State.Private_State.Current_Type, set
   --  State.Private_State.Private_Type_Completion True.
   --
   --  Also set State.Private_State.Current_Type.Invisible.

   procedure Find_Defining_Package
     (State          : in out State_Type;
      El             : in     Asis.Element;
      Result_Package :    out Asis.Element;
      Array_Role     : in     Array_Role_Type := None);
   --  El must be An_Identifier from a type name in a declaration; set
   --  Result_Package to package where El is declared (An_Expression),
   --  Not_An_Element if that is State.Parent_Package or Standard.
   --
   --  As a side effect, add appropriate packages to various lists in
   --  State.
   --
   --  Array_Role should be Index or Component if El is the type of an
   --  array index or component that will be used in a
   --  SAL.Gen_Array_Text_IO instantiation; this determines whether
   --  the corresponding packages must be visible in the spec or body.
   --
   --  Prints an error message and raises Not_Supported for any
   --  unsupported elements.

   function Is_Invisible (State : in State_Type; Element : in Asis.Element) return Boolean;
   --  True if Name (Element) is in State.Private_State.Invisible_Type_Tree

   function Is_Scalar (El : in Asis.Element) return Boolean;
   --  El must be An_Identifier or A_Selected_Component from a type
   --  name. Return True if the corresponding root type is a scalar
   --  type.
   --
   --  Raises Not_Supported if El is not supported; caller should
   --  output an error message.

   procedure Report_Unsupported
     (State   : in out State_Type;
      El      : in     Asis.Element;
      Message : in     String       := "");
   --  El must be a declaration in State.Parent_Package. Write file
   --  name, line & column number, Message & "not supported" to
   --  Standard_Error. If Options.Error_On_Warn, set exit status to
   --  Failure, and increment State.Error_Count.

   procedure Set_Array_Component
     (State : in out State_Type;
      El    : in     Asis.Element;
      Desc  : in out Lists.Type_Descriptor_Type);
   --  Set Desc.Array_Component_* for El.
   --
   --  Desc.Label must be Array_Type_Labels_Type.
   --
   --  El must be An_Identifier or A_Selected_Component from the array
   --  component type name in an array type declaration.
   --
   --  If A_Selected_Component, add Text_IO child for the package to
   --  the appropriate package list in State.
   --
   --  Raises Not_Supported if El is not supported; caller should
   --  output an error message.

end Auto_Io_Gen.Build.Process_Element_Utils;
