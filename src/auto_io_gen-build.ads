--  Abstract :
--
--  Build tree of types in File_Name needing .Text_IO.Put, using ASIS.
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

with Asis;
with Auto_Io_Gen.Lists;
package Auto_Io_Gen.Build is
--   pragma Elaborate_Body; -- body depends on Asis, but this is circular due to child packages.

   type Private_State_Type is limited private;

   type State_Type is limited record
      Error_Count                   : Integer := 0;
      Type_List                     : Lists.Type_Descriptor_Lists.List_Type;
      Parent_Package                : Asis.Element;
      Needs_Body                    : Boolean := False;
      Needs_Text_IO_Utils           : Boolean := False;
      Needs_Invisible_Spec          : Boolean := False;
      Needs_Invisible_Body          : Boolean := False;
      Needs_Invisible_Text_IO_Utils : Boolean := False;
      Is_Generic                    : Boolean := False;
      Spec_With_List                : Lists.Context_Trees.Tree_Type;
      Body_With_List                : Lists.Context_Trees.Tree_Type;
      Invisible_Spec_With_List      : Lists.Context_Trees.Tree_Type;
      Invisible_Body_With_List      : Lists.Context_Trees.Tree_Type;

      Formal_Package_List : Lists.Formal_Package_Lists.List_Type;
      --  List of A_Generic_Package_Declaration (ancestors of parent
      --  generic package) or A_Formal_Package_Declaration (parameter
      --  of parent generic package). Used to build generic formal
      --  package list for child. Empty if parent is not generic.

      --  Public parts of this structure are shared between Build and
      --  Generate. Private parts are used only within Build.
      Private_State : Private_State_Type;
   end record;

   procedure Build_Tree
      (Element : in     Asis.Element;
       Control : in out Asis.Traverse_Control;
       State   : in out State_Type);
   --  Asis context must be initialized and open. Build Tree in state;
   --  Element must be the Unit_Declaration that needs a child Text_IO
   --  package.
   --
   --  State.Error_Count reports the number of errors encountered
   --  (such as unsupported constructs).

private

   type State_Label_Type is
      (Initial,
       In_Package,
       In_Formal_Package,
       In_Type,
       In_Array_Type,
       In_Discriminant_Part,
       In_Discriminant,
       In_Record_Type,
       In_Variant_Part,
       In_Component,
       In_Component_Definition);

   type Private_State_Type is limited record
      Label         : State_Label_Type := Initial;
      Separate_Body : Boolean;

      --  We get the type name before we know the type label, so we
      --  need a place to store it.
      Current_Type_Name : Asis.Element;
      Current_Type      : Lists.Type_Descriptor_Access_Type;
      Current_Component : Lists.Component_Type;
      Current_Variant   : Lists.Variant_Access_Type;

      Private_Type_Completion : Boolean;
      --  True if Current_Type is the completion of a private type declaration.

      Private_Type_Iterator : Lists.Type_Descriptor_Lists.Iterator_Type;
      --  If Private_Type_Completion, points to the private type
      --  descriptor in Type_List, so we can replace it.

      Abandon_Current_Type : Boolean;
      --  Used to get back to In_Package state when a type turns out
      --  to be unsupported.

      Current_Formal_Package : Lists.Formal_Package_Lists.Iterator_Type;
      --  Used to add instantiation arguments

      Private_Type_Tree : Lists.Type_Iterator_Trees.Tree_Type;
      --  List of private types sorted by name. A Private_Label or
      --  Record_Label descriptor (if the public part has
      --  discriminants) is created in Type_List when the public
      --  declaration is processed, and replaced by the appropriate
      --  descriptor when the full type is processed.

      Invisible_Type_Tree : Lists.Name_Trees.Tree_Type;
      --  List of types declared in private part.

   end record;

   ----------
   --  Utilities visible to child subprograms. Most utilities are in
   --  Build.Process_Element_Utils, to reduce recompilation time.

   type Element_Action_Type is (Processing, Skipping, Ignoring);

   procedure Debug_Put (Element : in Asis.Element; Action : in Element_Action_Type);
   --  If Options.Debug, put an appropriate message to Current_Output

   procedure Debug_Put
      (Message  : in String;
       New_Line : in Boolean := True);
   --  if Options.Debug, write Message to Current_Output.

end Auto_Io_Gen.Build;
