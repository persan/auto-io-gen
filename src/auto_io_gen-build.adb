--  Abstract :
--
--  see spec.
--
--  Design:
--
--  The processing within Process_Element for each state is done by a
--  subprogram declared as a child of Build, to keep Process_Element
--  subprogram shorter, make it easier to find the code for each
--  state, and to reduce recompilation times.
--
--  Utility functions used only by Process_Element and its state
--  processors are declared in Build.Process_Element_Utils
--
--  Copyright (C) 2001 - 2004 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Exceptions;
with Ada.Text_IO;
with Asis.Aux;
with Asis.Elements;
with Asis.Iterator;
with Auto_Io_Gen.Options;

--  State processing subprograms, listed in state order
with Auto_Io_Gen.Build.Process_Element_Do_Initial;
with Auto_Io_Gen.Build.Process_Element_Do_Package;
with Auto_Io_Gen.Build.Process_Element_Do_Formal_Package;
with Auto_Io_Gen.Build.Process_Element_Do_Type;
with Auto_Io_Gen.Build.Process_Element_Do_Array_Type;
with Auto_Io_Gen.Build.Process_Element_Do_Discriminant_Part;
with Auto_Io_Gen.Build.Process_Element_Do_Discriminant;
with Auto_Io_Gen.Build.Process_Element_Do_Record_Type;
with Auto_Io_Gen.Build.Process_Element_Do_Variant_Part;
with Auto_Io_Gen.Build.Process_Element_Do_Component;
with Auto_Io_Gen.Build.Process_Element_Do_Component_Definition;
package body Auto_Io_Gen.Build is

   --------------
   --  Local declarations

   procedure Process_Element
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out State_Type);

   procedure Terminate_Children
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out State_Type);
   --  Finish component list of current type. For
   --  Asis.Iterator.Traverse_Element.Post_Operation

   procedure Inst_Build_Tree is new Asis.Iterator.Traverse_Element
     (State_Information => State_Type,
      Pre_Operation     => Process_Element,
      Post_Operation    => Terminate_Children);
   --  Create, in State_Information, a list of types that need
   --  .Text_IO.Put procedures.

   ----------
   --  Private bodies

   procedure Process_Element
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out State_Type)
   is begin
      Debug_Put
         ("Process_Element.   State " & State_Label_Type'Image (State.Private_State.Label) & " ",
          New_Line => False);

      case State.Private_State.Label is
      when Initial =>
         Build.Process_Element_Do_Initial (Element, Control, State);

      when In_Package =>
         Build.Process_Element_Do_Package (Element, Control, State);

      when In_Formal_Package =>
         Build.Process_Element_Do_Formal_Package (Element, Control, State);

      when In_Type =>
         Build.Process_Element_Do_Type (Element, Control, State);

      when In_Array_Type =>
         Build.Process_Element_Do_Array_Type (Element, Control, State);

      when In_Discriminant_Part =>
         Build.Process_Element_Do_Discriminant_Part (Element, Control, State);

      when In_Discriminant =>
         Build.Process_Element_Do_Discriminant (Element, Control, State);

      when In_Record_Type =>
         Build.Process_Element_Do_Record_Type (Element, Control, State);

      when In_Variant_Part =>
         Build.Process_Element_Do_Variant_Part (Element, Control, State);

      when In_Component =>
         Build.Process_Element_Do_Component (Element, Control, State);

      when In_Component_Definition =>
         Build.Process_Element_Do_Component_Definition (Element, Control, State);

      end case;

      Debug_Put ("                   Control => " & Asis.Traverse_Control'Image (Control));

   exception
      --  Don't catch ASIS_Failed here, so Diagnosis is set properly.
      --
      --  Not_Supported should be handled locally, not here

   when E : Program_Error =>
      --  ASIS iterator maps any exception to ASIS_Failed, and our top
      --  level driver assumes an error message is output for that at
      --  the raise point, so put an error message now.
      Ada.Text_IO.Put_Line
         (Ada.Text_IO.Standard_Error,
          "Exception " & Ada.Exceptions.Exception_Name (E) & " for " & Asis.Aux.Image (Element));
      raise;

   end Process_Element;

   procedure Terminate_Children
     (Element : in     Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out State_Type)
   is
      use Asis;
   begin
      Debug_Put
         ("Terminate_Children." &
          " State " & State_Label_Type'Image (State.Private_State.Label) &
          " Element " & Aux.Image (Element));

      case State.Private_State.Label is
      when Initial =>
         raise Program_Error;

      when In_Package =>
         null; --  No more types

      when In_Formal_Package =>
         State.Private_State.Label := In_Package;

      when In_Type | In_Array_Type | In_Record_Type =>

         if State.Private_State.Abandon_Current_Type then
            Control                   := Abandon_Siblings;
            State.Private_State.Label := In_Package;
         else
            case Elements.Element_Kind (Element) is
            when A_Declaration =>
               case Elements.Declaration_Kind (Element) is
               when A_Formal_Type_Declaration |
                 An_Ordinary_Type_Declaration |
                 A_Private_Extension_Declaration |
                 A_Private_Type_Declaration =>

                  State.Private_State.Label := In_Package;

               when others =>
                  --  Continue with siblings
                  null;

               end case;

            when others =>
               --  Continue with siblings
               null;

            end case;
         end if;

      when In_Discriminant_Part =>
         if State.Private_State.Abandon_Current_Type then
            Control                   := Abandon_Siblings;
            State.Private_State.Label := In_Package;
         else
            case Elements.Element_Kind (Element) is
            when A_Declaration =>
               case Elements.Declaration_Kind (Element) is
               when An_Ordinary_Type_Declaration |
                 A_Private_Type_Declaration =>

                  State.Private_State.Label := In_Package;

               when others =>
                  --  Continue with siblings
                  null;

               end case;

            when others =>
               --  Continue with siblings
               null;

            end case;
         end if;

      when In_Discriminant =>
         if State.Private_State.Abandon_Current_Type then
            Control                   := Abandon_Siblings;
            State.Private_State.Label := In_Discriminant_Part;
         else
            case Elements.Element_Kind (Element) is
            when A_Declaration =>
               case Elements.Declaration_Kind (Element) is
               when A_Discriminant_Specification =>

                  --  Find next discriminant.
                  State.Private_State.Label := In_Discriminant_Part;

               when others =>
                  --  Continue with siblings
                  null;

               end case;

            when others =>
               --  Continue with siblings
               null;

            end case;
         end if;

      when In_Variant_Part =>
         if State.Private_State.Abandon_Current_Type then
            Control                   := Abandon_Siblings;
            State.Private_State.Label := In_Record_Type;
         else
            case Elements.Element_Kind (Element) is
            when A_Definition =>
               case Elements.Definition_Kind (Element) is
               when A_Variant_Part =>
                  State.Private_State.Label := In_Record_Type;

               when others =>
                  --  Continue with siblings
                  null;

               end case;
            when others =>
               --  Continue with siblings
               null;

            end case;
         end if;

      when In_Component =>

         if State.Private_State.Abandon_Current_Type then
            Control                   := Abandon_Siblings;
            State.Private_State.Label := In_Record_Type;
         else
            case Elements.Element_Kind (Element) is
            when A_Declaration =>
               case Elements.Declaration_Kind (Element) is
               when A_Component_Declaration =>
                  State.Private_State.Label := In_Record_Type;

               when others =>
                  --  Continue with siblings
                  null;

               end case;
            when others =>
               --  Continue with siblings
               null;

            end case;
         end if;

      when In_Component_Definition =>

         if State.Private_State.Abandon_Current_Type then
            Control                   := Abandon_Siblings;
            State.Private_State.Label := In_Component;
         else
            case Elements.Element_Kind (Element) is
            when A_Definition =>
               case Elements.Definition_Kind (Element) is
               when A_Component_Definition =>
                  State.Private_State.Label := In_Component;

                  --  Siblings of the type are initializations, which we
                  --  don't care about.
                  Control := Abandon_Siblings;

               when others =>
                  --  Continue with siblings
                  null;

               end case;
            when others =>
               --  Continue with siblings
               null;

            end case;
         end if;

      end case;

      Debug_Put ("           Control " & Traverse_Control'Image (Control));

   end Terminate_Children;

   ----------
   --  Public bodies

   procedure Build_Tree
      (Element : in     Asis.Element;
       Control : in out Asis.Traverse_Control;
       State   : in out State_Type)
   is begin
      State.Error_Count         := 0;
      State.Private_State.Label := Initial;

      Inst_Build_Tree (Element, Control, State);
   end Build_Tree;

   -----------
   --  Utilities visible to child subprograms (alphabetical).

   procedure Debug_Put (Element : in Asis.Element; Action : in Element_Action_Type)
   is begin
      if Options.Debug then
         Ada.Text_IO.Put_Line (Element_Action_Type'Image (Action) & " " & Asis.Aux.Image (Element));
      end if;
   end Debug_Put;

   procedure Debug_Put
      (Message  : in String;
       New_Line : in Boolean := True)
   is begin
      if Auto_Io_Gen.Options.Debug then
         if New_Line then
            Ada.Text_IO.Put_Line (Message);
         else
            Ada.Text_IO.Put (Message);
         end if;
      end if;
   end Debug_Put;

end Auto_Io_Gen.Build;
