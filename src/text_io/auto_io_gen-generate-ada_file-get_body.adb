--  Abstract :
--
--  See spec.
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

with Ada.Characters.Handling;
with Asis.Aux;
with Asis.Declarations;
with Asis.Elements;
with Auto_Io_Gen.Options;
with SAL.Gen.Alg.Process_All_Constant;
package body Auto_Io_Gen.Generate.Ada_File.Get_Body is
   use Ada.Text_IO;

   Body_First : Boolean := True; -- Shared between printing discriminants and components.

   procedure Generate_Discriminant_Declare
     (File         : in Ada.Text_IO.File_Type;
      Discriminant : in Auto_Io_Gen.Lists.Component_Type);
   --  Generate body code to declare a local temp for one discriminant.

   procedure Generate_Discriminant_Get
     (File         : in Ada.Text_IO.File_Type;
      Discriminant : in Auto_Io_Gen.Lists.Component_Type);
   --  Generate body code to get one discriminant into a local temp.

   procedure Generate_Discriminant_Compare
     (File         : in Ada.Text_IO.File_Type;
      Discriminant : in Auto_Io_Gen.Lists.Component_Type);
   --  Generate body code to compare one discriminant.

   procedure Generate_Component_Temp_Get
     (File      : in Ada.Text_IO.File_Type;
      Component : in Auto_Io_Gen.Lists.Component_Type);
   --  Generate body code to get one component into Temp_Item.

   procedure Generate_Derived_Array
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate body code for all Get subprograms for a derived array type.

   procedure Generate_Record
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Record_Type_Descriptor_Type);
   --  Generate body code for all Get subprograms for a record type.

   procedure Generate_Private_Array_Wrapper
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate body code for all Put subprograms in the public child
   --  for a private array type.

   procedure Generate_Access
      (File            : in Ada.Text_IO.File_Type;
       Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type);
   --  Generate body code for all Get subprograms for an access type.

   procedure Generate
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is begin
      if Options.Debug then
         Put_Line ("Generate.Get_Body. " & Lists.Type_Name (Type_Descriptor));
      end if;

      case Type_Descriptor.Label is
      when Lists.Array_Labels_Type =>
         if Type_Descriptor.Private_Implementation then
            Generate_Private_Array_Wrapper (File, Type_Descriptor);
         else
            --  No body needed; SAL.Gen_Array_Text_IO instantiated in spec.
            null;
         end if;

      when Lists.Scalar_Labels_Type =>
         --  No body needed; Ada.Text_IO instantiated in spec.
         null;

      when Lists.Record_Label =>
         Generate_Record (File, Type_Descriptor);

      when Lists.Derived_Label =>
         case Type_Descriptor.Derived_Root_Label is
         when Lists.Array_Labels_Type =>
            Generate_Derived_Array (File, Type_Descriptor);

         when Lists.Record_Label =>
            raise SAL.Not_Implemented;
         end case;

      when Lists.Private_Label =>
         --  These should have been replaced by a specific label when
         --  the full type declaration was processed.
         raise Program_Error;

      when Lists.Access_Label =>
         Generate_Access (File, Type_Descriptor);
      end case;
   end Generate;

   procedure Generate_Component_Temp_Get
     (File      : in Ada.Text_IO.File_Type;
      Component : in Auto_Io_Gen.Lists.Component_Type)
   is
      use Ada.Characters.Handling;

      Component_Name : constant String := To_String (Asis.Declarations.Defining_Name_Image (Component.Component_Name));

   begin

      if Body_First then
         Body_First := False;
      else
         --  Finish previous component get
         Indent_Line (File, "Check (File, "","");");

         --  Start current component get
         Indent (File, "Skip_Whitespace (File);");
      end if;

      Indent_Line
        (File,
         "if Named_Association_Record then Check (File, """ & Component_Name & " => ""); end if;");

      if Asis.Elements.Is_Nil (Component.Type_Package) then
         if Component.Invisible then
            Indent (File, "Private_Text_IO.");
         else
            null;
         end if;
      else
         Indent (File, Text_IO_Child_Name (Component.Type_Package) & '.');
      end if;

      if Component.Scalar then
         Indent_Line (File, "Get (File, Temp_Item." & Component_Name & ");");
      else
         Indent_Line (File, "Get_Item (File, Temp_Item." & Component_Name & ",");
         Indent_More (File, "Named_Association => Named_Association_Component);");
      end if;

   end Generate_Component_Temp_Get;

   procedure Generate_Derived_Array
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is begin
      Indent_Line (File, "procedure Get");
      Indent_Level := Indent_Level + 1;

      Indent_Line (File, "(File                      : in     " & Ada_Text_IO & ".File_Type;",
                         " Item                      :    out " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Named_Association_Array   : in     Boolean := False;",
                         " Named_Association_Element : in     Boolean := False)");

      Indent_Less (File, "is begin");

      if Asis.Elements.Is_Nil (Type_Descriptor.Derived_Root_Package_Declaration) then
         Indent_Line (File, "Get");

         Indent_Line
           (File,
            " (File, " &
              Asis.Aux.Name (Type_Descriptor.Derived_Root_Type_Declaration) &
              " (Item),");
      else
         Indent_Line
           (File,
            Asis.Aux.Name (Type_Descriptor.Derived_Root_Package_Declaration) &
              ".Text_IO.Get");

         Indent_Line
           (File,
            " (File, " &
              Asis.Aux.Name (Type_Descriptor.Derived_Root_Package_Declaration) &
              "." &
              Asis.Aux.Name (Type_Descriptor.Derived_Root_Type_Declaration) &
              " (Item),");
      end if;

      Indent_Line
        (File,
         "Named_Association_Array, Named_Association_Element);");

      Indent_Level := Indent_Level - 1;
      Indent_Line (File, "end Get;");
      New_Line (File);

      Indent_Incr (File, "procedure Get");
      Indent_Line (File, "(Item                      :    out " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Named_Association_Array   : in     Boolean := False;",
                         " Named_Association_Element : in     Boolean := False)");
      Indent_Less (File, "is begin");
      Indent_Line (File, "Get (Current_Input, Item, Named_Association_Array, Named_Association_Element);");
      Indent_Decr (File, "end Get;");
      New_Line (File);

      Indent_Incr (File, "procedure Get_Item");
      Indent_Line (File, "(File              : in     " & Ada_Text_IO & ".File_Type;",
                         " Item              :    out " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Named_Association : in     Boolean := False)");
      Indent_Less (File, "is begin");
      Indent_Line (File, "Get (File, Item, Named_Association, Named_Association);");
      Indent_Decr (File, "end Get_Item;");
      New_Line (File);
   end Generate_Derived_Array;

   procedure Generate_Discriminant_Declare
     (File         : in Ada.Text_IO.File_Type;
      Discriminant : in Auto_Io_Gen.Lists.Component_Type)
   is
      use Asis.Aux;
   begin

      Indent (File, Name (Discriminant.Component_Name) & " : ");

      if not Asis.Elements.Is_Nil (Discriminant.Type_Package) then
         Put (File, Name (Discriminant.Type_Package) & ".");
      end if;

      Put_Line (File, Name (Discriminant.Type_Name) & ";");

   end Generate_Discriminant_Declare;

   procedure Generate_Discriminant_Get
     (File         : in Ada.Text_IO.File_Type;
      Discriminant : in Auto_Io_Gen.Lists.Component_Type)
   is
      Discriminant_Name : constant String := Asis.Aux.Name (Discriminant.Component_Name);
   begin

      if Body_First then
         Body_First := False;
      else
         --  Finish previous get
         Indent_Line (File, "Check (File, "","");");

         --  Start current get
         Indent (File, "Skip_Whitespace (File);");
      end if;

      Indent_Line
        (File,
         "if Named_Association_Record then Check (File, """ & Discriminant_Name & " => ""); end if;");

      if not Asis.Elements.Is_Nil (Discriminant.Type_Package) then
         Indent (File, Text_IO_Child_Name (Discriminant.Type_Package) & '.');
      end if;

      Indent_Line (File, "Get (File, " & Discriminant_Name & ");");

   end Generate_Discriminant_Get;

   procedure Generate_Discriminant_Compare
     (File         : in Ada.Text_IO.File_Type;
      Discriminant : in Auto_Io_Gen.Lists.Component_Type)
   is
      use Asis.Aux;
      use type Asis.Element_Kinds;

      Discriminant_Name : constant String := Name (Discriminant.Component_Name);
   begin

      if Asis.Elements.Element_Kind (Discriminant.Type_Package) = Asis.Not_An_Element then
         Indent_Line (File, "if " & Discriminant_Name & " /= Item." & Discriminant_Name & " then");
      else
         Indent_Line
           (File,
            "if " &
              Name (Discriminant.Type_Package) &
              ".""/="" (" &
              Discriminant_Name &
              ", Item." &
              Discriminant_Name &
              ") then");
      end if;
      Indent_More (File, "raise Discriminant_Error;");
      Indent_Line (File, "end if;");

   end Generate_Discriminant_Compare;

   procedure Generate_Private_Array_Wrapper
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is
      Package_Name : constant String := Instantiated_Package_Name (Asis.Aux.Name (Type_Descriptor.Type_Name));
   begin
      --  Generic_Array_IO was instantiated by Put_Body.

      Indent_Incr (File, "procedure Get");

      Indent_Line (File, "(File                        : in " & Ada_Text_IO & ".File_Type;",
                         " Item                        :    out " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Named_Association_Record    : in     Boolean := False;",
                         " Named_Association_Component : in     Boolean := False)");

      Indent_Level := Indent_Level - 1;

      if Type_Descriptor.Array_Component_Label in Lists.Scalar_Array_Component_Labels_Type then
         Indent_Line (File, "is");
         Indent_Line (File, "   pragma Unreferenced (Named_Association_Component);");
         Indent_Line (File, "begin");
      else
         Indent_Line (File, "is begin");
      end if;

      Indent_Level := Indent_Level + 1;

      Indent (File, Package_Name & ".Get");
      Indent_Line (File, " (File, Item,");

      if Type_Descriptor.Array_Component_Label in Lists.Scalar_Array_Component_Labels_Type then
         Indent_Line (File, " Named_Association => Named_Association_Record);");
      else
         Indent_Line (File, " Named_Association_Record, Named_Association_Component);");
      end if;

      Indent_Decr (File, "end Get;");
      New_Line (File);

      Indent_Line (File, "procedure Get");
      Indent_Level := Indent_Level + 1;
      Indent_Line (File, "(Item                        :    out " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Named_Association_Record    : in     Boolean := False;",
                         " Named_Association_Component : in     Boolean := False)");
      Indent_Level := Indent_Level - 1;

      if Type_Descriptor.Array_Component_Label in Lists.Scalar_Array_Component_Labels_Type then
         Indent_Line (File, "is");
         Indent_More (File, "pragma Unreferenced (Named_Association_Component);");
         Indent_Line (File, "begin");
      else
         Indent_Line (File, "is begin");
      end if;

      Indent_Level := Indent_Level + 1;
      Indent_Line (File, Package_Name & ".Get (Current_Input, Item,");
      if Type_Descriptor.Array_Component_Label in Lists.Scalar_Array_Component_Labels_Type then
         Indent_Line (File, " Named_Association => Named_Association_Record);");
      else
         Indent_Line (File, " Named_Association_Record, Named_Association_Component);");
      end if;
      Indent_Decr (File, "end Get;");
      New_Line (File);

      Indent_Line (File, "procedure Get_Item");
      Indent_Level := Indent_Level + 1;
      Indent_Line (File, "(File              : in     " & Ada_Text_IO & ".File_Type;",
                         " Item              :    out " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Named_Association : in     Boolean := False)");
      Indent_Less (File, "is begin");
      Indent_Line (File, Package_Name & ".Get_Item (File, Item, Named_Association);");
      Indent_Decr (File, "end Get_Item;");
      New_Line (File);
   end Generate_Private_Array_Wrapper;

   procedure Generate_Record
     (File            : in Ada.Text_IO.File_Type;
      Type_Descriptor : in Auto_Io_Gen.Lists.Record_Type_Descriptor_Type)
   is
      Separate_Body_Name : constant String := "Get_" & Root_Type_Name (Asis.Aux.Name (Type_Descriptor.Type_Name));

      procedure Print_Parameter_List
      is begin
         Indent_Level := Indent_Level + 1;

         Indent_Line (File, "(File                        : in     " & Ada_Text_IO & ".File_Type;",
                            " Item                        :    out " & Lists.Type_Name (Type_Descriptor) & ";",
                            " Named_Association_Record    : in     Boolean := False;",
                            " Named_Association_Component : in     Boolean := False)");

         Indent_Level := Indent_Level - 1;
      end Print_Parameter_List;

      procedure Print_Discriminant_Declare
        (Component : in Auto_Io_Gen.Lists.Component_Type;
         First     : in Boolean                          := True)
      is
         pragma Unreferenced (First);
      begin
         Generate_Discriminant_Declare (File, Component);
      end Print_Discriminant_Declare;

      procedure Print_Discriminant_Declares is new Auto_Io_Gen.Lists.Component_Algs.Process_All_Constant
        (Process_Item => Print_Discriminant_Declare);

      procedure Print_Discriminant
        (Discriminant : in Auto_Io_Gen.Lists.Component_Type;
         First        : in Boolean                          := True)
      is begin
         if First then
            Put (File, Asis.Aux.Name (Discriminant.Component_Name));
         else
            Put_Line (File, ",");
            Indent (File, Asis.Aux.Name (Discriminant.Component_Name));
         end if;

      end Print_Discriminant;

      procedure Print_Discriminants is new Auto_Io_Gen.Lists.Component_Algs.Process_All_Constant
        (Process_Item => Print_Discriminant);

      procedure Print_Discriminant_Compare
        (Component : in Auto_Io_Gen.Lists.Component_Type;
         First     : in Boolean                          := True)
      is
         pragma Unreferenced (First);
      begin
         Generate_Discriminant_Compare (File, Component);
      end Print_Discriminant_Compare;

      procedure Print_Discriminant_Compares is new Auto_Io_Gen.Lists.Component_Algs.Process_All_Constant
        (Process_Item => Print_Discriminant_Compare);

      procedure Print_Discriminant_Get
        (Component : in Auto_Io_Gen.Lists.Component_Type;
         First     : in Boolean                          := True)
      is
         pragma Unreferenced (First);
      begin
         Generate_Discriminant_Get (File, Component);
      end Print_Discriminant_Get;

      procedure Print_Discriminant_Gets is new Auto_Io_Gen.Lists.Component_Algs.Process_All_Constant
        (Process_Item => Print_Discriminant_Get);

      procedure Print_Component_Get
        (Component : in Auto_Io_Gen.Lists.Component_Type;
         First     : in Boolean                          := True)
      is
         pragma Unreferenced (First);
      begin
         Generate_Component_Temp_Get (File, Component);
      end Print_Component_Get;

      procedure Print_Component_Gets is new Auto_Io_Gen.Lists.Component_Algs.Process_All_Constant
        (Process_Item => Print_Component_Get);

      procedure Print_Variant
        (Variant   : in Lists.Variant_Access_Type;
         First     : in Boolean)
      is
         pragma Unreferenced (First);
      begin
         Indent_Line (File, "when " & Asis.Aux.Name (Variant.Choice) & " =>");

         Indent_Level := Indent_Level + 1;

         if Lists.Length (Variant.Components) = 0 then
            Indent_Line (File, "null;");
         else
            Print_Component_Gets (Variant.Components);
         end if;

         Indent_Level := Indent_Level - 1;
      end Print_Variant;

      procedure Print_Variants is new Lists.Variant_Algs.Process_All_Constant (Process_Item => Print_Variant);

      procedure Print_Variant_Part
      is begin
         if Lists.Length (Type_Descriptor.Record_Variant_Part.Variants) = 0 then
            return;
         end if;

         --  Discriminant was read with other discriminants; only
         --  need here to generate case statement.
         Indent_Line
           (File, "case Temp_Item." & Asis.Aux.Name (Type_Descriptor.Record_Variant_Part.Discriminant) & " is");

         Print_Variants (Type_Descriptor.Record_Variant_Part.Variants);

         Indent_Line (File, "end case;");
      end Print_Variant_Part;

   begin
      if Type_Descriptor.Separate_Body then
         Indent_Line (File, "procedure " & Separate_Body_Name);
         Print_Parameter_List;
         Indent_Line (File, "is separate;");
      end if;

      Indent_Line (File, "procedure Get");
      Print_Parameter_List;

      if Type_Descriptor.Separate_Body then
         Indent_Line (File, "renames " & Separate_Body_Name & ";");
      else
         Indent_Line (File, "is");
         Indent_Level := Indent_Level + 1;

         if not Type_Descriptor.Record_Structured_Components then
            Indent_Line (File, "pragma Unreferenced (Named_Association_Component);");
         end if;

         Print_Discriminant_Declares (Type_Descriptor.Record_Discriminants);

         if Type_Descriptor.Record_Constrained then
            Indent_Line (File, "Temp_Item : " & Asis.Aux.Name (Type_Descriptor.Type_Name) & " renames Item;");
         end if;

         Indent_Less (File, "begin");

         Body_First := True;

         Indent_Line (File, "Check (File, ""("");");

         if Lists.Length (Type_Descriptor.Record_Discriminants) > 0 then
            Print_Discriminant_Gets (Type_Descriptor.Record_Discriminants);
         end if;

         if Type_Descriptor.Record_Constrained then
            Print_Discriminant_Compares (Type_Descriptor.Record_Discriminants);
         else
            Indent_Incr (File, "declare");
            Indent_Line (File, "Temp_Item : " & Asis.Aux.Name (Type_Descriptor.Type_Name));
            Indent_Level := Indent_Level + 1;
            Indent (File, "(");
            Print_Discriminants (Type_Descriptor.Record_Discriminants);
            Put_Line (File, ");");
            Indent_Level := Indent_Level - 1;
            Indent_Less (File, "begin");
         end if;

         if Type_Descriptor.Record_Derived then
            if Body_First then
               Body_First := False;
            else
               --  Finish discriminant get
               Indent_Line (File, "Check (File, "","");");
               Indent (File, "Skip_Whitespace (File);");
            end if;

            Indent_Line
              (File,
               Asis.Aux.Name (Type_Descriptor.Record_Parent_Package_Name) &
                 ".Text_IO.Get_Components");
            Indent_Level := Indent_Level + 1;
            Indent_Line
              (File,
               " (File, " &
                 Asis.Aux.Name (Type_Descriptor.Record_Parent_Package_Name) &
                 "." &
                 Asis.Aux.Name (Type_Descriptor.Record_Parent_Type_Name) &
                 " (Temp_Item),",
               "Named_Association_Record, Named_Association_Component);");
            Indent_Level := Indent_Level - 1;
         end if;

         if Type_Descriptor.Record_Tagged then
            if Body_First then
               Body_First := False;
            else
               --  Finish discriminant get
               Indent_Line (File, "Check (File, "","");");
               Indent (File, "Skip_Whitespace (File);");
            end if;
            Indent_Line
              (File,
               "Get_Components (File, Temp_Item, Named_Association_Record, Named_Association_Component);");
         else
            Print_Component_Gets (Type_Descriptor.Record_Components);
            Print_Variant_Part;
         end if;

         if not Type_Descriptor.Record_Constrained then
            Indent_Line (File, "Item := Temp_Item;");
            Indent_Decr (File, "end;");
         end if;

         Indent_Line (File, "Check (File, "")"");");
         Indent_Decr (File, "end Get;");
         New_Line (File);

      end if;

      Indent_Incr (File, "procedure Get");
      Indent_Line (File, "(Item                        :    out " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Named_Association_Record    : in     Boolean := False;",
                         " Named_Association_Component : in     Boolean := False)");
      Indent_Less (File, "is begin");
      Indent_Line (File, "Get (Current_Input, Item, Named_Association_Record, Named_Association_Component);");
      Indent_Decr (File, "end Get;");
      New_Line (File);

      Indent_Incr (File, "procedure Get_Item");
      Indent_Line (File, "(File              : in     " & Ada_Text_IO & ".File_Type;",
                         " Item              :    out " & Lists.Type_Name (Type_Descriptor) & ";",
                         " Named_Association : in     Boolean := False)");
      Indent_Less (File, "is begin");
      Indent_Line (File, "Get (File, Item, Named_Association, Named_Association);");
      Indent_Decr (File, "end Get_Item;");
      New_Line (File);

      if Type_Descriptor.Record_Tagged then
         Indent_Line (File, "procedure Get_Components");
         Print_Parameter_List;

         Indent_Line (File, "is");
         Indent_More (File, "Temp_Item : " & Asis.Aux.Name (Type_Descriptor.Type_Name) & " renames Item;");
         Indent_Line (File, "begin");
         Indent_Level := Indent_Level + 1;

         Body_First := True;
         Print_Component_Gets (Type_Descriptor.Record_Components);

         Indent_Decr (File, "end Get_Components;");
         New_Line (File);
      end if;

   end Generate_Record;

   procedure Generate_Access
      (File            : in Ada.Text_IO.File_Type;
       Type_Descriptor : in Auto_Io_Gen.Lists.Type_Descriptor_Type)
   is
      Type_Name : constant String := Lists.Type_Name (Type_Descriptor);
      Base_Name : constant String := Asis.Aux.Name (Type_Descriptor.Accessed_Subtype_Ident);
      Aux_Pkg   : constant String := Type_Name & "_Aux";
      Acc_Pkg   : constant String := "Auto_Text_IO.Access_IO";
   begin
      Indent_Incr (File, "procedure Get");
      Indent_Line (File, "(File                   : in     " & Ada_Text_IO & ".File_Type;",
                         " Item                   :    out " & Type_Name & ";",
                         " Named_Association_Item : in     Boolean := False;",
                         " Named_Association_Part : in     Boolean := False)");
      Indent_Less (File, "is");
      Indent_Line (File, "use type " & Acc_Pkg & ".ID_T;");
      Indent_Line (File, "C  : Character;");
      Indent_Line (File, "S  : String (1 .. 4) := (others => ' ');");
      Indent_Line (File, "ID : " & Acc_Pkg & ".ID_T := 0;");
      Indent_Line (File, "Is_Reference : Boolean := False;");
      Indent_Less (File, "begin");
      Indent_Line (File, Ada_Text_IO & ".Get (File, C);");
      Indent_Incr (File, "if C = 'n' then");
      Indent_Line (File, Ada_Text_IO & ".Get (File, S (1 .. 3));");
      Indent_Line (File, "Check (File, S (1 .. 3) = ""ull"", "": Expecting 'null'"");");
      Indent_Line (File, "Item := null;");
      Indent_Line (File, "return;");
      Indent_Decr (File, "end if;");
      Indent_Line (File, "Is_Reference := C = '^';");
      Indent_Line (File, "Check (File, C = '#' or Is_Reference, "": Expecting # or ^"");");
      Indent_Line (File, Acc_Pkg & ".ID_IO.Get (File, ID);");
      Indent_Line (File, "Check (File, ID /= 0, "": Expecting positive number"");");
      Indent_Incr (File, "if Is_Reference then");
      Indent_Line (File, "Check (File, " & Acc_Pkg & ".Id2Addr_Map.Contains (ID)," &
                         " "": Unknown reference"" & ID'Img);");
      Indent_Line (File, "Item := " & Aux_Pkg & ".To_Access" &
                         " (" & Acc_Pkg & ".Id2Addr_Map.Element (ID));");
      Indent_Line (File, "return;");
      Indent_Decr (File, "end if;");
      -- Not null and not a reference: Create new instance
      Indent_Line (File, "Item := new " & Base_Name & ";");
      -- Enter it into the address maps
      Indent_Incr (File, "declare");
      Indent_Line (File, "Addr : constant System.Address := " & Aux_Pkg & ".To_Address (Item);");
      Indent_Less (File, "begin");
      Indent_Line (File, Acc_Pkg & ".Id2Addr_Map.Insert (ID, Addr);",
                         "-- not strictly required; for consistency only:",
                         Acc_Pkg & ".Addr2Id_Map.Insert (Addr, ID);");
      Indent_Decr (File, "end;");
      Indent_Line (File, Ada_Text_IO & ".Get (File, C);");
      Indent_Line (File, "Check (File, C = ''', "": Expecting Tic (')"");");
      -- Call the generated Get procedure for Item.all
      -- if Type_Descriptor.Is_Scalar then
      --    Indent_Line (File, Text_IO_Child_Name (Type_Descriptor.Type_Package) & "Get (File, Item.all);");
      -- else
      Indent_Line (File, "Get (File, Item.all, Named_Association_Item, Named_Association_Part);");
      -- end if;
      Indent_Decr (File, "end Get;");

      Indent_Incr (File, "procedure Get");
      Indent_Line (File, "(Item                   :    out " & Type_Name & ";",
                         " Named_Association_Item : in     Boolean := False;",
                         " Named_Association_Part : in     Boolean := False)");
      Indent_Less (File, "is");
      Indent_Less (File, "begin");
      Indent_Line (File, "Get (Current_Input, Item, Named_Association_Item, Named_Association_Part);");
      Indent_Decr (File, "end Get;");

      Indent_Incr (File, "procedure Get_Item");
      Indent_Line (File, "(File              : in     " & Ada_Text_IO & ".File_Type;",
                         " Item              :    out " & Type_Name & ";",
                         " Named_Association : in     Boolean := False)");
      Indent_Less (File, "is");
      Indent_Less (File, "begin");
      Indent_Line (File, "Get (File, Item, Named_Association, Named_Association);");
      Indent_Decr (File, "end Get_Item;");

      New_Line (File);
   end Generate_Access;

end Auto_Io_Gen.Generate.Ada_File.Get_Body;
