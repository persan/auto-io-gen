--  Abstract :
--
--  Various list types used by other packages.
--
--  Copyright (C) 2001 - 2003, 2006 - 2007 Stephen Leake.  All Rights Reserved.
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
with Ada.Unchecked_Deallocation;
with SAL.Aux.Definite_Private_Items;
with SAL.Aux.Indefinite_Private_Items;
with SAL.Gen.Alg.Count;
with SAL.Gen.Alg.Find_Linear;
with SAL.Poly.Binary_Trees.Sorted.Iterators;
with SAL.Poly.Lists.Double;
package Auto_Io_Gen.Lists is
   pragma Elaborate_Body; --  Asis is

   type Global_Access_Type is access all Integer;
   --  For storage pools in SAL instantiations.

   type Type_Labels_Type is
     (Constrained_Array_Label,
      Unconstrained_Array_Label,
      Record_Label,
      Derived_Label,
      Private_Label,
      Enumeration_Label,
      Float_Label,
      Fixed_Label,
      Signed_Integer_Label,
      Modular_Integer_Label);
   --  Just the kinds of types we support so far.

   subtype Array_Labels_Type is Type_Labels_Type range Constrained_Array_Label .. Unconstrained_Array_Label;

   subtype Non_Scalar_Labels_Type is Type_Labels_Type range Type_Labels_Type'First .. Record_Label;
   subtype Scalar_Labels_Type is Type_Labels_Type range Enumeration_Label .. Type_Labels_Type'Last;
   --  Scalar_Labels_Type are supported by generics in Ada.Text_IO.
   --  Note that Private_Label is a placeholder; it is not known to be
   --  either scalar or non-scalar. Derived_Label is not included in
   --  Non_Scalar_Labels_Type because it is handled specially.

   type Component_Type is record
      Component_Name : Asis.Element; --  A_Defining_Identifier
      Type_Package   : Asis.Element; --  A_Package_Declaration | Nil_Element
      Type_Name      : Asis.Element; --  An_Expression; An_Identifier | A_Selected_Component
      Scalar         : Boolean;
      Invisible      : Boolean;      --  True if Type_Name is declared in the private part of the parent package.
   end record;

   package Component_Aux is new SAL.Aux.Definite_Private_Items (Component_Type);

   package Component_Lists is new SAL.Poly.Lists.Double
     (Item_Type         => Component_Type,
      Item_Node_Type    => Component_Type,
      To_Item_Node      => Component_Aux.To_Item_Node,
      Free_Item         => Component_Aux.Free_Item,
      Copy              => Component_Aux.Copy,
      Node_Storage_Pool => Global_Access_Type'Storage_Pool);

   package Component_Algs is new SAL.Gen.Alg
     (Item_Node_Type => Component_Type,
      Container_Type => Component_Lists.List_Type,
      Iterator_Type  => Component_Lists.Iterator_Type,
      Current        => Component_Lists.Current,
      First          => Component_Lists.First,
      Last           => Component_Lists.Last,
      None           => Component_Lists.None,
      Is_Null        => Component_Lists.Is_Null,
      Next_Procedure => Component_Lists.Next,
      Next_Function  => Component_Lists.Next);

   function Length is new Component_Algs.Count;

   type Array_Component_Labels_Type is
     (Private_Label,
      Enumeration_Label,
      Float_Label,
      Signed_Integer_Label,
      Modular_Integer_Label);
   --  The array component types supported by SAL.Gen_Array_Text_IO.

   subtype Scalar_Array_Component_Labels_Type is Array_Component_Labels_Type range
     Enumeration_Label .. Modular_Integer_Label;

   type Variant_Type is record
      Choice     : Asis.Element; --  An_Identifier
      Components : Component_Lists.List_Type;
   end record;

   type Variant_Access_Type is access Variant_Type;

   function To_Item_Node (Item : in Asis.Element) return Variant_Access_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Variant_Type, Variant_Access_Type);

   function Null_Copy (Item : in Variant_Access_Type) return Variant_Access_Type;
   --  Raises SAL.Programmer_Error.

   package Variant_Lists is new SAL.Poly.Lists.Double
     (Item_Type         => Asis.Element, --  Case_Identifier
      Item_Node_Type    => Variant_Access_Type,
      To_Item_Node      => To_Item_Node,
      Free_Item         => Free,
      Copy              => Null_Copy,
      Node_Storage_Pool => Global_Access_Type'Storage_Pool);

   package Variant_Algs is new SAL.Gen.Alg
     (Item_Node_Type => Variant_Access_Type,
      Container_Type => Variant_Lists.List_Type,
      Iterator_Type  => Variant_Lists.Iterator_Type,
      Current        => Variant_Lists.Current,
      First          => Variant_Lists.First,
      Last           => Variant_Lists.Last,
      None           => Variant_Lists.None,
      Is_Null        => Variant_Lists.Is_Null,
      Next_Procedure => Variant_Lists.Next,
      Next_Function  => Variant_Lists.Next);

   function Length is new Variant_Algs.Count;

   type Variant_Part_Type is record
      Discriminant : Asis.Element; --  An_Identifier
      Variants     : Variant_Lists.List_Type;
   end record;

   type Type_Descriptor_Type (Label : Type_Labels_Type) is record

      Type_Name : Asis.Element; --  A_Defining_Identifier

      Invisible : Boolean;
      --  True if this type is declared in the private part (it has no
      --  public view). Note that "Is_Private" would mean that the
      --  keyword 'private' appears in the visible type declaration.

      Private_Implementation : Boolean;
      --  True if the public view of the type is different enough from
      --  the private view to require an implementation in the private
      --  child or public child body, with a wrapper in the public
      --  child. For example, the public view is not an array (it is
      --  just "private"), but the private view is; then we
      --  instantiate Gen_Array_Text_IO in the public child body
      --  instead of in the spec.
      --
      --  Ideally, Gen_Array_Text_IO should be instantiated in the
      --  private child spec, but that can't be included as a generic
      --  formal package in the public child; we need a "with private"
      --  generic formal package, which isn't in Ada 2005.

      Separate_Body : Boolean;
      --  True if user has labeled the type with the annotation "--
      --  Auto_Io_Gen : separate"; user provides a separate
      --  subprogram for the basic Put and Get; Auto_Io_Gen generates
      --  calls to them.

      case Label is
      when Array_Labels_Type =>
         Array_Index                     : Asis.Element; --  An_Identifier
         Array_Index_Package             : Asis.Element; --  A_Package_Declaration | Nil_Element
         Array_Component_Subtype         : Asis.Element; --  An_Identifier
         Array_Component_Subtype_Package : Asis.Element; --  A_Package_Declaration | Nil_Element
         Array_Component_Type_Package    : Asis.Element; --  A_Package_Declaration | Nil_Element

         --  Array_Component_Subtype_Package names the subtype's
         --  package for instantiating SAL.Gen_Array_Text_IO, and
         --  Array_Component_Type_Package names the type's package for
         --  generating the correct Text_IO child package name to use
         --  for the components.

         Array_Component_Label : Array_Component_Labels_Type;

      when Record_Label =>
         Record_Discriminants : Component_Lists.List_Type;
         Record_Components    : Component_Lists.List_Type;
         Record_Constrained   : Boolean;
         --  If 'Record_Constrained' is True, and this record has
         --  discriminants, they have no defaults, so all objects of
         --  this type are constrained. Note that Ada requires all
         --  disriminants to have defaults if one does.

         Record_Tagged                : Boolean;
         Record_Derived               : Boolean;
         Record_Parent_Type_Name      : Asis.Element; --  An_Identifier
         Record_Parent_Package_Name   : Asis.Element; --  An_Identifier

         Record_Structured_Components : Boolean;
         --  At least one component type is a structured type, so
         --  'Single_Line_Component' etc will be referenced in Get and
         --  Put procedures.

         Record_Variant_Part : Variant_Part_Type;

      when Derived_Label =>
         --  Non-tagged non-scalar derived types, handled by
         --  conversion to/from the root type. For scalar derived
         --  types, we just instantiate Text_IO.
         Derived_Root_Label               : Non_Scalar_Labels_Type;
         Derived_Root_Type_Declaration    : Asis.Element;
         Derived_Root_Package_Declaration : Asis.Element;

      when Private_Label |
        Scalar_Labels_Type =>
         null;

      end case;
   end record;

   subtype Record_Type_Descriptor_Type is Type_Descriptor_Type (Record_Label);

   type Type_Descriptor_Access_Type is access all Type_Descriptor_Type;
   --  We need to update the descriptor while it is stored in the main
   --  list, because we get different pieces of information at
   --  different times, so we use an access type.

   function Type_Name (Item : in Type_Descriptor_Type) return String;

   package Type_Descriptor_Aux is new SAL.Aux.Indefinite_Private_Items
     (Type_Descriptor_Type, Type_Descriptor_Access_Type);

   package Type_Descriptor_Lists is new SAL.Poly.Lists.Double
     (Item_Type         => Type_Descriptor_Type,
      Item_Node_Type    => Type_Descriptor_Access_Type,
      To_Item_Node      => Type_Descriptor_Aux.To_Item_Node,
      Free_Item         => Type_Descriptor_Aux.Free_Item,
      Copy              => Type_Descriptor_Aux.Copy,
      Node_Storage_Pool => Global_Access_Type'Storage_Pool);

   package Type_Descriptor_Algs is new SAL.Gen.Alg
     (Item_Node_Type => Type_Descriptor_Access_Type,
      Container_Type => Type_Descriptor_Lists.List_Type,
      Iterator_Type  => Type_Descriptor_Lists.Iterator_Type,
      Current        => Type_Descriptor_Lists.Current,
      First          => Type_Descriptor_Lists.First,
      Last           => Type_Descriptor_Lists.Last,
      None           => Type_Descriptor_Lists.None,
      Is_Null        => Type_Descriptor_Lists.Is_Null,
      Next_Procedure => Type_Descriptor_Lists.Next,
      Next_Function  => Type_Descriptor_Lists.Next);

   --  We occasionally need to delete a type after it has been added
   --  to the tree, when the full type turns out to be not supported.
   --  We don't sort the list, because we need the Text_IO
   --  declarations in the same order as the parent declarations, for
   --  dependencies. Deleting doesn't happen often, so this is ok.
   package Type_Descriptor_Find_Linear is new Type_Descriptor_Algs.Find_Linear
     (Item_Type         => Type_Descriptor_Type,
      Key_Type          => Type_Descriptor_Access_Type,
      Is_Equal_Node_Key => "=",
      Delete            => Type_Descriptor_Lists.Delete,
      Insert_Before     => Type_Descriptor_Lists.Insert_Before);

   ----------
   --  Sorted list of "pointers" to items in a Type_Descriptor_List

   package Type_Iterator_Aux is
      function To_Item (Item : in Type_Descriptor_Lists.Iterator_Type) return Type_Descriptor_Lists.Iterator_Type;
      function To_Key  (Item : in Type_Descriptor_Lists.Iterator_Type) return String;
      procedure Free_Item (Item : in out Type_Descriptor_Lists.Iterator_Type);
   end Type_Iterator_Aux;

   package Type_Iterator_Trees is new SAL.Poly.Binary_Trees.Sorted
     (Item_Type            => Type_Descriptor_Lists.Iterator_Type,
      Item_Node_Type       => Type_Descriptor_Lists.Iterator_Type,
      To_Item_Node         => Type_Iterator_Aux.To_Item,
      Free_Item            => Type_Iterator_Aux.Free_Item,
      Key_Type             => String,
      To_Key               => Type_Iterator_Aux.To_Key,
      Is_Greater           => ">",
      Is_Equal             => "=",
      Duplicate_Key_Action => SAL.Error,
      Node_Storage_Pool    => Lists.Global_Access_Type'Storage_Pool);

   ----------
   --  Sorted list of names for 'with'ed packages. We use a binary tree
   --  for fast searching; each package is 'with'ed only once.

   type String_Access_Type is access String;
   procedure Free_String is new Ada.Unchecked_Deallocation (String, String_Access_Type);

   type Context_Type is record
      Name     : String_Access_Type;
      Need_Use : Boolean;
   end record;

   package Context_Aux is

      function Copy    (Item : in Context_Type) return Context_Type;
      function To_Item (Item : in Context_Type) return Context_Type;
      function To_Key  (Item : in Context_Type) return String;

      procedure Free_Item (Item : in out Context_Type);

   end Context_Aux;

   package Context_Trees is new SAL.Poly.Binary_Trees.Sorted
     (Item_Type            => Context_Type,
      Item_Node_Type       => Context_Type,
      To_Item_Node         => Context_Aux.To_Item,
      Free_Item            => Context_Aux.Free_Item,
      Key_Type             => String,
      To_Key               => Context_Aux.To_Key,
      Is_Greater           => ">",
      Is_Equal             => "=",
      Duplicate_Key_Action => SAL.Ignore,
      Node_Storage_Pool    => Global_Access_Type'Storage_Pool);

   package Context_Trees_Iterators is new Context_Trees.Iterators
     (Stack_Storage_Pool => Global_Access_Type'Storage_Pool);

   ----------
   --  Sorted and un-sorted lists of simple names for various things.

   package Name_Aux is

      function Copy         (Item : in String) return String;
      function To_Item_Node (Item : in String) return String_Access_Type;
      function To_Key       (Item : in String_Access_Type) return String;

   end Name_Aux;

   package Name_Trees is new SAL.Poly.Binary_Trees.Sorted
     (Item_Type            => String,
      Item_Node_Type       => String_Access_Type,
      To_Item_Node         => Name_Aux.To_Item_Node,
      Free_Item            => Free_String,
      Key_Type             => String,
      To_Key               => Name_Aux.To_Key,
      Is_Greater           => ">",
      Is_Equal             => "=",
      Duplicate_Key_Action => SAL.Ignore,
      Node_Storage_Pool    => Global_Access_Type'Storage_Pool);

   package Name_Trees_Iterators is new Name_Trees.Iterators
     (Stack_Storage_Pool => Global_Access_Type'Storage_Pool);

   package Element_Aux is new SAL.Aux.Definite_Private_Items (Asis.Element);

   package Element_Lists is new SAL.Poly.Lists.Double
     (Item_Type         => Asis.Element,
      Item_Node_Type    => Asis.Element,
      To_Item_Node      => Element_Aux.To_Item_Node,
      Free_Item         => Element_Aux.Free_Item,
      Copy              => Element_Aux.Copy,
      Node_Storage_Pool => Global_Access_Type'Storage_Pool);

   ----------
   --  List of generic formal package parameters, with instantiation arguments.

   type Formal_Package_Type is record
      Package_Declaration : Asis.Element;
      Arguments           : Element_Lists.List_Type;
      --  All Elements are A_Formal_Package_Declaration, for use by
      --  Text_IO_Child_Name.
   end record;

   type Formal_Package_Access_Type is access Formal_Package_Type;

   function To_Item_Node (Item : in Asis.Element) return Formal_Package_Access_Type;
   procedure Free is new Ada.Unchecked_Deallocation (Formal_Package_Type, Formal_Package_Access_Type);

   function Null_Copy (Item : in Formal_Package_Access_Type) return Formal_Package_Access_Type;
   --  Raises SAL.Programmer_Error.

   package Formal_Package_Lists is new SAL.Poly.Lists.Double
     (Item_Type         => Asis.Element, --  Package_Declaration
      Item_Node_Type    => Formal_Package_Access_Type,
      To_Item_Node      => To_Item_Node,
      Free_Item         => Free,
      Copy              => Null_Copy,
      Node_Storage_Pool => Global_Access_Type'Storage_Pool);

   function Find_Package (List : in Formal_Package_Lists.List_Type; Name : in String) return Asis.Element;
   --  Return the item in List that has Formal_Name Name. Result is
   --  nil if not found.

   function Is_Present (List : in Formal_Package_Lists.List_Type; Name : in String) return Boolean;
   --  Return True if there is an item in List that has Formal_Name Name.

end Auto_Io_Gen.Lists;
