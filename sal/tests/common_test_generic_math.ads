--  Abstract:
--
--  Declarations used in all three test_gen_*_array subprograms.
--

with SAL.Gen_Array_Text_IO;
with SAL.Math_Float; use SAL.Math_Float;
with SAL.Math_Float.Text_IO; use SAL.Math_Float.Text_IO;
package Common_Test_Generic_Math is

   type Three_Index_Type is range 1 .. 3;
   type Four_Index_Type  is (A, B, C, D);
   type Five_Index_Type  is range 1 .. 5;

   type Three_Array_Real_Type is array (Three_Index_Type) of Real_Type;
   type Four_Array_Real_Type is array (Four_Index_Type) of Real_Type;
   type Five_Array_Real_Type is array (Five_Index_Type) of Real_Type;

   type Three_By_Three_Type is array (Three_Index_Type) of Three_Array_Real_Type;
   type Three_By_Four_Type  is array (Three_Index_Type) of Four_Array_Real_Type;
   type Three_By_Five_Type  is array (Three_Index_Type) of Five_Array_Real_Type;

   type Four_By_Three_Type  is array (Four_Index_Type)  of Three_Array_Real_Type;
   type Four_By_Four_Type   is array (Four_Index_Type)  of Four_Array_Real_Type;
   type Four_By_Five_Type   is array (Four_Index_Type)  of Five_Array_Real_Type;

   package Three_Array_Float_IO is new SAL.Gen_Array_Text_IO.Float_1D
     (Element_Type             => Real_Type,
      Index_Type               => Three_Index_Type,
      Index_Array_Element_Type => Three_Array_Real_Type,
      Element_Put              => Real_Text_IO.Put,
      Element_Get              => Real_Text_IO.Get,
      Init_Default_Exp         => 0);

   package Four_Array_Float_IO is new SAL.Gen_Array_Text_IO.Float_1D
     (Element_Type             => Real_Type,
      Index_Type               => Four_Index_Type,
      Index_Array_Element_Type => Four_Array_Real_Type,
      Element_Put              => Real_Text_IO.Put,
      Element_Get              => Real_Text_IO.Get,
      Init_Default_Exp         => 0);

   package Five_Array_Float_IO is new SAL.Gen_Array_Text_IO.Float_1D
     (Element_Type             => Real_Type,
      Index_Type               => Five_Index_Type,
      Index_Array_Element_Type => Five_Array_Real_Type,
      Element_Put              => Real_Text_IO.Put,
      Element_Get              => Real_Text_IO.Get,
      Init_Default_Exp         => 0);

   package Three_By_Three_IO is new SAL.Gen_Array_Text_IO.Private_1D
     (Element_Type                   => Three_Array_Real_Type,
      Index_Type                     => Three_Index_Type,
      Index_Array_Element_Type       => Three_By_Three_Type,
      Element_Put                    => Three_Array_Float_IO.Put_Item,
      Element_Get                    => Three_Array_Float_IO.Get_Item,
      Init_Default_Single_Line_Array => False);

   package Three_By_Four_IO is new SAL.Gen_Array_Text_IO.Private_1D
     (Element_Type                   => Four_Array_Real_Type,
      Index_Type                     => Three_Index_Type,
      Index_Array_Element_Type       => Three_By_Four_Type,
      Element_Put                    => Four_Array_Float_IO.Put_Item,
      Element_Get                    => Four_Array_Float_IO.Get_Item,
      Init_Default_Single_Line_Array => False);

   package Three_By_Five_IO is new SAL.Gen_Array_Text_IO.Private_1D
     (Element_Type                  => Five_Array_Real_Type,
      Index_Type                    => Three_Index_Type,
      Index_Array_Element_Type      => Three_By_Five_Type,
      Element_Put                   => Five_Array_Float_IO.Put_Item,
      Element_Get                   => Five_Array_Float_IO.Get_Item,
      Init_Default_Single_Line_Array => False);

   package Four_By_Three_IO is new SAL.Gen_Array_Text_IO.Private_1D
     (Element_Type                   => Three_Array_Real_Type,
      Index_Type                     => Four_Index_Type,
      Index_Array_Element_Type       => Four_By_Three_Type,
      Element_Put                    => Three_Array_Float_IO.Put_Item,
      Element_Get                    => Three_Array_Float_IO.Get_Item,
      Init_Default_Single_Line_Array => False);

   package Four_By_Four_IO is new SAL.Gen_Array_Text_IO.Private_1D
     (Element_Type                   => Four_Array_Real_Type,
      Index_Type                     => Four_Index_Type,
      Index_Array_Element_Type       => Four_By_Four_Type,
      Element_Put                    => Four_Array_Float_IO.Put_Item,
      Element_Get                    => Four_Array_Float_IO.Get_Item,
      Init_Default_Single_Line_Array => False);

   package Four_By_Five_IO is new SAL.Gen_Array_Text_IO.Private_1D
     (Element_Type                   => Five_Array_Real_Type,
      Index_Type                     => Four_Index_Type,
      Index_Array_Element_Type       => Four_By_Five_Type,
      Element_Put                    => Five_Array_Float_IO.Put_Item,
      Element_Get                    => Five_Array_Float_IO.Get_Item,
      Init_Default_Single_Line_Array => False);

end Common_Test_Generic_Math;
