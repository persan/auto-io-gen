with Ada.Wide_Characters;
package Auto_Image.Standard is

   function Image (Item  : Boolean) return String is (Item'Img);
   function Image (Item  : Integer) return String is (Item'Img);
   function Image (Item  : Short_Integer) return String is (Item'Img);
   function Image (Item  : Short_Short_Integer) return String is (Item'Img);
   function Image (Item  : Long_Integer) return String is (Item'Img);
   function Image (Item  : long_Long_Integer) return String is (Item'Img);

   function Image (Item  : Float) return String is (Item'Img);
   function Image (Item  : Long_Float) return String is (Item'Img);
   function Image (Item  : Long_Long_Float) return String is (Item'Img);

   function Image (Item  : String) return String is ('"' & Item & '"');

   --   function Image (Item  : Wide_String) return String is (Ada.Wide_Characters.);
   --   function Image (Item  : Wide_Wide_String) return String is (Item);

   function Image (Item  : Duration) return String is (Item'Img);

   generic
      type Element_Type is private;
      type Index_Type is(<>);
      type Element_Array is array (Index_Type) of Element_Type;
      with function Image (Item  : Element_Type) return String is <>;
   function Array_Image_Generic (Item  : Element_Array) return String;

end Auto_Image.Standard;
