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

--     function Image (Item  : Wide_String) return String is (S);
--     function Image (Item  : Wide_Wide_String) return String is (S);

   function Image (Item  : Duration) return String is (Item'Img);

end Auto_Image.Standard;
