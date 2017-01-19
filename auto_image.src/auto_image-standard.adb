package body Auto_Image.Standard is

   -------------------------
   -- Array_Image_Generic --
   -------------------------

   function Array_Image_Generic (Item  : Element_Array) return String is
   begin
      if Item'Length = 1 then
         return Image (Item (Item'First));
      else
         return Image (Item (Item'First)) & ", " & Array_Image_Generic (Item (Index_Type'Succ (Item'First) .. Item'Last));
      end if;
   end Array_Image_Generic;

end Auto_Image.Standard;
