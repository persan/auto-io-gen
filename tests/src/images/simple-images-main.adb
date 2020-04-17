with Ada.Text_IO;
with Auto_Image;

procedure Simple.Images.Main is
   F  : constant Simple_Struct := (12, (others => '1'));
   F2 : constant Struct_With_Discriminant_and_Default := (True, 12, (others => '1'), False);
   F3 : constant Struct_With_Discriminant_and_Default := (False, 12, (others => '1'), 1);
   F4 : Struct_With_Discriminant_and_Default;
   F5 : Tagged_Record_Childern;
   F6 : Standard_Types_Record;
   F7a, F7b : constant Linked_List_Access := new Linked_List;
begin
   Auto_Image.Named_Association_Record := True;

   Ada.Text_IO.Put_Line (Image (F));
   Ada.Text_IO.Put_Line (Image (F2));
   Ada.Text_IO.Put_Line (Image (F3));

   Auto_Image.Single_Line_Record := False;

   Ada.Text_IO.Put_Line (Image (F4));
   Ada.Text_IO.Put_Line (Image (F5));
   Ada.Text_IO.Put_Line (Image (F6));

   F7a.Next := F7b;
   F7b.Data := True;
   F7b.Prev := F7a;
   Ada.Text_IO.Put_Line (Image (F7a));

end Simple.Images.Main;
