with Ada.Text_IO;
procedure Simple.Text_Io.Main is
   use Ada.Text_IO;

   F : constant Simple_Struct := (12, (others => '1'));
   F2 : constant Struct_With_Discriminant_and_Default := (True, 12, (others => '1'), False);
   F3 : constant Struct_With_Discriminant_and_Default := (False, 12, (others => '1'), 1);
   F4 : Struct_With_Discriminant_and_Default;
   F5 : Tagged_Record_Childern;
   F6 : Standard_Types_Record;
   F7a, F7b : constant Linked_List_Access := new Linked_List;

begin
   Put (Item => F, Named_Association_Record => True); New_Line;
   Put (Item => F2, Named_Association_Record => True); New_Line;
   Put (Item => F3, Named_Association_Record => True); New_Line;
   Put (Item => F4,
        Single_Line_Record => False ,
        Named_Association_Record => True); New_Line;
   Put (Item => F5,
        Single_Line_Record => False ,
        Named_Association_Record => True); New_Line;

   Put (Item => F6,
        Single_Line_Record => False ,
        Named_Association_Record => True); New_Line;

   F7a.Next := F7b;
   F7b.Data := True;
   F7b.Prev := F7a;
   Put (Item => F7a,
        Single_Line_Item => False ,
        Named_Association_Item => True);  New_Line;

end Simple.Text_Io.Main;
