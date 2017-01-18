with Simple.Text_Io;
with Ada.Text_IO;
procedure Simple.Main is
   use Simple.Text_Io;
   use Ada.Text_IO;

   F : Simple.Simple_Struct := (12, (others => '1'));
   F2 : Struct_With_Discriminant := (True, 12, (others => '1'), False);
   F3 : Struct_With_Discriminant := (False, 12, (others => '1'), 1);
   F4 : Nested_Struct;
   F5 : Nested_Struct_2;
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

end Simple.Main;
