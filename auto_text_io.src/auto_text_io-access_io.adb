
package body Auto_Text_IO.Access_IO is

   function Next_ID return ID_T is
   begin
      Count := Count + 1;
      return Count;
   end Next_ID;

   procedure Reset is
   begin
      Count := 0;
      Addr2Id_Map.Clear;
      Id2Addr_Map.Clear;
   end Reset;

   function To_String (ID : ID_T) return String is
      Str : constant String := ID_T'Image (ID);
   begin
      return Str (2 .. Str'Last);
   end To_String;

end Auto_Text_IO.Access_IO;

