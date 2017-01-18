package body Auto_Text_Io.Text_IO is
   use Ada.Text_IO;
   --------------
   -- Put_Item --
   --------------

   procedure Put_Item
     (File              : in Ada.Text_IO.File_Type;
      Item              : in String;
      Single_Line       : in Boolean := False;
      Named_Association : in Boolean := False)
   is
      pragma Unreferenced (Single_Line, Named_Association);
   begin
      Ada.Text_IO.Put (file , '"' & Item & '"');
   end Put_Item;

   --------------
   -- Get_Item --
   --------------

   procedure Get_Item
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out String;
      Named_Association : in     Boolean := False)
   is
      pragma Unreferenced (Named_Association);
      Buffer : String (Item'Range) := (others => ' ');
      Cursor : Natural := Buffer'First;
      Is_Quoted : Boolean := False;

   begin

      while not End_Of_File (File) and then (Buffer(cursor) not  in ASCII.HT | ASCII.VT | ASCII.CR | ASCII.LF | ' ')loop
         Ada.Text_IO.Get (File, Buffer (Cursor));
      end loop;
      if Buffer (Cursor) = '"' then
         Is_Quoted := True;
         Ada.Text_IO.Get (File, Buffer (Cursor));
      end if;
      Cursor := Cursor + 1;
      while not End_Of_File (File) loop
         Ada.Text_IO.Get (File, Buffer (Cursor));
         if Is_Quoted and then Buffer (Cursor) = '"' then
            Item (Item'First .. Cursor - 1) := Buffer (Buffer'First .. Cursor - 1);
            Item (Cursor .. Item'Last) := (others => ' ');
            return;

         elsif Buffer (Cursor) = ' ' then
            Item (Item'First .. Cursor - 1) := Buffer (Buffer'First .. Cursor - 1);
            Item (Cursor .. Item'Last) := (others => ' ');
            return;
         end if;
         Item (Item'First .. Cursor - 1) := Buffer (Buffer'First .. Cursor - 1);
         Item (Cursor .. Item'Last) := (others => ' ');
         return;
      end loop;

   end Get_Item;

end Auto_Text_Io.Text_IO;
