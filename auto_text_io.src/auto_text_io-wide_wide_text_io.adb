with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Auto_Text_Io.Text_IO;
with Ada.Strings.Wide_Wide_Fixed;
package body Auto_Text_Io.Wide_Wide_Text_IO is
   use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   use Ada.Strings.Wide_Wide_Fixed;
   --------------
   -- Put_Item --
   --------------

   procedure Put_Item
     (File              : in Ada.Text_IO.File_Type;
      Item              : in Wide_Wide_String;
      Single_Line       : in Boolean := False;
      Named_Association : in Boolean := False)
   is
   begin
      Auto_Text_Io.Text_IO.Put_Item (File, Encode (Item), Single_Line, Named_Association);
   end Put_Item;

   --------------
   -- Get_Item --
   --------------

   procedure Get_Item
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out Wide_Wide_String;
      Named_Association : in     Boolean := False)
   is
      Buffer : String (1 .. Item'Length * 2) := (others => ' ');
   begin
      Auto_Text_Io.Text_IO.Get_Item (File, Buffer, Named_Association);
      Move (Decode (Buffer), Item);
   end Get_Item;


   procedure Put
     (File              : in Ada.Text_IO.File_Type;
      Item              : in Wide_Wide_Character;
      Single_Line       : in Boolean := False;
      Named_Association : in Boolean := False) is
   begin
      Auto_Text_Io.Text_IO.Put_Item (File, Encode (Item & ""), Single_Line, Named_Association);
   end;

   procedure Get
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out Wide_Wide_Character;
      Named_Association : in     Boolean := False)is
      Buffer : String (1 .. 1);
      Obuff  :  Wide_Wide_String (1 .. 1);
   begin
      Auto_Text_Io.Text_IO.Get_Item (File, Buffer, Named_Association);
      Move (Decode (Buffer), Obuff);
      Item := Obuff (Obuff'First);
   end;


end Auto_Text_Io.Wide_Wide_Text_IO;
