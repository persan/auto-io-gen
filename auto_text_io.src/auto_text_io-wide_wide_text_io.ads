with Ada.Text_IO;
package Auto_Text_Io.Wide_Wide_Text_IO is

   procedure Put
     (File              : in Ada.Text_IO.File_Type;
      Item              : in Wide_Wide_Character;
      Single_Line       : in Boolean := False;
      Named_Association : in Boolean := False);

   procedure Get
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out Wide_Wide_Character;
      Named_Association : in     Boolean := False);
   procedure Put_Item
     (File              : in Ada.Text_IO.File_Type;
      Item              : in Wide_Wide_String;
      Single_Line       : in Boolean := False;
      Named_Association : in Boolean := False);
   procedure Get_Item
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out Wide_Wide_String;
      Named_Association : in     Boolean := False);

end Auto_Text_Io.Wide_Wide_Text_IO;
