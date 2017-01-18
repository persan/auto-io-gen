with Ada.Text_IO;
package Auto_Text_Io.Text_IO is
   procedure Put_Item
     (File              : in Ada.Text_IO.File_Type;
      Item              : in String;
      Single_Line       : in Boolean := False;
      Named_Association : in Boolean := False);
   procedure Get_Item
     (File              : in     Ada.Text_IO.File_Type;
      Item              :    out String;
      Named_Association : in     Boolean := False);

end Auto_Text_Io.Text_IO;
