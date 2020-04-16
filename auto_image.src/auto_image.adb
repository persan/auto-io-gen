with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Auto_Image is

   use Ada.Strings.Unbounded;

   procedure Put      (To : in out Unbounded_String;
                       Item : in Character)
   is begin
      Append (To, Item);
   end Put;

   procedure Put      (To : in out Unbounded_String;
                       Item : in String)
   is begin
      Append (To, Item);
   end Put;

   procedure Putw     (To   : in out Ada.Strings.Unbounded.Unbounded_String;
                       Item : in Wide_String)
   is
      use Ada.Strings.UTF_Encoding.Wide_Strings;
   begin
      Append (To, Encode (Item));
   end Putw;

   procedure Putww    (To   : in out Ada.Strings.Unbounded.Unbounded_String;
                       Item : in Wide_Wide_String)
   is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   begin
      Append (To, Encode (Item));
   end Putww;

   procedure Put_Line (To : in out Unbounded_String;
                       Item : in String)
   is begin
      Put (To, Item);
      New_Line (To);
   end Put_Line;

   procedure New_Line (To : in out Unbounded_String)
   is begin
      Append (To, ASCII.LF);
   end New_Line;

end Auto_Image;
