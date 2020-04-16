with Ada.Strings.Unbounded;

package Auto_Image is

   -- These flags control the way the image is printed.
   -- The user shall set them to the desired values before calling an
   -- Image attribute.
   --
   Single_Line_Record          : Boolean := True;
   Named_Association_Record    : Boolean := False;
   Single_Line_Component       : Boolean := True;
   Named_Association_Component : Boolean := False;

   -- Utility functions, too small to warrant an own package...

   procedure Put      (To   : in out Ada.Strings.Unbounded.Unbounded_String;
                       Item : in Character);

   procedure Put      (To   : in out Ada.Strings.Unbounded.Unbounded_String;
                       Item : in String);

   procedure Putw     (To   : in out Ada.Strings.Unbounded.Unbounded_String;
                       Item : in Wide_String);

   procedure Putww    (To   : in out Ada.Strings.Unbounded.Unbounded_String;
                       Item : in Wide_Wide_String);

   procedure Put_Line (To   : in out Ada.Strings.Unbounded.Unbounded_String;
                       Item : in String);

   procedure New_Line (To   : in out Ada.Strings.Unbounded.Unbounded_String);

end Auto_Image;
