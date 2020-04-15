--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2020 Oliver Kellogg  <okellogg@users.sf.net>
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Directories;
with Auto_Io_Gen.Options;

package body Auto_Io_Gen.Generate is

   procedure Set_Indent (File : in Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      --  Indent 0 means column 1
      Set_Col (File, 1 + Count (Auto_Io_Gen.Options.Indent) * (Count (Indent_Level) - 1));
   end Set_Indent;

   procedure Indent (File : in Ada.Text_IO.File_Type; Text : in String)
   is begin
      Set_Indent (File);
      Ada.Text_IO.Put (File, Text);
   end Indent;

   procedure Indent_Line (File  : in Ada.Text_IO.File_Type;
                          Text  : in String;
                          Text1 : in String := "";
                          Text2 : in String := "";
                          Text3 : in String := "";
                          Text4 : in String := "";
                          Text5 : in String := "";
                          Text6 : in String := "";
                          Text7 : in String := "";
                          Text8 : in String := "";
                          Text9 : in String := "")
   is begin
      Set_Indent (File);
      Ada.Text_IO.Put_Line (File, Text);
      if Text1 /= "" then
         Set_Indent (File);
         Ada.Text_IO.Put_Line (File, Text1);
         if Text2 /= "" then
            Set_Indent (File);
            Ada.Text_IO.Put_Line (File, Text2);
            if Text3 /= "" then
               Set_Indent (File);
               Ada.Text_IO.Put_Line (File, Text3);
               if Text4 /= "" then
                  Set_Indent (File);
                  Ada.Text_IO.Put_Line (File, Text4);
                  if Text5 /= "" then
                     Set_Indent (File);
                     Ada.Text_IO.Put_Line (File, Text5);
                     if Text6 /= "" then
                        Set_Indent (File);
                        Ada.Text_IO.Put_Line (File, Text6);
                        if Text7 /= "" then
                           Set_Indent (File);
                           Ada.Text_IO.Put_Line (File, Text7);
                           if Text8 /= "" then
                              Set_Indent (File);
                              Ada.Text_IO.Put_Line (File, Text8);
                              if Text9 /= "" then
                                 Set_Indent (File);
                                 Ada.Text_IO.Put_Line (File, Text9);
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Indent_Line;

   procedure Indent_Incr (File : in Ada.Text_IO.File_Type; Text  : in String)
   is
      use type Ada.Text_Io.Count;
   begin
      Indent_Line (File, Text);
      Indent_Level := Indent_Level + 1;
   end Indent_Incr;

   procedure Indent_Decr (File : in Ada.Text_IO.File_Type; Text  : in String)
   is
      use type Ada.Text_Io.Count;
   begin
      Indent_Level := Indent_Level - 1;
      Indent_Line (File, Text);
   end Indent_Decr;

   procedure Indent_Less (File : in Ada.Text_IO.File_Type; Text : in String)
   is
      use type Ada.Text_Io.Count;
   begin
      Indent_Level := Indent_Level - 1;
      Indent_Line (File, Text);
      Indent_Level := Indent_Level + 1;
   end Indent_Less;

   procedure Indent_More (File : in Ada.Text_IO.File_Type; Text : in String)
   is
      use type Ada.Text_Io.Count;
   begin
      Indent_Level := Indent_Level + 1;
      Indent_Line (File, Text);
      Indent_Level := Indent_Level - 1;
   end Indent_More;

   function Ada2file (Name : String) return String is
      Map : constant Ada.Strings.Maps.Character_Mapping :=
              Ada.Strings.Maps.To_Mapping ("ABCDEFGHIJKLMNOPQRSTUVWXYZ.",
                                           "abcdefghilklmnopqrstuvwxyz-");
   begin
      return Ada.Strings.Fixed.Translate (Name, Mapping => Map);
   end;

   function Ada2file (Folder, Name , Suffix : String) return String is
   begin
      return Ada.Directories.Compose (Folder, Ada2file (Name), Suffix);
   end;

end Auto_Io_Gen.Generate;
