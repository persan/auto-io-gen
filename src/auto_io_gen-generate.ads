--  Abstract :
--
--  Auto_Io_Gen master child package for code generation.
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

with Ada.Text_IO;

package Auto_Io_Gen.Generate is

   procedure Set_Indent (File : in Ada.Text_IO.File_Type);
   Indent_Level : Ada.Text_IO.Positive_Count := 1;
   --  1 is no indentation.

   procedure Indent (File : in Ada.Text_IO.File_Type; Text : in String);
   --  Do Set_Indent (File), then Put (File, Text).

   procedure Indent_Line (File : in Ada.Text_IO.File_Type;
                          Text  : in String;
                          Text1 : in String := "";
                          Text2 : in String := "";
                          Text3 : in String := "";
                          Text4 : in String := "";
                          Text5 : in String := "";
                          Text6 : in String := "";
                          Text7 : in String := "";
                          Text8 : in String := "";
                          Text9 : in String := "");
   --  Do Set_Indent (File), then Put_Line (File, Text).

   procedure Indent_Incr (File : in Ada.Text_IO.File_Type; Text : in String);
   --  Call Indent_Line, then increment Indent_Level.

   procedure Indent_Decr (File : in Ada.Text_IO.File_Type; Text : in String);
   --  Decrement Indent_Level, then call Indent_Line.

   procedure Indent_Less (File : in Ada.Text_IO.File_Type; Text : in String);
   --  Decrement Indent_Level, call Indent_Line, increment Indent_Level.

   procedure Indent_More (File : in Ada.Text_IO.File_Type; Text : in String);
   --  Increment Indent_Level, call Indent_Line, decrement Indent_Level.

   function Ada2file (Folder, Name , Suffix : String) return String;

end Auto_Io_Gen.Generate;
