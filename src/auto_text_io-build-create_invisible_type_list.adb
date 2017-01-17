--  Abstract :
--
--  See spec
--
--  Copyright (C) 2002, 2003 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--

with Asis.Declarations;
with Ada.Characters.Handling;
with Asis.Elements;
procedure Auto_Text_IO.Build.Create_Invisible_Type_List (State : in out State_Type; Element : in Asis.Element)
is
   use Ada.Characters.Handling;

   Visible_Type_Tree : Auto_Text_IO.Lists.Name_Trees.Tree_Type;
   --  List of types declared in visible part, sorted by name.

begin
   Lists.Name_Trees.Finalize (Visible_Type_Tree);
   Lists.Name_Trees.Finalize (State.Private_State.Invisible_Type_Tree);

   if Asis.Declarations.Is_Private_Present (Element) then
      declare
         Public_Elements  : constant Asis.Element_List := Asis.Declarations.Visible_Part_Declarative_Items (Element);
         Private_Elements : constant Asis.Element_List := Asis.Declarations.Private_Part_Declarative_Items (Element);
      begin
         for I in Public_Elements'Range loop
            case Asis.Elements.Element_Kind (Public_Elements (I)) is
            when Asis.A_Declaration =>
               case Asis.Elements.Declaration_Kind (Public_Elements (I)) is
               when Asis.An_Ordinary_Type_Declaration |
                 Asis.A_Private_Type_Declaration |
                 Asis.A_Private_Extension_Declaration =>

                  declare
                     Name : constant String := To_String
                       (Asis.Declarations.Defining_Name_Image
                          (Asis.Declarations.Names (Public_Elements (I)) (1)));
                  begin
                     Lists.Name_Trees.Add (Visible_Type_Tree, Name);
                  end;

               when others =>
                  null;

               end case;

            when others =>
               null;
            end case;
         end loop;

         for I in Private_Elements'Range loop
            case Asis.Elements.Element_Kind (Private_Elements (I)) is
            when Asis.A_Declaration =>
               case Asis.Elements.Declaration_Kind (Private_Elements (I)) is
               when Asis.An_Ordinary_Type_Declaration =>

                  declare
                     Name : constant String := To_String
                       (Asis.Declarations.Defining_Name_Image
                          (Asis.Declarations.Names (Private_Elements (I)) (1)));
                  begin
                     if not Lists.Name_Trees.Is_Present (Visible_Type_Tree, Name) then
                        Lists.Name_Trees.Add (State.Private_State.Invisible_Type_Tree, Name);
                     end if;
                  end;

               when others =>
                  null;

               end case;

            when others =>
               null;
            end case;
         end loop;
      end;
   end if;
end Auto_Text_IO.Build.Create_Invisible_Type_List;
