--  Abstract :
--
--  See spec
--
--  Copyright (C) 1999 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
package body Test_Gen_Stacks_Bounded_Aux is

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access_Type);

   function Create (Name : in String) return Name_Type
   is begin
      if Debug then
         Ada.Text_IO.Put_Line ("Create " & Name);
         Ada.Text_IO.Flush;
      end if;
      return (Ada.Finalization.Controlled with new String'(Name));
   end Create;

   function Name (Item : in Name_Type) return String
   is begin
      if Item.Name /= null then
         return Item.Name.all;
      else
         return "";
      end if;
   end Name;

   procedure Free_Name (Item : in out Name_Type)
   is begin
      Item := Null_Name;
   end Free_Name;

   procedure Adjust (Item : in out Name_Type)
   is begin
      if Debug then
         Ada.Text_IO.Put ("Adjust ");
      end if;
      if Item.Name /= null then
         if Debug then
            Ada.Text_IO.Put_Line (Item.Name.all);
         end if;
         Item.Name := new String'(Item.Name.all);
      else
         if Debug then
            Ada.Text_IO.Put_Line (" null");
         end if;
      end if;
      if Debug then
         Ada.Text_IO.Flush;
      end if;
   end Adjust;

   procedure Finalize (Item : in out Name_Type)
   is begin
      if Item.Name /= null then
         if Debug then
            Ada.Text_IO.Put_Line ("Finalize " & Item.Name.all);
            Ada.Text_IO.Flush;
         end if;
         Free (Item.Name);
      end if;
   end Finalize;

end Test_Gen_Stacks_Bounded_Aux;
