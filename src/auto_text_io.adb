--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2003-2004, 2006, 2007 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Asis.Aux;
with Asis.Declarations;
with Asis.Elements;
with Asis.Expressions;
with Auto_Text_IO.Options;
package body Auto_Text_IO is

   function Version return String
   is begin
      return Program_Name & " version 3.05";
   end Version;

   function Private_Text_IO_Child_Name (Package_Declaration : in Asis.Element) return String
   is
      use Asis;
   begin
      --  This is much simpler than Text_IO_Child_Name because we
      --  don't have to worry about private children of the Ada
      --  predefined packages.
      case Elements.Element_Kind (Package_Declaration) is
      when A_Declaration =>

         case Elements.Declaration_Kind (Package_Declaration) is
         when A_Package_Declaration =>
            return Aux.Name (Package_Declaration) & ".Private_Text_IO";

         when A_Generic_Package_Declaration =>
            return Aux.Name (Package_Declaration) & ".Gen_Private_Text_IO";

         when others =>
            raise Program_Error;

         end case;

      when others =>
         raise Program_Error;

      end case;
   end Private_Text_IO_Child_Name;

   function Text_IO_Child_Name
     (Package_Declaration : in Asis.Element;
      Label               : in Child_Name_Label_Type := Expression)
     return String
   is
      use Asis;

      function Subst_Underscores (Name : in String) return String
      is
         Result : String := Name;
      begin
         for I in Result'Range loop
            if Result (I) = '.' then
               Result (I) := '_';
            end if;
         end loop;
         return Result;
      end Subst_Underscores;

      function Check_Name (Package_Name : in String) return String
      is begin
         if Package_Name'Length >= 4 and then
            Package_Name (Package_Name'First .. Package_Name'First + 3) = "Ada." then
            return Subst_Underscores (Package_Name) & "_Text_IO";

         elsif Package_Name'Length = 10 and then
            Package_Name = "Interfaces" then
            return "Interfaces_Text_IO";

         elsif Package_Name'Length > 10 and then
            Package_Name (Package_Name'First .. Package_Name'First + 10) = "Interfaces." then
            return Subst_Underscores (Package_Name) & "_Text_IO";

         elsif Package_Name'Length = 6 and then
            Package_Name = "System" then
            return "System_Text_IO";

         elsif Package_Name'Length > 6 and then
            Package_Name (Package_Name'First .. Package_Name'First + 6) = "System." then
            return Subst_Underscores (Package_Name) & "_Text_IO";

         else
            return Package_Name & Options.Package_Separator & "Text_IO";

         end if;
      end Check_Name;

      function Formal_Text_IO_Name (Element : in Asis.Element) return String
      is
         use Asis.Declarations, Asis.Elements, Asis.Expressions;

         function Handle_Identifier (Wide_Name : in Wide_String) return String
         is
            Name : constant String := Ada.Characters.Handling.To_String (Wide_Name);
         begin
            if Name'Length >= 4 and then Name (1 .. 4) = "Gen_" then
               return Name (5 .. Name'Last);
            else
               --  Not a generic package name; just drop it. For
               --  example, in SAL.Gen_Math.Gen_DOF_3, this is
               --  "SAL", and we want Math_DOF_3, so drop SAL.
               return "";
            end if;
         end Handle_Identifier;

         function Concat (Left, Right : in String) return String
         is begin
            if Left'Length = 0 then
               return Right;
            else
               return Left & "_" & Right;
            end if;
         end Concat;

      begin --  Formal_Text_IO_Name
         case Element_Kind (Element) is
         when A_Defining_Name =>
            --  Recursive call
            case Defining_Name_Kind (Element) is
            when A_Defining_Expanded_Name =>
               return Concat
                 (Formal_Text_IO_Name (Defining_Prefix (Element)),
                  Formal_Text_IO_Name (Defining_Selector (Element)));

            when A_Defining_Identifier =>
               return Handle_Identifier (Defining_Name_Image (Element));

            when others =>
               raise Program_Error;
            end case;

         when A_Declaration =>
            return Formal_Text_IO_Name (Asis.Declarations.Names (Element) (1)) & "_Text_IO";

         when An_Expression =>
            case Expression_Kind (Element) is
            when An_Identifier =>
               return Handle_Identifier (Name_Image (Element));

            when A_Selected_Component =>
               return Concat (Formal_Text_IO_Name (Prefix (Element)), Formal_Text_IO_Name (Selector (Element)));

            when others =>
               raise Program_Error;
            end case;

         when others =>
            raise Program_Error;
         end case;
      end Formal_Text_IO_Name;

   begin
      case Elements.Declaration_Kind (Package_Declaration) is
      when A_Package_Declaration =>
         return Check_Name (Aux.Name (Package_Declaration));

      when A_Formal_Package_Declaration =>
         case Label is
         when Expression | Generic_Formal =>
            return Aux.Name (Package_Declaration) & "_Text_IO";

         when Generic_Unit =>
            return Aux.Name (Package_Declaration) & ".Gen_Text_IO";

         when With_Clause =>
            return Aux.Name (Declarations.Generic_Unit_Name (Package_Declaration)) & ".Gen_Text_IO";
         end case;

      when A_Generic_Package_Declaration =>
         --  Don't need Check_Name here; the generic package can't
         --  be a predefined one, and we assume we aren't doing
         --  this with Ada 83.

         case Label is
         when Expression | Generic_Formal =>
            return Formal_Text_IO_Name (Package_Declaration);

         when Generic_Unit |
           With_Clause =>
            return Aux.Name (Package_Declaration) & ".Gen_Text_IO";

         end case;

      when others =>
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            Aux.Image (Package_Declaration));

      end case;

   end Text_IO_Child_Name;

end Auto_Text_IO;
