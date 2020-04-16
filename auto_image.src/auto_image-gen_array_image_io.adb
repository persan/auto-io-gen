--  Abstract:
--
--  See package spec.
--
--  Based on similar package by Stephen Leake.
--  Copyright (C) 2020 Oliver Kellogg  <okellogg@users.sf.net>
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

-- with Auto_Text_Io.Text_IO_Utils; use Auto_Text_Io.Text_IO_Utils;

package body Auto_Image.Gen_Array_Image_IO is

   function Single_Line return Boolean
   is begin
      return Single_Line_Record or else Single_Line_Component;
   end Single_Line;

   function Named_Association return Boolean
   is begin
      return Named_Association_Record or else Named_Association_Component;
   end Named_Association;


   package body Integer_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is
      begin
         Put (To, '(');
         if Item'Length > 1 then
            for I in Index_Type'First .. Index_Type'Pred (Index_Type'Last) loop
               if Named_Association then
                  if I > Index_Type'First then
                     Put (To, ' ');
                  end if;

                  Put (To, Index_Type'Image (I) & " => ");

                  if not Single_Line then
                     New_Line (To);
                  end if;
               end if;

               Element_Put (To, Item (I));

               if Single_Line then
                  Put (To, String' (", "));
               else
                  Put_Line (To, ",");
               end if;

            end loop;
         end if;

         if Named_Association then
            Put (To, " " & Index_Type'Image (Index_Type'Last) & " => ");

            if not Single_Line then
               New_Line (To);
            end if;
         end if;

         Element_Put (To, Item (Index_Type'Last));
         Put (To, ')');
      end Put;

   end Integer_1D;

   package body Unconstrained_Integer_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is
      begin
         Put (To, '(');
         if Item'Length > 0 then
            if Item'Length > 1 then
               for I in Item'First .. Index_Type'Pred (Item'Last) loop
                  if Named_Association then
                     if I > Index_Type'First then
                        Put (To, ' ');
                     end if;

                     Put (To, Index_Type'Image (I) & " => ");

                     if not Single_Line then
                        New_Line (To);
                     end if;
                  end if;

                  Element_Put (To, Item (I));

                  if Single_Line then
                     Put (To, String' (", "));
                  else
                     Put_Line (To, ",");
                  end if;

               end loop;
            end if;

            if Named_Association then
               Put (To, " " & Index_Type'Image (Item'Last) & " => ");

               if not Single_Line then
                  New_Line (To);
               end if;
            end if;

            Element_Put (To, Item (Item'Last));
         end if;

         Put (To, ')');
      end Put;

   end Unconstrained_Integer_1D;

   package body Modular_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is
      begin
         Put (To, '(');
         if Item'Length > 1 then
            for I in Index_Type'First .. Index_Type'Pred (Index_Type'Last) loop
               if I > Index_Type'First then
                  Put (To, ' ');
               end if;

               if Named_Association then
                  Put (To, Index_Type'Image (I) & " => ");

                  if not Single_Line then
                     New_Line (To);
                  end if;
               end if;

               Element_Put (To, Item (I));

               if Single_Line then
                  Put (To, String' (", "));
               else
                  Put_Line (To, ",");
               end if;

            end loop;
         end if;

         if Named_Association then
            Put (To, " " & Index_Type'Image (Index_Type'Last) & " => ");

            if not Single_Line then
               New_Line (To);
            end if;
         end if;

         Element_Put (To, Item (Index_Type'Last));
         Put (To, ')');
      end Put;

   end Modular_1D;

   package body Unconstrained_Modular_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is
      begin
         Put (To, '(');
         if Item'Length > 0 then
            if Item'Length > 1 then
               for I in Item'First .. Index_Type'Pred (Item'Last) loop
                  if I > Index_Type'First then
                     Put (To, ' ');
                  end if;

                  if Named_Association then
                     Put (To, Index_Type'Image (I) & " => ");

                     if not Single_Line then
                        New_Line (To);
                     end if;
                  end if;

                  Element_Put (To, Item (I));

                  if Single_Line then
                     Put (To, String' (", "));
                  else
                     Put_Line (To, ",");
                  end if;

               end loop;
            end if;

            if Named_Association then
               Put (To, " " & Index_Type'Image (Item'Last) & " => ");

               if not Single_Line then
                  New_Line (To);
               end if;
            end if;

            Element_Put (To, Item (Item'Last));
         end if;

         Put (To, ')');
      end Put;

   end Unconstrained_Modular_1D;

   package body Enumeration_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is begin
         Put (To, '(');
         if Item'Length > 1 then
            for I in Index_Type'First .. Index_Type'Pred (Index_Type'Last) loop
               if Named_Association then
                  if I > Index_Type'First then
                     Put (To, ' ');
                  end if;

                  Put (To, Index_Type'Image (I) & " => ");

                  if not Single_Line then
                     New_Line (To);
                  end if;
               end if;

               Element_Put (To, Item (I));
               if Single_Line then
                  Put (To, String' (", "));
               else
                  Put_Line (To, ",");
               end if;

            end loop;
         end if;

         if Named_Association then
            Put (To, " " & Index_Type'Image (Index_Type'Last) & " => ");

            if not Single_Line then
               New_Line (To);
            end if;
         end if;

         Element_Put (To, Item (Index_Type'Last));
         Put (To, ')');
      end Put;

   end Enumeration_1D;

   package body Unconstrained_Enumeration_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is begin
         Put (To, '(');
         if Item'Length > 0 then
            if Item'Length > 1 then
               for I in Item'First .. Index_Type'Pred (Item'Last) loop
                  if Named_Association then
                     if I > Index_Type'First then
                        Put (To, ' ');
                     end if;

                     Put (To, Index_Type'Image (I) & " => ");

                     if not Single_Line then
                        New_Line (To);
                     end if;
                  end if;

                  Element_Put (To, Item (I));
                  if Single_Line then
                     Put (To, String' (", "));
                  else
                     Put_Line (To, ",");
                  end if;

               end loop;
            end if;

            if Named_Association then
               Put (To, " " & Index_Type'Image (Item'Last) & " => ");

               if not Single_Line then
                  New_Line (To);
               end if;
            end if;

            Element_Put (To, Item (Item'Last));
         end if;

         Put (To, ')');
      end Put;

   end Unconstrained_Enumeration_1D;

   package body Float_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is begin
         Put (To, '(');
         if Item'Length > 1 then
            for I in Index_Type'First .. Index_Type'Pred (Index_Type'Last) loop
               if Named_Association then
                  if I > Index_Type'First then
                     Put (To, ' ');
                  end if;

                  Put (To, Index_Type'Image (I) & " => ");

                  if not Single_Line then
                     New_Line (To);
                  end if;
               end if;

               Element_Put (To, Item (I));

               if Single_Line then
                  Put (To, String' (", "));
               else
                  Put_Line (To, ",");
               end if;

            end loop;
         end if;

         if Named_Association then
            Put (To, " " & Index_Type'Image (Index_Type'Last) & " => ");

            if not Single_Line then
               New_Line (To);
            end if;
         end if;

         Element_Put (To, Item (Index_Type'Last));
         Put (To, ')');
      end Put;

   end Float_1D;

   package body Unconstrained_Float_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is begin
         Put (To, '(');
         if Item'Length > 0 then
            if Item'Length > 1 then
               for I in Item'First .. Index_Type'Pred (Item'Last) loop
                  if Named_Association then
                     if I > Index_Type'First then
                        Put (To, ' ');
                     end if;

                     Put (To, Index_Type'Image (I) & " => ");

                     if not Single_Line then
                        New_Line (To);
                     end if;
                  end if;

                  Element_Put (To, Item (I));

                  if Single_Line then
                     Put (To, String' (", "));
                  else
                     Put_Line (To, ",");
                  end if;

               end loop;
            end if;

            if Named_Association then
               Put (To, " " & Index_Type'Image (Item'Last) & " => ");

               if not Single_Line then
                  New_Line (To);
               end if;
            end if;

            Element_Put (To, Item (Item'Last));
         end if;

         Put (To, ')');
      end Put;

   end Unconstrained_Float_1D;

   package body Private_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is begin
         Put (To, '(');

         if Item'Length > 1 then
            for I in Index_Type'First .. Index_Type'Pred (Item'Last) loop
               if Named_Association then
                  if I > Index_Type'First then
                     Put (To, ' ');
                  end if;

                  Put (To, Index_Type'Image (I) & " => ");

                  if not Single_Line then
                     New_Line (To);
                  end if;
               end if;

               Element_Put (To, Item (I));
               if Single_Line then
                  Put (To, String' (", "));
               else
                  Put_Line (To, ",");
               end if;
            end loop;
         end if;

         if Named_Association then
            Put (To, " " & Index_Type'Image (Index_Type'Last) & " => ");

            if not Single_Line then
               New_Line (To);
            end if;
         end if;

         Element_Put (To, Item (Index_Type'Last));

         Put (To, ')');
      end Put;

   end Private_1D;

   package body Unconstrained_Private_1D is

      procedure Put
        (To    : in out Ada.Strings.Unbounded.Unbounded_String;
         Item  : in Index_Array_Element_Type)
      is begin
         Put (To, '(');

         if Item'Length > 0 then
            if Item'Length > 1 then
               for I in Item'First .. Index_Type'Pred (Item'Last) loop
                  if Named_Association then
                     if I > Index_Type'First then
                        Put (To, ' ');
                     end if;

                     Put (To, Index_Type'Image (I) & " => ");

                     if not Single_Line then
                        New_Line (To);
                     end if;
                  end if;

                  Element_Put (To, Item (I));
                  if Single_Line then
                     Put (To, String' (", "));
                  else
                     Put_Line (To, ",");
                  end if;
               end loop;
            end if;

            if Named_Association then
               Put (To, " " & Index_Type'Image (Item'Last) & " => ");

               if not Single_Line then
                  New_Line (To);
               end if;
            end if;

            Element_Put (To, Item (Item'Last));
         end if;

         Put (To, ')');
      end Put;

   end Unconstrained_Private_1D;

end Auto_Image.Gen_Array_Image_IO;
