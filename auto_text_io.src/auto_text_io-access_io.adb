with Interfaces;
with Ada.Text_IO;
with Ada.Characters.Handling;

package body Auto_Text_IO.Access_IO is
   use Interfaces;

   function Unsigned_to_Hex_Str
     (Value : in System.Storage_Elements.Integer_Address)
            return Address_String
   is
      function To_Unsigned is new Ada.Unchecked_Conversion
                 (System.Storage_Elements.Integer_Address,
                  Interfaces.Unsigned_64);
      U : constant Interfaces.Unsigned_64 := To_Unsigned (Value);
      S : Address_String := (others => '0');
      Output : Unsigned_64;
      ShftNu : Natural := 0;
   begin
      for Count in reverse Address_String'Range loop
         Output := Shift_Right (U, ShftNu) and 16#F#;  -- shift right into LS-nibble
         if Output <= 9 then
            S(Count) := Character'Val (Output + 48);   -- ASCII '0'..'9'
         else
            S(Count) := Character'Val (Output + 55);   -- ASCII 'A'..'F'
         end if;
         ShftNu := ShftNu + 4;
      end loop;
      return S;
   end Unsigned_To_Hex_Str;

   type Nibble_Range is mod 16;

   function Hex_Char_To_Nibble (C : Character) return Nibble_Range is
      Binval : Natural := Character'Pos (Ada.Characters.Handling.To_Upper(C))
                        - Character'Pos ('0');
   begin
      if Binval > 9 then
         Binval := Binval - 7;
      end if;
      return Nibble_Range (Binval);
   end Hex_Char_To_Nibble;

   function Hex_Str_to_Unsigned (S : in String)
            return System.Storage_Elements.Integer_Address
   is
      Hex_Digit : Nibble_Range;
      Fried_Bits : Unsigned_64 := 0;
      Len : Natural := 0;
   begin
      for I in S'Range loop
        if S(I) in '0' .. '9' or else S(I) in 'A' .. 'F'
                              or else S(I) in 'a' .. 'f' then
          Len := Len + 1;
          if Len > 16 then
            Ada.Text_IO.Put_Line("Hex_Str_To_Unsigned: input number is too long");
            exit;
          end if;
          Hex_Digit := Hex_Char_To_Nibble (S(I));
          Fried_Bits := Shift_Left(Fried_Bits, 4) or Unsigned_64 (Hex_Digit);
        elsif S(I) /= '_' then
          exit;
        end if;
      end loop;
      return System.Storage_Elements.Integer_Address (Fried_Bits);
   end Hex_Str_to_Unsigned;

   function Is_Valid_Hex_String (S : String) return Boolean is
   begin
      for I in S'Range loop
         if S (I) not in '0' .. '9' and then
             S (I) not in 'A' .. 'F' and then
              S (I) not in 'a' .. 'f' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid_Hex_String;


   package body Conversions is

      function To_Integer (A : T_Access)
               return System.Storage_Elements.Integer_Address is
      begin
         return System.Storage_Elements.To_Integer (To_Address (A));
      end To_Integer;

      function To_String (A : T_Access) return Address_String is
      begin
         return Unsigned_to_Hex_Str (To_Integer (A));
      end To_String;

   end Conversions;

end Auto_Text_IO.Access_IO;

