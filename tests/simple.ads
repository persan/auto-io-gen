package Simple is
   type Integer_Type is range 0 .. 10;

   type Mod_Type is mod 8;

   type Enum_Type is (E1, E2, E3);

   type Float_Type is digits 5 range 1.0 .. 60.0;

   type Fixed_Type is delta 0.1 range 0.0 .. 1.0;
--     type Integer_Type is range 0 .. 10 with Default_Value => 0;
--
--     type Mod_Type is mod 8 with Default_Value => 0;
--
--     type Enum_Type is (E1, E2, E3) with
--       Default_Value => E1,
--       Annotate => Auto_Io_Ignore;
--
--     type Float_Type is digits 5 range 1.0 .. 60.0 with Default_Value => 5.0;
--
--     type Fixed_Type is delta 0.1 range 0.0 .. 1.0 with Default_Value => 0.0;


   type Simple_Struct is record
      F1 : Integer := -1;
      F2 : String (1 .. 19) := (1 => 'F' , 2 => '2' , others => ' ');
   end record;

   type Struct_With_Discriminant_And_Default (D : Boolean := True) is record
      F1 : Integer := -1;
      F2 : String (1 .. 19) := (1 => 'F' , 2 => '6' , others => ' ');
      case D is
         when True =>
            DT : Boolean := True;
         when False =>
            FD : Integer := 0;
      end case;
   end record;


   type Tagged_Record is tagged record
      V1 : Simple_Struct;
      V2 : Simple_Struct;
      V3 : Struct_With_Discriminant_And_Default (False);
      V4 : Struct_With_Discriminant_And_Default;
   end record;


   type Tagged_Record_Childern is new Tagged_Record with record
      Extra : Float := -1.0;
   end record;

   type Standard_Types_Record is record
      Boolean_Field  : Boolean := False;

      Integer_Field             : Integer := 0;
      Natural_Field             : Natural := 1;
      Positive_Field            : Positive := 2;
      Short_Short_Integer_Field : Short_Short_Integer := 3;
      Short_Integer_Field       : Short_Integer := 4;
      Long_Integer_Field        : Long_Integer := 5;
      Long_Long_Integer_Field   : Long_Long_Integer := 6;


      Float_Field           : Float := 0.0;
      Long_Float_Field      : Long_Float := 0.0;
      Long_Long_Float_Field : Long_Long_Float := 0.0;

      Character_Field : Character := ' ';
      Wide_Character_Field : Wide_Character := ' ';
      Wide_Wide_Character_Field : Wide_Wide_Character := ' ';

      String_Field    : String (1 .. 5) := (others => '1');
      Wide_String_Field : Wide_String (1 .. 10) := (others => '2');
      Wide_Wide_String_Field : Wide_Wide_String (1 .. 20) := (others => '1');

      Duration_Field  : Character := ' ';

      Integer_Type_Field : Integer_Type := 0;
   end record;

   type Standard_Types_Array is array (Natural range <>) of Standard_Types_Record;

   -- with  Annotate => Auto_Io_Gen_Ignore;

   --  type Standard_Types_Access is access all Standard_Types_Array;
end Simple;
