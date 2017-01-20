package Anotations is
   type Enum_Type is (E1, E2, E3) with
     Default_Value => E1,
     Annotate => (Auto_Io , Ignore),
     Annotate => (DDS , Top_Level);
end Anotations;
