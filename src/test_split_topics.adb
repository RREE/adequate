with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Test_Split_Topics is
   type Str_Ptr is access all String;
   type Str_Array is array (Positive range <>) of Str_Ptr;
   Topic : constant Str_Array :=
     (new String'("/zwei/drei/vier/"),
      new String'("heim/s122/heiz/T_Dach"),
      new String'("heim/s124/Keller/T_Vorlauf"),
      new String'("heim/s124/Keller/T_Ruecklauf"),
      new String'("heim/s124/Keller/T_Heizkoerper"),
      new String'("heim/s124/Keller/T_Luft_Boden"),
      new String'("heim/s124/Keller/h_Luft_Boden"),
      new String'("heim/s122/Dach/T_Dach"),
      new String'("heim/s122/Dach/h_Dach"),
      new String'("heim/s122/Dach/V_Batt_Dach"),
      new String'("heim/hzg1w/T_Warmwasser_Zul"),
      new String'("heim/hzg1w/T_Warmwasser_Ausl"),
      new String'("heim/hzg1w/T_Heizung_Vorl"),
      new String'("heim/hzg1w/T_Heizung_Rueckl"),
      new String'("heim/hzg1w/T_Zirkulation"),
      new String'("heim/s125/Ext/T_DS"),
      new String'("heim/s125/Ext/T_Ext"),
      new String'("heim/s125/Ext/h_Ext"),
      new String'("heim/s125/Ext/p_Ext")
);
   type Str_Token is record
      Start_Ptr : Natural;
      End_Ptr   : Natural;
   end record;
   subtype Idx_Range is Natural range 1 .. 20;
   type Token_Array is array (Idx_Range) of Str_Token;

   Tokens : Token_Array := (others => (1, 0));
   Idx : Idx_Range := 1;
   Pos : Natural := 1;

begin
   for T of Topic loop
      Ada.Text_IO.Put_Line ("topic = '" & T.all & ''');
      Tokens := (others => (1, 0));
      Idx := 1;
      Pos  := 1;

      loop
         Tokens(Idx).Start_Ptr := Pos;
         Pos := Index (T(Pos .. T'Last), "/");
         exit when Pos = 0;
         Tokens(Idx).End_Ptr := Pos-1;
         Pos := Pos + 1;
         Output:
         declare
            use Ada.Text_IO;
         begin
            Put ("idx =" & Idx'Img);
            Put (", token = '" & T(Tokens(Idx).Start_Ptr .. Tokens(Idx).End_Ptr));
            Put (''');
            New_Line;
         end Output;
         Idx := Idx + 1;
      end loop;
      Tokens(Idx).End_Ptr := T'Last;

      declare
         use Ada.Text_IO;
      begin
         Put ("idx =" & Idx'Img);
         Put (", token = '" & T(Tokens(Idx).Start_Ptr .. Tokens(Idx).End_Ptr));
         Put (''');
         New_Line;
      end;
   end loop;

end Test_Split_Topics;
