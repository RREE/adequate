with Ada.Containers.Indefinite_Ordered_Maps;

package Rrd is
   package Str_Str_Map_Pkg is new Ada.Containers.Indefinite_Ordered_Maps (String, String);
   package Str_Int_Map_Pkg is new Ada.Containers.Indefinite_Ordered_Maps (String, Natural);

   Rrdserv : Str_Str_Map_Pkg.Map;
   Rrdport : Str_Int_Map_Pkg.Map;
   Rrdfile : Str_Str_Map_Pkg.Map;
   Rrdname : Str_Str_Map_Pkg.Map;

end Rrd;
