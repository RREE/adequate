with Ada.Containers.Indefinite_Ordered_Maps;
with GNAT.Sockets;                 use GNAT.Sockets;

package Influx is
   package Str_Str_Map_Pkg  is new Ada.Containers.Indefinite_Ordered_Maps (String, String);
   package Str_Port_Map_Pkg is new Ada.Containers.Indefinite_Ordered_Maps (String, Port_Type);
   package Str_Nat_Map_Pkg  is new Ada.Containers.Indefinite_Ordered_Maps (String, Natural);
   package Str_Bool_Map_Pkg is new Ada.Containers.Indefinite_Ordered_Maps (String, Boolean);

   Influxserv     : Str_Str_Map_Pkg.Map;
   Influxport     : Str_Port_Map_Pkg.Map;
   Influxdb       : Str_Str_Map_Pkg.Map;
   Influxname     : Str_Str_Map_Pkg.Map;
   Show_Topic     : Str_Bool_Map_Pkg.Map;
   Show_Device    : Str_Bool_Map_Pkg.Map;
   Show_Location  : Str_Bool_Map_Pkg.Map;
   Device         : Str_Str_Map_Pkg.Map;
   Location       : Str_Str_Map_Pkg.Map;
   Transformation : Str_Str_Map_Pkg.Map;

end Influx;
