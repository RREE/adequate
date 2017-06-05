with Ada.Containers.Indefinite_Ordered_Maps;
with GNAT.Sockets;                 use GNAT.Sockets;

package Influx is
   package Str_Str_Map_Pkg is new Ada.Containers.Indefinite_Ordered_Maps (String, String);
   package Str_Port_Map_Pkg is new Ada.Containers.Indefinite_Ordered_Maps (String, Port_Type);

   Influxserv : Str_Str_Map_Pkg.Map;
   Influxport : Str_Port_Map_Pkg.Map;
   Influxdb   : Str_Str_Map_Pkg.Map;
   Influxname : Str_Str_Map_Pkg.Map;

end Influx;
