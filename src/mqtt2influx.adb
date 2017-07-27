with Ada.Exceptions;               use Ada.Exceptions;

with Alog;                         use Alog;
with Alog.Logger;
with Alog.Policy_DB;
with Logs;                         use Logs;

with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with Strings_Edit.Integers;        use Strings_Edit.Integers;

with MQTT_Influx_Client;           use MQTT_Influx_Client;

with Config;                       use Config;
with Influx;


procedure MQTT2Influx is
   --  read config file(s).
   Cfg : Configuration := Init ("mqtt2influx.cfg",
                                Case_Sensitive   => False,
                                On_Type_Mismatch => Print_Warning);


   My_Version  : constant String  := "V0.0";
   My_Name     : constant String  := Config.Value_Of (Cfg, "broker", "client_id", "mqtt2influx");
   Broker_Name : constant String  := Config.Value_Of (Cfg, "broker", "servername", "localhost");
   Port_Int    : constant Integer := Config.Value_Of (Cfg, "broker", "port", 1883);
   Broker_Port : constant GNAT.Sockets.Port_Type := GNAT.Sockets.Port_Type (Port_Int);


   Influxserver_Default : constant String :=
     Config.Value_Of (Cfg, "defaults", "influxdbserver", "localhost");
   Influxport_Default : constant Natural :=
     Config.Value_Of (Cfg, "defaults", "influxdbport", 8086);
   Influxdb_Default : constant String :=
     Config.Value_Of (Cfg, "defaults", "influxdb", "testdata_db");


   Factory   : aliased Connections_Factory;
   Broker    : aliased Connections_Server (Factory'Access, 0);
   QoS       : constant QoS_Level := At_Most_Once;
   Myself    : aliased MQTT_Client (Listener             => Broker'Unchecked_Access,
                                    Input_Size           => 800,
                                    Output_Size          => 800,
                                    Max_Subscribe_Topics => 500);

   Sub_Id : Packet_Identification (QoS);
   Sub_Nr : Packet_Identifier := 24; -- arbitrary, may be anything

   use Config;
   use Config.String_Vector;

   Patterns : Section_List := Read_Sections (Cfg);

   procedure Remove_Section (Name : String)
   is
      Position : Cursor;
   begin
      Position := Find (Patterns, Name);
      Delete (Patterns, Position);
   exception
   when Constraint_Error => null;
   --  one of the predefined sections doesn't exist, don't care
   end Remove_Section;

begin
   L.Log_Message (Debug, "MQTT2Influx started (MQTT client id = '" & My_Name & " " & My_Version & "')");

   --  read configured patterns
   Remove_Section ("broker");
   Remove_Section ("logs");
   Remove_Section ("defaults");
   L.Log_Message (Debug, "default Influx server  = " & Influxserver_Default);
   L.Log_Message (Debug, "default Influx port    = " & Image(Influxport_Default));
   L.Log_Message (Debug, "default Influx DB name = " & Influxdb_Default);

   for P of Patterns loop
      Influx.Influxserv.Insert (P, Config.Value_Of (Cfg, P, "influxdbserver", Influxserver_Default));
      L.Log_Message (Debug, "influxserver("&P&")="&Influx.Influxserv(P));
      Influx.Influxport.Insert (P, GNAT.Sockets.Port_Type (Config.Value_Of (Cfg, P, "influxdbport", Influxport_Default)));
      -- L.Log_Message (Debug, "influxport("&P&")="&Image(Natural(Influx.Influxport(P))));
      Influx.Influxdb.Insert (P, Config.Value_Of (Cfg, P, "influxdb", Influxdb_Default));
      L.Log_Message (Debug, "influxdb("&P&")="&Influx.Influxdb(P));
      Influx.Influxname.Insert (P, Config.Value_Of (Cfg, P, "influxname", P));
      L.Log_Message (Debug, "influxname("&P&")="&Influx.Influxname(P));
   end loop;


   if Policy_DB.Get_Default_Loglevel = Debug then
      Trace_On (Factory  => Factory,
                Received => GNAT.Sockets.Server.Trace_Decoded,
                Sent     => GNAT.Sockets.Server.Trace_Decoded);
   else
      Trace_Off (Factory);
   end if;

   --
   --  connect to broker
   --
   if QoS > At_Least_Once then
      Sub_Id.ID := 24; -- arbitrary
   end if;

   Set_Overlapped_Size (Myself, 4); -- One response packet (4 bytes) is
                                    -- queued for send without blocking
                                    -- receiving

   L.Log_Message (Debug, "connect to MQTT broker " & Broker_Name & ':' & Broker_Port'Img);
   Connect (Broker, Myself'Unchecked_Access, Broker_Name, Broker_Port);
   while not Is_Connected (Myself) loop -- busy waiting
      delay 0.01;
   end loop;
   L.Log_Message (Debug, My_Name & " connected to MQTT broker " & Broker_Name & ':' & Broker_Port'Img);
   Send_Connect (Myself, My_Name);
   L.Log_Message (Debug, "presented myself at broker");

   delay 0.1;

   --  subscribe to patterns
   for P of Patterns loop
      L.Log_Message (Info, "subscribe to " & P);
      Send_Subscribe (Myself, Sub_Nr, P, QoS);
   end loop;

   loop
      --  wait for incoming message
      --  on reception handle as configured
      --  send the transformed message to influx
      delay 0.01;
   end loop;

exception
when E : others =>
   L.Log_Message (Error, "Error (main): " & Exception_Information (E));

end MQTT2Influx;
