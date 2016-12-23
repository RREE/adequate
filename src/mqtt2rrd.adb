with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;

with Alog;                         use Alog;
with Alog.Logger;
with Alog.Policy_DB;
with Alog.Facilities.File_Descriptor;
with Logs;                         use Logs;

with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;
with GNAT.Sockets.MQTT.Server;     use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;
with Strings_Edit.Integers;        use Strings_Edit.Integers;

with MQTT_Rrd_Client;              use MQTT_Rrd_Client;

with Config;                       use Config;
with Rrd;


procedure MQTT2rrd is
   Cfg : Configuration := Init ("mqtt2rrd.cfg",
                                Case_Sensitive   => False,
                                On_Type_Mismatch => Print_Warning);

   -- Log_Level :
   Client_Name : constant String := Config.Value_Of (Cfg, "broker", "client_id", "mqtt2rdd");
   Broker_Name : constant String := Config.Value_Of (Cfg, "broker", "servername", "localhost");
   Port_Int    : constant Integer := Config.Value_Of (Cfg, "broker", "port", 1883);
   Broker_Port : constant GNAT.Sockets.Port_Type := GNAT.Sockets.Port_Type (Port_Int);

   Factory   : aliased Connections_Factory;
   Broker    : aliased Connections_Server (Factory'Access, 0);
   Reference : Handle;

   QoS : constant QoS_Level := At_Most_Once;

begin
   --  read config file(s).
   L.Log_Message (Debug, "MQTT2rrd started (client id = '" & Client_Name & "')");

   if Policy_DB.Get_Default_Loglevel = Debug then
      Trace_On (Factory  => Factory,
                Received => GNAT.Sockets.Server.Trace_Decoded,
                Sent     => GNAT.Sockets.Server.Trace_Decoded);
   else
      Trace_Off (Factory);
   end if;


   Set (Reference,
        new MQTT_Client (Listener             => Broker'Unchecked_Access,
                         Input_Size           => 80,
                         Output_Size          => 80,
                         Max_Subscribe_Topics => 20));

   --  connect to broker

   TT:
   declare
      Client : MQTT_Client renames MQTT_Client (Ptr (Reference).all);
      Sub_Id : Packet_Identification (QoS);
      Sub_Nr : Packet_Identifier := 24; -- arbitrary, may be anything
   begin
      if QoS > At_Least_Once then
         Sub_Id.ID := 24; -- arbitrary
      end if;

      Set_Overlapped_Size (Client, 4); -- One response packet (4 bytes) is
                                       -- queued for send without blocking
                                       -- receiving

      Connect (Broker, Client'Unchecked_Access, Broker_Name, Broker_Port);
      while not Is_Connected (Client) loop -- busy waiting
         delay 0.01;
      end loop;
      L.Log_Message (Debug, Client_Name & " connected to " & Broker_Name & ':' & Broker_Port'Img);
      Send_Connect (Client, Client_Name);


      --  subscribe to configured patterns
      declare
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

         Rrdserver_Default : constant String :=
           Config.Value_Of (Cfg, "defaults", "rrdserver", "localhost");
         Rrdport_Default : constant Natural :=
           Config.Value_Of (Cfg, "defaults", "rrdport", 13900);
         Rrdfile_Default : constant String :=
           Config.Value_Of (Cfg, "defaults", "rrdfile", "rrdfile.rrd");
      begin
         Remove_Section ("broker");
         Remove_Section ("logs");
         Remove_Section ("defaults");
         L.Log_Message (Debug, "0");
         L.Log_Message (Debug, "default_server = " & Rrdserver_Default);
         L.Log_Message (Debug, "default_port = " & Image(Rrdport_Default));
         L.Log_Message (Debug, "default_file = " & Rrdfile_Default);

         for P of Patterns loop
            Rrd.Rrdserv.Insert (P, Config.Value_Of (Cfg, P, "rrdserver", Rrdserver_Default));
            L.Log_Message (Debug, "rrdserver("&P&")="&Rrd.Rrdserv(P));
            Rrd.Rrdport.Insert (P, Config.Value_Of (Cfg, P, "rrdport", Rrdport_Default));
            L.Log_Message (Debug, "rrdport("&P&")="&Image(Rrd.Rrdport(P)));
            Rrd.Rrdfile.Insert (P, Config.Value_Of (Cfg, P, "rrdfile", Rrdfile_Default));
            L.Log_Message (Debug, "rrdfile("&P&")="&Rrd.Rrdfile(P));
            Rrd.Rrdname.Insert (P, Config.Value_Of (Cfg, P, "rrdname", P));
            L.Log_Message (Debug, "rrdname("&P&")="&Rrd.Rrdname(P));

            L.Log_Message (Info, "subscribe to " & P);
            Send_Subscribe (Client, Sub_Nr, P, QoS);
         end loop;
      end;
   end TT;

   loop
      --  wait for incoming message


      --  on reception handle as configured


      --  send the transformed message to rrd

      null;
   end loop;

exception
when E : others =>
   L.Log_Message (Error, "Error: " & Exception_Information (E));

end MQTT2rrd;
