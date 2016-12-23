
with Alog;                         use Alog;
with Alog.Logger;
with Alog.Facilities;
with Alog.Policy_DB;
with Logs;                         use Logs;

with Ada.Exceptions;               use Ada.Exceptions;
with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;
with GNAT.Sockets.MQTT.Server;     use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;

with Opt_Cli_Client;               use Opt_Cli_Client;
with MQTT_Cli_Clients;             use MQTT_Cli_Clients;

procedure Aq_Sub is

   Factory   : aliased Connections_Factory;
   Server    : aliased Connections_Server (Factory'Access, 0);
   Reference : Handle;

begin
   Opt_Cli_Client.Set_Options;

   if Policy_DB.Get_Default_Loglevel = Debug then
      Trace_On (Factory  => Factory,
                Received => GNAT.Sockets.Server.Trace_Decoded,
                Sent     => GNAT.Sockets.Server.Trace_Decoded);
   else
      Trace_Off (Factory);
   end if;

   Set (Reference,
        new MQTT_Client (Listener             => Server'Unchecked_Access,
                         Input_Size           => 80,
                         Output_Size          => 80,
                         Max_Subscribe_Topics => 20));

   declare
      Client : MQTT_Client renames MQTT_Client (Ptr (Reference).all);
      Sub_Id : Packet_Identification (QoS);
      Sub_Nr : Packet_Identifier := 24;
   begin
      L.Log_Message (Debug, "MQTT client '" & Client_Name.all & "' started");
      if QoS > At_Least_Once then
         Sub_Id.Id := 24; -- arbitrary
      end if;
      Set_Overlapped_Size (Client, 4); -- One response packet (4
                                       -- bytes) is queued for send
                                       -- without blockking receiving
      Connect (Server,
               Client'Unchecked_Access,
               Server_Name.all,
               Port);
      while not Is_Connected (Client) loop -- busy waiting
         delay 0.01;
      end loop;
      L.Log_Message (Debug, "MQTT client '" & Client_Name.all &
                       "' connected to '" & Server_Name.all & "'");
      Send_Connect (Client, Client_Name.all);

      Send_Subscribe (Client,
                      Sub_Nr,
                      Opt_Cli_Client.Topic_Text.all,
                      QoS);

      loop null; end loop;
   end;

exception
when E : others =>
   L.Log_Message (Alog.Error, "Error: " & Exception_Information (E));

end Aq_Sub;
