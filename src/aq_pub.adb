with Ada.Text_IO;
with Alog;                         use Alog;
with Alog.Logger;
with Alog.Policy_DB;
with Log;                          use Log;

with Ada.Exceptions;               use Ada.Exceptions;
with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;
with GNAT.Sockets.MQTT.Server;     use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;

with Opt_Pub;                      use Opt_Pub;
with MQTT_Clients;                 use MQTT_Clients;

procedure Aq_Pub is

   Factory   : aliased Connections_Factory;
   Server    : aliased Connections_Server (Factory'Access, 0);
   Reference : Handle;

begin
   Alog.Policy_DB.Set_Default_Loglevel (Level => Debug);

   Opt_Pub.Set_Options;

   Trace_On (Factory  => Factory,
             Received => GNAT.Sockets.Server.Trace_Decoded,
             Sent     => GNAT.Sockets.Server.Trace_Decoded);
   Set (Reference,
        new MQTT_Client (Listener             => Server'Unchecked_Access,
                         Input_Size           => 80,
                         Output_Size          => 80,
                         Max_Subscribe_Topics => 20));
   declare
      Client : MQTT_Client renames MQTT_Client (Ptr (Reference).all);

   begin
      L.Log_Message (Debug, "aq_pub MQTT client started");
      -- Test_1;
      Set_Overlapped_Size (Client, 4); -- One response packet
      Connect (Server,
               Client'Unchecked_Access,
               Server_Name_Text.all,
               GNAT.Sockets.Port_Type (Port));
      while not Is_Connected (Client) loop -- busy waiting
         delay 0.01;
      end loop;
      L.Log_message (Debug, "aq_pub MQTT client connected to " &
                              Server_Name_Text.all);
      Send_Connect (Client, "aq_pub");

      Send_Publish (Client,
                    Topic   => Opt_Pub.Topic_Text.all,
                    Message => Opt_Pub.Message_Text.all,
                    Packet  => (Exactly_Once, 14));
      delay 0.1;
      Send_Disconnect (Client);
   end;

exception
when Error : others =>
   Ada.Text_IO.Put_Line ("Error: " & Exception_Information (Error));

end Aq_Pub;
