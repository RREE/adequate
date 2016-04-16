with Ada.Text_IO;
with Ada.Exceptions;               use Ada.Exceptions;
with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;
with GNAT.Sockets.MQTT.Server;     use GNAT.Sockets.MQTT.Server;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Handles;  use GNAT.Sockets.Server.Handles;

with Opt_Pub;                      use Opt_Pub;
with MQTT_Clients;                 use Mqtt_Clients;

procedure Aq_Pub is
   Factory   : aliased Connections_Factory;
   Server    : aliased Connections_Server (Factory'Access, 0);
   Reference : Handle;

begin
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

      procedure Test_1 is
      begin
         Set_Overlapped_Size (Client, 4); -- One response packet
         Connect (Server,
                  Client'Unchecked_Access,
                  Opt_Pub.Server_Name_Text.all,
                  --MQTT_Port
                  GNAT.Sockets.Port_Type (Port));
         while not Is_Connected (Client) loop -- Busy waiting
            delay 0.1;
         end loop;
         Ada.Text_IO.Put_Line ("MQTT client connected to " &
                                 Opt_Pub.Server_Name_Text.all);
         Send_Connect (Client, "aq_pub");

         Send_Subscribe (Client,
                         12,
                         "$SYS/broker/uptime" / "$SYS/broker/load/#",
                         (At_Least_Once, Exactly_Once)
                        );
         delay 1.0;
         Send_Ping (Client);
         delay 5.0;
         Send_Unsubscribe (Client, 13, "$SYS/broker/uptime" / "??");
         delay 1.0;
         Send_Disconnect (Client);
         delay 1.0;
      end Test_1;

   begin
      Ada.Text_Io.Put_Line ("aq_pub MQTT client started");
      -- Test_1;
      Set_Overlapped_Size (Client, 4); -- One response packet
      Connect (Server,
               Client'Unchecked_Access,
               Opt_Pub.Server_Name_Text.all,
               --MQTT_Port
               GNAT.Sockets.Port_Type (Port));
      while not Is_Connected (Client) loop -- Busy waiting
         delay 0.1;
      end loop;
      Ada.Text_IO.Put_Line ("aq_pub MQTT client connected to " &
                              Opt_Pub.Server_Name_Text.all);
      Send_Connect (Client, "aq_pub");

      Send_Publish (Client, Opt_Pub.Topic_Text.all, Opt_Pub.Message_Text.all, At_Most_Once);

      Send_Disconnect (Client);
   end;

exception
when Error : others =>
   Ada.Text_IO.Put_Line ("Error: " & Exception_Information (Error));

end Aq_Pub;
