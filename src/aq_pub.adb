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

with Opt;                          use Opt;
with MQTT_Clients;                 use MQTT_Clients;

procedure Aq_Pub is

   Factory   : aliased Connections_Factory;
   Server    : aliased Connections_Server (Factory'Access, 0);
   Reference : Handle;

begin
   Opt.Set_Options;

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
      Pub_Id : Packet_Identification (QoS);
   begin
      L.Log_Message (Debug, "MQTT client '" & Client_Name.all & "' started");
      if QoS > At_Least_Once then
         Pub_Id.Id := 14; -- arbitrary
      end if;
      Set_Overlapped_Size (Client, 4); -- One response packet
      Connect (Server,
               Client'Unchecked_Access,
               Server_Name.all,
               Port);
      while not Is_Connected (Client) loop -- busy waiting
         delay 0.01;
      end loop;
      L.Log_message (Debug, "MQTT client '" & Client_Name.all &
                       "' connected to '" & Server_Name.all & "'");
      Send_Connect (Client, Client_Name.all);

      Send_Publish (Client,
                    Topic   => Opt.Topic_Text.all,
                    Message => Opt.Message_Text.all,
                    Packet  => Pub_Id);
      delay 0.1;
      Send_Disconnect (Client);
   end;

exception
when Error : others =>
   Ada.Text_IO.Put_Line ("Error: " & Exception_Information (Error));

end Aq_Pub;
