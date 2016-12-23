--  options for aq_pub and aq_sub
with Ada.Command_Line;             use Ada.Command_Line;
with GNAT.Strings;                 use GNAT.Strings;
with GNAT.Sockets;                 use GNAT.Sockets;
with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;

package Opt_Cli_Client is
   type Client_Type is (Publisher, Subscriber);
   Prg_Type : constant Client_Type :=
     (if Command_Name(Command_Name'Last-5..Command_Name'Last) = "aq_pub" then
        Publisher
      else Subscriber);
   --

   Port_Int        : aliased Integer;
   Topic_Text      : aliased String_Access;
   Message_Text    : aliased String_Access;
   Server_Name     : aliased String_Access  := new String'("localhost");
   Client_Name     : aliased String_Access  :=
     (if Prg_Type = Publisher then new String'("aq_pub") else new String'("aq_sub"));
   QoS_Int         : aliased Integer;
   Verbosity_Level : aliased Integer;
   --
   Port : GNAT.Sockets.Port_Type := 1833;
   QoS  : QoS_Level              := At_Most_Once;
   procedure Set_Options;
end Opt_Cli_Client;
