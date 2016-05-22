--  options for aq_pub and aq_sub
with Ada.Command_Line;             use Ada.Command_Line;
with GNAT.Strings;                 use GNAT.Strings;
with GNAT.Sockets;                 use GNAT.Sockets;
with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;

package Opt_Broker is

   Port_Int        : aliased Integer;
   Sys_Interval     : aliased Integer;
   Verbosity_Level  : aliased Integer;
   Config_File_Name : aliased String_Access := new String'("aq_broker.conf");
   Config_Sys_Dir   : aliased String_Access := new String'("/etc/aq");
   Config_User_Dir  : aliased String_Access := new String'("/home/re/.aq");
   Config_Local_Dir : aliased String_Access := new String'(".");

   --
   Port : GNAT.Sockets.Port_Type := 1833;

   procedure Set_Options;
end Opt_Broker;
