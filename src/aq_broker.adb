with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;

with Opt_Broker;                   use Opt_Broker;
with MQTT_Broker; --                 use MQTT_Broker;


procedure Aq_Broker is
begin
   Opt_Broker.Set_Options;

   MQTT_Broker.Start_Broker;

   loop
      delay Duration (Opt_Broker.Sys_Interval);
      MQTT_Broker.Publish_Broker_Internals;
   end loop;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Aq_Broker;
