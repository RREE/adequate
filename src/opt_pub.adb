--  options for aq_pub

with Ada.Command_Line;             use Ada.Command_Line;
with GNAT.Command_Line;            use GNAT.Command_Line;
with GNAT.Exception_Traces;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Exceptions;               use Ada.Exceptions;
with ALog;                         use ALog;
with Log;                          use Log;

package body Opt_Pub is
   Cfg_Pub : Command_Line_Configuration;
   Stop : exception;

   procedure Set_Options
   is
   begin
      -- GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
      Define_Switch (Cfg_Pub, Port'Access, "-p:",
                     Long_Switch => "--port=",
                     Initial     => 1883,
                     Default     => 1883,
                     Argument    => "INT",
                     Help        => "MQTT server port (default: 1883).");

      Define_Switch (Cfg_Pub, Server_Name_Text'Access, "-H:",
                     Long_Switch => "--host=",
                     Argument    => "FQN|IP",
                     Help        => "The MQTT server name or IP address (default: localhost).");

      Define_Switch (Cfg_Pub, Topic_Text'Access, "-t:",
                     Long_Switch => "--topic=",
                     Argument    => """TEXT""",
                     Help        => "The MQTT topic on which to publish the message.");

      Define_Switch (Cfg_Pub, Message_Text'Access, "-m:",
                     Long_Switch => "--message=",
                     Argument    => """TEXT""",
                     Help        => "A single message to send.");

      Define_Switch (Cfg_Pub, "-h", Long_Switch => "--help", Help => "show this text");
      Set_Usage (Cfg_Pub,
                 Usage => "[switches], topic and message must not be empty.",
                 Help  => "simple MQTT client to publish messages.");
      Getopt (Cfg_Pub);

      if Topic_Text.all = "" or else Message_Text.all = "" then
         Display_Help (Cfg_Pub);
      end if;
      L.Log_Message (Debug,
                     "options set to" & Ascii.LF &
                       "port     :" & Port'Img & Ascii.LF &
                       "server   : " & Server_Name_Text.all & Ascii.LF &
                       "topic    : " & Topic_Text.all & Ascii.LF &
                       "message  : " & Message_Text.all & Ascii.LF);
   exception
   when Error : GNAT.Command_Line.Exit_From_Command_Line => null;
   when Error : GNAT.Command_Line.Invalid_Switch =>
      Put_Line (Standard_Error, "invalid switch");
      Display_Help (Cfg_Pub);
      Set_Exit_Status (Failure);
      raise Stop;
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
   end Set_Options;

end Opt_Pub;
