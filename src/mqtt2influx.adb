with Ada.Text_IO;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Command_Line;             use Ada.Command_Line;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
-- with POSIX.User_Database;
with POSIX.Process_Identification;
with POSIX.Unsafe_Process_Primitives;

with Alog;                         use Alog;
with Alog.Policy_DB;
with Alog.Logger;
with Logs;                         use Logs;

with GNAT.Sockets.MQTT;            use GNAT.Sockets.MQTT;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with Strings_Edit.Integers;        use Strings_Edit.Integers;

with MQTT_Influx_Client;           use MQTT_Influx_Client;

with Opt_M2I;                      use Opt_M2I;
with Config;
with Influx;


procedure MQTT2Influx is
   --  config file
   Cfg : Config.Configuration;

   My_Version  : constant String  := "V0.1";


   Factory   : aliased Connections_Factory;
   Broker    : aliased Connections_Server (Factory'Access, 0);
   QoS       : constant QoS_Level := At_Most_Once;
   Myself    : aliased MQTT_Client (Listener             => Broker'Unchecked_Access,
                                    Input_Size           => 800,
                                    Output_Size          => 800,
                                    Max_Subscribe_Topics => 500);

   Sub_Id : Packet_Identification (QoS);
   Sub_Nr : Packet_Identifier := 24; -- arbitrary, may be anything

   --
   -- topic handling
   --
   type Str_Token is record
      Start_Ptr : Natural;
      End_Ptr   : Natural;
   end record;
   subtype Topic_Range is Positive range 1 .. 500;
   type Level_Array is array (Topic_Range) of Str_Token;
   Levels : Level_Array;
   Level_Count : Topic_Range;


   procedure Parse_Topic (Topic : String) is
      use Ada.Strings.Fixed;

      Idx : Topic_Range renames Level_Count;
      Pos : Natural := Topic'First;
   begin
      Levels := (others => (1, 0));
      Idx := 1;

      loop
         Levels(Idx).Start_Ptr := Pos;
         Pos := Index (Topic(Pos .. Topic'Last), "/");
         exit when Pos = 0;
         Levels(Idx).End_Ptr := Pos-1;
         Pos := Pos + 1;
         Idx := Idx + 1;
      end loop;
      Levels(Idx).End_Ptr := Topic'Last;
   end Parse_Topic;


   function Level_From_Esc_Seq (Topic : String; Esc : String) return String
   is
      use Ada.Characters.Handling;
      Device : constant String (1..Esc'Length) := Esc;
      Idx : Integer;
   begin
      if Device(1) ='%' and then
        (Is_Digit(Device(2)) or else (Device(2) = '-' and then Is_Digit(Device(3))))
      then
         Idx := Integer'Value (Device(2..Device'Last));
         if Idx in 1 .. Level_Count then
            return Topic (Levels(Idx).Start_Ptr .. Levels(Idx).End_Ptr);
         elsif -Idx in 1 .. Level_Count then
            Idx := Level_Count + 1 + Idx;
            return Topic (Levels(Idx).Start_Ptr .. Levels(Idx).End_Ptr);
         else
            return Esc;
         end if;
      else
         return Esc;
      end if;
   end Level_From_Esc_Seq;


   --  procedure Disp_Levels (Topic : String) is
   --  begin
   --     for I in 1 .. Level_Count loop
   --        L.Log_Message (Debug, "topic : " &Topic&", level ="&I'Img& ", level = " &
   --                         Topic(Levels(I).Start_Ptr .. Levels(I).End_Ptr));
   --     end loop;
   --  end Disp_Levels;

begin
   L.Log_Message (Debug, "MQTT2Influx started (version " & My_Version & "')");
   Opt_M2I.Set_Options;
   Cfg := Config.Init (Opt_M2I.Config_File.all,
                       Case_Sensitive   => False,
                       On_Type_Mismatch => Config.Print_Warning);

   Logger.Clear (Logs.L);

   if Opt_M2I.Daemon then
      Logs.Set_Log_Dest_To ("syslog");

      Ada.Text_IO.Put_Line ("vor erstem fork");
      delay 2.0;

      Fork_Daemon:
      declare
         use POSIX.Unsafe_Process_Primitives;
         use POSIX.Process_Identification;
      begin
         if Fork /= Null_Process_ID then
            raise Opt_M2I.Stop_Success;
         end if;
         Ada.Text_IO.Put_Line ("nach erstem fork");

         --  delay 2.0;
         --  if Fork /= Null_Process_ID then
         --     raise Opt_M2I.Stop_Success;
         --  end if;
         --  Ada.Text_IO.Put_Line ("nach zweitem fork");
      end Fork_Daemon;
   else
      declare
         Logdev : constant String := Config.Value_Of (Cfg, "mqtt2influx", "logdevice", "stderr");
      begin
         Logs.Set_Log_Dest_To (Logdev);
      end;
   end if;


   zzz:
   declare
      My_Name     : constant String  := Config.Value_Of (Cfg, "broker", "client_id", "mqtt2influx");
      Broker_Name : constant String  := Config.Value_Of (Cfg, "broker", "servername", "localhost");
      Port_Int    : constant Integer := Config.Value_Of (Cfg, "broker", "port", 1883);
      Broker_Port : constant GNAT.Sockets.Port_Type := GNAT.Sockets.Port_Type (Port_Int);


      Influxserver_Default : constant String :=
        Config.Value_Of (Cfg, "influx", "server", "localhost");
      Influxport_Default : constant Natural :=
        Config.Value_Of (Cfg, "influx", "port", 8086);
      Influxdb_Default : constant String :=
        Config.Value_Of (Cfg, "influx", "db", "testdata_db");
      Topic_As_Attribute_Default : constant String :=
        Config.Value_Of (Cfg, "defaults", "topic_as_attribute", "false");
      Device_As_Attribute_Default : constant String :=
        Config.Value_Of (Cfg, "defaults", "device_as_attribute", "false");
      Location_As_Attribute_Default : constant String :=
        Config.Value_Of (Cfg, "defaults", "location_as_attribute", "false");
      Device_Default : constant String :=
        Config.Value_Of (Cfg, "defaults", "device", "unknown_device");
      Location_Default : constant String :=
        Config.Value_Of (Cfg, "defaults", "location", "unknown_location");


      use Config;
      use Config.String_Vector;
      Patterns : Section_List;

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

      --  read configured patterns
      Patterns := Read_Sections (Cfg);
      Remove_Section ("broker");
      Remove_Section ("mqtt2influx");
      Remove_Section ("influx");
      Remove_Section ("defaults");
      L.Log_Message (Debug, "Influx server          = " & Influxserver_Default);
      L.Log_Message (Debug, "Influx port            = " & Image(Influxport_Default));
      L.Log_Message (Debug, "Influx DB name         = " & Influxdb_Default);
      L.Log_Message (Debug, "default show topic     = " & Topic_As_Attribute_Default);
      L.Log_Message (Debug, "default show device    = " & Device_As_Attribute_Default);
      L.Log_Message (Debug, "default show location  = " & Location_As_Attribute_Default);
      L.Log_Message (Debug, "default device         = " & Device_Default);
      L.Log_Message (Debug, "default location       = " & Location_Default);

      for P of Patterns loop
         Parse_Topic(P);
         -- Disp_Levels(P);

         Influx.Influxserv.Insert (P, Config.Value_Of (Cfg, P, "influxdbserver", Influxserver_Default));
         L.Log_Message (Debug, "influxserver("&P&")="&Influx.Influxserv(P));
         Influx.Influxport.Insert (P, GNAT.Sockets.Port_Type (Config.Value_Of (Cfg, P, "influxdbport", Influxport_Default)));
         -- L.Log_Message (Debug, "influxport("&P&")="&Image(Natural(Influx.Influxport(P))));
         Influx.Influxdb.Insert (P, Config.Value_Of (Cfg, P, "influxdb", Influxdb_Default));
         L.Log_Message (Debug, "influxdb("&P&")="&Influx.Influxdb(P));


         Influx.Influxname.Insert (P, Config.Value_Of (Cfg, P, "influxname", P));
         L.Log_Message (Debug, "influxname("&P&")="&Influx.Influxname(P));

         Show_Topic:
         declare
            use Ada.Strings.Fixed;
            use Ada.Strings.Maps.Constants;

            Show : constant Boolean :=
              Translate (Config.Value_Of (Cfg, P, "topic_as_attribute", Topic_As_Attribute_Default),
                         Lower_Case_Map) = "true";
         begin
            Influx.Show_Topic.Insert (P, Show);
            L.Log_Message (Debug, "show_topic("&P&")="&(if Influx.Show_Topic(P) then "True" else "False"));
         end Show_Topic;
         Show_Device:
         declare
            use Ada.Strings.Fixed;
            use Ada.Strings.Maps.Constants;

            Show : constant Boolean :=
              Translate (Config.Value_Of (Cfg, P, "device_as_attribute", Device_As_Attribute_Default),
                         Lower_Case_Map) = "true";
            Device : constant String := Config.Value_Of (Cfg, P, "device", Device_Default);
         begin
            Influx.Show_Device.Insert (P, Show);
            L.Log_Message (Debug, "show_device("&P&")="&Boolean'Image(Influx.Show_Device(P)));
            -- (if Influx.Show_Device(P) then "True" else "False"));
            Influx.Device.Insert (P, Level_From_Esc_Seq (P, Device));
            L.Log_Message (Debug, "device("&P&")="&Influx.Device(P));
         end Show_Device;
         Show_Location:
         declare
            use Ada.Strings.Fixed;
            use Ada.Strings.Maps.Constants;

            Show : constant Boolean :=
              Translate (Config.Value_Of (Cfg, P, "location_as_attribute", Device_As_Attribute_Default),
                         Lower_Case_Map) = "true";
            Location : constant String := Config.Value_Of (Cfg, P, "location", Location_Default);
         begin
            Influx.Show_Location.Insert (P, Show);
            L.Log_Message (Debug, "show_location(" & P & ")=" & Boolean'Image (Influx.Show_Location(P)));

            Influx.Location.Insert (P, Level_From_Esc_Seq (P, Location));
            L.Log_Message (Debug, "location("&P&")="&Influx.Location(P));

         end Show_Location;
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
   end zzz;

   loop
      --  wait for incoming message
      --  on reception handle as configured
      --  send the transformed message to influx
      delay 0.01;
   end loop;


exception

when Opt_M2I.Stop_Success =>
   Set_Exit_Status (Success);

when E : others =>
   Set_Exit_Status (Failure);
   L.Log_Message (Error, "Error (main): " & Exception_Information (E));

end MQTT2Influx;
