--  Daemon process in Linux
with Ada.Command_Line;
with POSIX.Process_Identification;
with POSIX.Unsafe_Process_Primitives;
-- with POSIX.Process_Primitives;

with MQTT2Influx;

procedure Mqtt2influxd is
   Stop_Successfully : exception;
begin
   --
   -- fork the process
   --
   declare
      use POSIX.Unsafe_Process_Primitives;
      use POSIX.Process_Identification;
   begin
      if Fork /= Null_Process_ID then
         raise Stop_Successfully;
      end if;

      if Fork /= Null_Process_ID then
         raise Stop_Successfully;
      end if;
   end;

   --
   -- change file mode mask
   --
   null;

   --
   -- open logs for writing
   --
   -- done in Alog


   --
   -- create session id
   --


   --
   -- change working dir
   --


   --
   -- close standard file descriptors
   --
   -- not needed as there is not Ada.Text_IO

   --
   -- call actual daemon code
   --

   MQTT2Influx;

exception
   when Stop_Successfully =>
   declare
      use Ada.Command_Line;
   begin
      Set_Exit_Status (Success);
   end;

end Mqtt2influxd;
