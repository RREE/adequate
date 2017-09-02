--                                                                    --
--  package MQTT_Clients            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2016       --
--                                                                    --
--                                Last revision :  18:59 21 Mar 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

-- 2016-12: R. Ebert, use alog instead of Text_IO

with Ada.Exceptions;
-- with Ada.Calendar; -- Real_Time;
-- with Ada.Calendar.Formatting;
-- with Ada.Text_IO;
-- with Ada.Strings.Fixed;
-- with Ada.Strings.Maps;

with Alog;                         use Alog;
with Alog.Logger;                  use Alog.Logger;
with Logs;                         use Logs;

with GNAT.Sockets;                 use GNAT.Sockets;
with Strings_Edit;
-- with Strings_Edit.Streams;
with Strings_Edit.Integers;        use Strings_Edit.Integers;

with Influx;


package body MQTT_Influx_Client is


   procedure On_Connect_Accepted (Pier            : in out MQTT_Client;
                                  Session_Present : Boolean)
   is
      pragma Unreferenced (Pier);
      pragma Unreferenced (Session_Present);
   begin
      L.Log_Message (Info, "MQTT connect accepted");
   end On_Connect_Accepted;


   procedure On_Connect_Rejected (Pier     : in out MQTT_Client;
                                  Response : Connect_Response)
   is
      pragma Unreferenced (Pier);
   begin
      L.Log_Message (Warning, "Connect rejected " & Image (Response));
   end On_Connect_Rejected;


   procedure On_Publish (Pier      : in out MQTT_Client;
                         Topic     : String;
                         Message   : Stream_Element_Array;
                         Packet    : Packet_Identification;
                         Duplicate : Boolean;
                         Retain    : Boolean)
   is
      use Strings_Edit;
      use Strings_Edit.Integers;
      use Influx;
      -- use Ada.Strings.Maps;

      To_Influx : Socket_Type;
      Address : Sock_Addr_Type;
      Channel : GNAT.Sockets.Stream_Access;

      Output   : String (1 .. 500);
      Content  : String (1 .. 400);
      Out_Ptr  : Natural         := 1;
      Cont_Ptr : Natural         := 1;
      -- Ptr      : Natural;
      CRLF     : constant String := ASCII.CR & ASCII.LF;

      --  MQTT_Name_Set : constant Character_Set := To_Set ("0123456789") or
      --    To_Set ("abcdefghijklmnopqrstuvwxyz") or To_Set ("ABCDEFGHIJKLMNOPQRSTUVWXYZ");


   begin
      L.Log_Message (Info, "received " & Topic & " = " ); -- & Image (Message));
      On_Publish (MQTT_Pier (Pier), Topic, Message, Packet, Duplicate, Retain);

      GNAT.Sockets.Initialize;  -- initialize a specific package
      Create_Socket (To_Influx);
      Set_Socket_Option (To_Influx, Socket_Level, (Reuse_Address, True));
      Address.Addr := Addresses (Get_Host_By_Name (Influxserv(Topic)), 1);
      Address.Port := Influxport(Topic);

      Connect_Socket (To_Influx, Address);
      L.Log_Message (Info, "connected to influx" );
      Channel := Stream (To_Influx);

      Gen_Output:
      declare
         use Strings_Edit;
         Port : constant Natural := Natural(Port_Type'(Influxport(Topic)));
         Msg_Str : String (1 .. Message'Length);
         for Msg_Str'Address use Message'Address;
      begin
         L.Log_Message (Info, "Message = '"&Msg_Str&''');

         Put (Output, Out_Ptr, "POST /write?db=");
         Put (Output, Out_Ptr, Influxdb(Topic));
         Put (Output, Out_Ptr, " HTTP/1.1");
         Put (Output, Out_Ptr, CRLF);
         Put (Output, Out_Ptr, "Host: ");
         Put (Output, Out_Ptr, Influxserv(Topic));
         Put (Output, Out_Ptr, ':');
         Put (Output, Out_Ptr, Image(Port));
         Put (Output, Out_Ptr, CRLF);
         Put (Output, Out_Ptr, "Accept: */*");
         Put (Output, Out_Ptr, CRLF);
         Put (Output, Out_Ptr, "Content-Type: application/x-www-form-urlencoded");
         Put (Output, Out_Ptr, CRLF);
         Put (Output, Out_Ptr, "Content-Length: ");

         Put (Content, Cont_Ptr, Influxname(Topic));
         if Show_Topic (Topic) then
            Put (Content, Cont_Ptr, ",topic=""");
            Put (Content, Cont_Ptr, Topic);
            Put (Content, Cont_Ptr, '"');
         end if;
         if Show_Device (Topic) then
            Put (Content, Cont_Ptr, ",device=""");
            Put (Content, Cont_Ptr, Device(Topic));
            Put (Content, Cont_Ptr, '"');
         end if;
         if Show_Location (Topic) then
            Put (Content, Cont_Ptr, ",location=""");
            Put (Content, Cont_Ptr, Location(Topic));
            Put (Content, Cont_Ptr, '"');
         end if;
         Put (Content, Cont_Ptr, " value=");
         Put (Content, Cont_Ptr, Msg_Str);
         Cont_Ptr := Cont_Ptr - 1;
         L.Log_Message (Info, "Content = '"&Content(1..Cont_Ptr)&''');

         Put (Output, Out_Ptr, Image (Cont_Ptr));
         Put (Output, Out_Ptr, CRLF);
         Put (Output, Out_Ptr, CRLF);
         Put (Output, Out_Ptr, Content(1..Cont_Ptr));

         Out_Ptr := Out_Ptr - 1;
         String'Write (Channel, Output(1..Out_Ptr));
         L.Log_Message (Info, "sent '"&Output(1..Out_Ptr)&''');
      end Gen_Output;
      Close_Socket (To_Influx);
      L.Log_Message (Info, "closed Influx socket" );


   exception
   when E : others =>
      L.Log_Message (Error, "(On_Publish)" & Ada.Exceptions.Exception_Information (E));
      raise;
   end On_Publish;

end MQTT_Influx_Client;
