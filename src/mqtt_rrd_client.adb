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

-- 2016-12: R. Ebert, use alog instead of direct Text_IO

with Ada.Exceptions;
with Alog;                         use Alog;
with Alog.Logger;                  use Alog.Logger;
with Logs;                         use Logs;
with GNAT.Sockets;                 use GNAT.Sockets;
with GNAT.Sockets.Server;          use GNAT.Sockets.Server;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Rrd;

package body MQTT_Rrd_Client is

   procedure On_Connect_Accepted (Pier            : in out MQTT_Client;
                                  Session_Present : Boolean)
   is
   begin
      L.Log_Message (Info, "MQTT connect accepted");
   end On_Connect_Accepted;


   procedure On_Connect_Rejected (Pier     : in out MQTT_Client;
                                  Response : Connect_Response)
   is
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
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;
   begin
      L.Log_Message (Info, "received " & Topic & " = " & Image (Message));
      On_Publish (MQTT_Pier (Pier),
                  Topic,
                  Message,
                  Packet,
                  Duplicate,
                  Retain);

      declare
         Rrdserv : constant String  := Rrd.Rrdserv (Topic);
         Rrdport : constant Natural := Rrd.Rrdport (Topic);
         Rrdfile : constant String  := Rrd.Rrdfile (Topic);
         Rrdname : constant String  := Rrd.Rrdname (Topic);

         Value_Str : constant String := Image (Message);

         Message : constant String := "update " & Rrdfile & " -t " & Rrdname
           & " N:" & Value_Str;
      begin
         Address.Addr := Addresses (Get_Host_By_Name (Rrdserv));
         Address.Port := Port_Type (Rrdport);
         Create_Socket (Socket);
         Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
         Connect_Socket (Socket, Address);
         Channel := Stream(Socket);
         String'Write (Channel, Message);
         Close_Socket (Socket);

         L.Log_Message (Info, "sending '" & Message & "' to " & Rrdserv & ':' & Image (Rrdport));
      end;

   exception
   when E : others =>
      L.Log_Message (Error, Ada.Exceptions.Exception_Information (E));
      raise;
   end On_Publish;


end MQTT_Rrd_Client;
