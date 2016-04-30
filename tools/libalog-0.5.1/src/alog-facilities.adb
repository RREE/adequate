--
--  Copyright (c) 2008-2012,
--  Reto Buerki, Adrian-Ken Rueegsegger
--
--  This file is part of Alog.
--
--  Alog is free software; you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Public License as published
--  by the Free Software Foundation; either version 2.1 of the License, or
--  (at your option) any later version.
--
--  Alog is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with Alog; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
--  MA  02110-1301  USA
--

with Ada.Calendar.Time_Zones;

with GNAT.Calendar.Time_IO;

with Alog.Policy_DB;

package body Alog.Facilities is

   -------------------------------------------------------------------------

   function "="
     (Left  : Handle;
      Right : Handle)
      return Boolean is
   begin
      return Left.Get_Name = Right.Get_Name;
   end "=";

   -------------------------------------------------------------------------

   function Get_Name (Facility : Class) return String is
   begin
      return To_String (Facility.Name);
   end Get_Name;

   -------------------------------------------------------------------------

   function Get_Timestamp
     (Facility : Class;
      Time     : Ada.Calendar.Time := Ada.Calendar.Clock)
      return String
   is
      use GNAT.Calendar.Time_IO;
   begin
      if Facility.Is_UTC_Timestamp then
         declare
            use type Ada.Calendar.Time;
            use Ada.Calendar.Time_Zones;

            UTC_Offset    : constant Time_Offset :=
              UTC_Time_Offset (Time);
            UTC_Timestamp : constant String      :=
              Image (Date    => Time - Duration (UTC_Offset) * 60,
                     Picture => Picture_String
                       (To_String (Facility.Timestamp_Format)));
         begin
            return UTC_Timestamp;
         end;
      else
         declare
            Timestamp : constant String :=
              Image (Date    => Time,
                     Picture => Picture_String
                       (To_String (Facility.Timestamp_Format)));
         begin
            return Timestamp;
         end;
      end if;
   end Get_Timestamp;

   -------------------------------------------------------------------------

   function Is_UTC_Timestamp (Facility : Class) return Boolean is
   begin
      return Facility.UTC_Timestamp;
   end Is_UTC_Timestamp;

   -------------------------------------------------------------------------

   function Is_Write_Loglevel (Facility : Class) return Boolean is
   begin
      return Facility.Write_Loglevel;
   end Is_Write_Loglevel;

   -------------------------------------------------------------------------

   function Is_Write_Source (Facility : Class) return Boolean is
   begin
      return Facility.Write_Source;
   end Is_Write_Source;

   -------------------------------------------------------------------------

   function Is_Write_Timestamp (Facility : Class) return Boolean is
   begin
      return Facility.Write_Timestamp;
   end Is_Write_Timestamp;

   -------------------------------------------------------------------------

   procedure Process
     (Facility : Class;
      Request  : Log_Request.Instance)
   is
      Message : Unbounded_String;
      Level   : constant Log_Level := Request.Get_Log_Level;
      Msg     : constant String    := Request.Get_Message;
      Source  : constant String    := Request.Get_Source;
   begin
      if Policy_DB.Accept_Dst
        (Identifier => Facility.Get_Name,
         Level      => Level)
      then
         if Facility.Is_Write_Timestamp then
            Append (Source   => Message,
                    New_Item => Facility.Get_Timestamp & " ");
         end if;

         if Facility.Is_Write_Loglevel then
            Append (Source   => Message,
                    New_Item => "[" & Log_Level'Image (Level) (1 .. 4) & "] ");
         end if;

         if Source'Length > 0 and then Facility.Is_Write_Source then
            Append (Source   => Message,
                    New_Item => Source & ": ");
         end if;

         Append (Source   => Message,
                 New_Item => Msg);

         Facility.Write (Level => Level,
                         Msg   => To_String (Message));
      end if;
   end Process;

   -------------------------------------------------------------------------

   procedure Set_Name
     (Facility : in out Class;
      Name     :        String)
   is
   begin
      Facility.Name := To_Unbounded_String (Name);
   end Set_Name;

   -------------------------------------------------------------------------

   procedure Set_Timestamp_Format
     (Facility : in out Class;
      Format   :        String)
   is
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Validate_Format :
      declare
         Image : constant String := GNAT.Calendar.Time_IO.Image
           (Date    => Now,
            Picture => GNAT.Calendar.Time_IO.Picture_String (Format));
         pragma Unreferenced (Image);
      begin
         Facility.Timestamp_Format := To_Unbounded_String (Format);
      end Validate_Format;

   exception
      when GNAT.Calendar.Time_IO.Picture_Error =>
         raise Invalid_Timestamp_Format with "Given timestamp format '"
           & Format & "' is invalid";
   end Set_Timestamp_Format;

   -------------------------------------------------------------------------

   procedure Toggle_UTC_Timestamp
     (Facility : in out Class;
      State    :        Boolean)
   is
   begin
      Facility.UTC_Timestamp := State;
   end Toggle_UTC_Timestamp;

   -------------------------------------------------------------------------

   procedure Toggle_Write_Loglevel
     (Facility : in out Class;
      State    :        Boolean)
   is
   begin
      Facility.Write_Loglevel := State;
   end Toggle_Write_Loglevel;

   -------------------------------------------------------------------------

   procedure Toggle_Write_Source
     (Facility : in out Class;
      State    :        Boolean)
   is
   begin
      Facility.Write_Source := State;
   end Toggle_Write_Source;

   -------------------------------------------------------------------------

   procedure Toggle_Write_Timestamp
     (Facility : in out Class;
      State    :        Boolean)
   is
   begin
      Facility.Write_Timestamp := State;
   end Toggle_Write_Timestamp;

end Alog.Facilities;
