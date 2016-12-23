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

with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Calendar;

with Alog.Log_Request;

--  Alog facilities package. Provides common data and methods used by all
--  facilities.
package Alog.Facilities is

   use Ada.Strings.Bounded;
   use Ada.Strings.Unbounded;

   type Instance is abstract tagged limited private;
   --  Abstract type facility instance. All facilities in the Alog framework
   --  must implement this type.

   subtype Class is Instance'Class;

   type Handle is access all Class;

   function "="
     (Left  : Handle;
      Right : Handle) return Boolean;
   --  Equal function.

   procedure Set_Name
     (Facility : in out Class;
      Name     :        String);
   --  Set facility name.

   function Get_Name (Facility : Class) return String;
   --  Get facility name.

   function Get_Timestamp
     (Facility : Class;
      Time     : Ada.Calendar.Time := Ada.Calendar.Clock)
      return String;
   --  Creates a timestamp and returns it as String. If no Time is given, the
   --  current time is used.

   procedure Process
     (Facility : Class;
      Request  : Log_Request.Instance);
   --  Process a log request.

   procedure Write
     (Facility : Instance;
      Level    : Log_Level := Info;
      Msg      : String) is abstract;
   --  Write message with specified log level. This procedure must be
   --  implemented by all facilities.

   procedure Toggle_Write_Timestamp
     (Facility : in out Class;
      State    :        Boolean);
   --  Enable/disable whether a timestamp is written for log messages.

   procedure Toggle_UTC_Timestamp
     (Facility : in out Class;
      State    :        Boolean);
   --  Enable/disable UTC timestamps for log messages.

   function Is_Write_Timestamp (Facility : Class) return Boolean;
   --  Returns the current value of Write_Timestamp.

   function Is_UTC_Timestamp (Facility : Class) return Boolean;
   --  Returns True if the timestamp of the facility is written in UTC time.

   procedure Set_Timestamp_Format
     (Facility : in out Class;
      Format   :        String);
   --  Set timestamp format. The format follows the GNU Date specification with
   --  some GNAT specific extensions. See the GNAT.Calendar.Time_IO specs for
   --  details about the accepted directives. The procedure raises an
   --  Invalid_Timestamp_Format exception if the given format is incorrect.
   --  If no specific format is set, '%b %d %Y %T' is used as default.

   procedure Toggle_Write_Loglevel
     (Facility : in out Class;
      State    :        Boolean);
   --  Enable/disable whether the loglevel is written for log messages.

   function Is_Write_Loglevel (Facility : Class) return Boolean;
   --  Returns the current value of Write_Loglevel.

   procedure Toggle_Write_Source
     (Facility : in out Class;
      State    :        Boolean);
   --  Enable/disable whether the source of the message is logged.

   function Is_Write_Source (Facility : Class) return Boolean;
   --  Returns True if writing of log message sources is enabled.

   procedure Setup (Facility : in out Instance) is null;
   --  Each facility must provide a Setup-procedure. These procedures are
   --  called by Logger instances when attaching Facilities. All needed
   --  operations prior to writing log messages should be done here.

   procedure Teardown (Facility : in out Instance) is null;
   --  Each facility must provide a Teardown-procedure. These procedures are
   --  called by Logger instances when detaching Facilities or when the logger
   --  object gets out of scope.

   package BS_Path is new Generic_Bounded_Length (Max_Path_Length);
   use BS_Path;
   --  Bounded string with length Max_Path_Length. Used in methods which
   --  involve filesystem operations.

   Invalid_Timestamp_Format : exception;

private

   type Instance is abstract tagged limited record
      Name             : Unbounded_String
        := To_Unbounded_String (Ada.Command_Line.Command_Name);
      --  Facility Name. Defaults to command-name (first argument). If multiple
      --  facilities are used, names must be set differently.

      Timestamp_Format : Unbounded_String
        := To_Unbounded_String ("%b %d %Y %T");
      --  Default timestamp format to use in this facility.

      Write_Timestamp  : Boolean := True;
      --  If True, a timestamp is written with the log message.

      UTC_Timestamp    : Boolean := False;
      --  If True, the timestamp is written in UTC time.
      --  (log message timestamps are written timezone-dependent).

      Write_Loglevel   : Boolean := False;
      --  If True, the loglevel associated with the log message is written.

      Write_Source     : Boolean := True;
      --  If True, the source of a log message is prepended to the message.
   end record;

end Alog.Facilities;
