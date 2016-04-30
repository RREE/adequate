--
--  Copyright (c) 2009-2015,
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

with Ada.Task_Identification;
with Ada.Strings.Unbounded;

--  Log request type. Log request objects are used for asynchronous logging and
--  hold all relevant information of a log request.
package Alog.Log_Request is

   use Ada.Task_Identification;

   type Instance is tagged private;
   --  A log request contains all related information to log asynchronously
   --  (Caller identification, loglevel and message).

   Termination_Request : constant Instance;
   --  Special request to signal termination to the consumer.

   function Create
     (ID      : Task_Id   := Current_Task;
      Source  : String    := "";
      Level   : Log_Level := Debug;
      Message : String)
      return Instance;
   --  Create a log request object from the specified parameters.

   function Get_Caller_ID (Request : Instance) return Task_Id;
   --  Return the caller ID of the request object.

   function Get_Source (Request : Instance) return String;
   --  Return the source of the request object.

   function Get_Log_Level (Request : Instance) return Log_Level;
   --  Return the loglevel of the request object.

   function Get_Message (Request : Instance) return String;
   --  Return the log message of the request object.

private

   type Instance is tagged record
      Caller_ID : Task_Id   := Null_Task_Id;
      Source    : Ada.Strings.Unbounded.Unbounded_String;
      Level     : Log_Level := Info;
      Message   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   Termination_Request : constant Instance := (others => <>);

end Alog.Log_Request;
