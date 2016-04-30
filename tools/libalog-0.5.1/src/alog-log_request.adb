--
--  Copyright (c) 2009,
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

package body Alog.Log_Request is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   function Create
     (ID      : Task_Id   := Current_Task;
      Source  : String    := "";
      Level   : Log_Level := Debug;
      Message : String)
      return Instance
   is
   begin
      return Instance'(Caller_ID => ID,
                       Source    => To_Unbounded_String (Source),
                       Level     => Level,
                       Message   => To_Unbounded_String (Message));
   end Create;

   -------------------------------------------------------------------------

   function Get_Caller_ID (Request : Instance) return Task_Id is
   begin
      return Request.Caller_ID;
   end Get_Caller_ID;

   -------------------------------------------------------------------------

   function Get_Log_Level (Request : Instance) return Log_Level is
   begin
      return Request.Level;
   end Get_Log_Level;

   -------------------------------------------------------------------------

   function Get_Message (Request : Instance) return String is
   begin
      return To_String (Request.Message);
   end Get_Message;

   -------------------------------------------------------------------------

   function Get_Source (Request : Instance) return String is
   begin
      return To_String (Request.Source);
   end Get_Source;

end Alog.Log_Request;
