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

with Ada.Task_Identification;

with Ahven; use Ahven;

with Alog.Log_Request;

package body Log_Request_Tests is

   use Alog;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for log requests");
      T.Add_Test_Routine
        (Routine => Log_Request_Getter'Access,
         Name    => "log request getters");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Log_Request_Getter is
      use Ada.Task_Identification;

      Request : Log_Request.Instance;
      Ref_ID  : constant Task_Id := Current_Task;
      Ref_Src : constant String  := "Foo";
      Ref_Msg : constant String  := "Some log message";
   begin
      Assert (Condition => Request.Get_Caller_ID = Null_Task_Id,
              Message   => "Default ID not Null_Task_Id");
      Assert (Condition => Request.Get_Log_Level = Info,
              Message   => "Default log level not INFO");
      Assert (Condition => Request.Get_Message = "",
              Message   => "Default message not empty");

      Request := Log_Request.Create (ID      => Ref_ID,
                                     Source  => Ref_Src,
                                     Level   => Notice,
                                     Message => Ref_Msg);

      Assert (Condition => Request.Get_Caller_ID = Ref_ID,
              Message   => "Caller ID mismatch");
      Assert (Condition => Request.Get_Source = Ref_Src,
              Message   => "Source mismatch");
      Assert (Condition => Request.Get_Log_Level = Notice,
              Message   => "Log level mismatch");
      Assert (Condition => Request.Get_Message = Ref_Msg,
              Message   => "Message mismatch");
   end Log_Request_Getter;

end Log_Request_Tests;
