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
with Alog.Protected_Containers;

package body Protected_Container_Tests is

   use Alog;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for protected containers");
      T.Add_Test_Routine
        (Routine => Log_Request_List_Get_Put'Access,
         Name    => "log request list get/put");
      T.Add_Test_Routine
        (Routine => Log_Request_List_Clear'Access,
         Name    => "log request list clear");
      T.Add_Test_Routine
        (Routine => Log_Request_List_Done'Access,
         Name    => "log request list done");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Log_Request_List_Clear is
      use Ada.Task_Identification;

      R_List      : Protected_Containers.Log_Request_List;
      Ref_Request : Log_Request.Instance;
      Ref_ID      : constant Task_Id := Current_Task;
      Ref_Msg     : constant String  := "Some log message";
   begin
      R_List.Clear;

      Ref_Request := Log_Request.Create (ID      => Ref_ID,
                                         Level   => Notice,
                                         Message => Ref_Msg);
      R_List.Put (Element => Ref_Request);
      Assert (Condition => R_List.Length = 1,
              Message   => "Unable to insert request");

      R_List.Clear;
      Assert (Condition => R_List.Length = 0,
              Message   => "Unable to clear list");
   end Log_Request_List_Clear;

   -------------------------------------------------------------------------

   procedure Log_Request_List_Done is
      use Ada.Task_Identification;

      R_List      : Protected_Containers.Log_Request_List;
      Ref_Request : Log_Request.Instance;
      Ref_ID      : constant Task_Id := Current_Task;
      Ref_Msg     : constant String  := "Some log message";
   begin
      Ref_Request := Log_Request.Create (ID      => Ref_ID,
                                         Level   => Notice,
                                         Message => Ref_Msg);

      Assert (Condition => R_List.Pending = 0,
              Message   => "new list has pending requests");

      R_List.Done;
      Assert (Condition => R_List.Pending = 0,
              Message   => "done on new list failed");

      R_List.Put (Element => Ref_Request);
      R_List.Get (Element => Ref_Request);

      Assert (Condition => R_List.Pending = 1,
              Message   => "No request pending");

      R_List.Done;
      Assert (Condition => R_List.Pending = 0,
              Message   => "Still pending requests");
   end Log_Request_List_Done;

   -------------------------------------------------------------------------

   procedure Log_Request_List_Get_Put is
      use Ada.Task_Identification;

      R_List      : Protected_Containers.Log_Request_List;
      Ref_Request : Log_Request.Instance;
      Ref_ID      : constant Task_Id := Current_Task;
      Ref_Msg     : constant String  := "Some log message";
   begin
      Ref_Request := Log_Request.Create (ID      => Ref_ID,
                                         Level   => Notice,
                                         Message => Ref_Msg);

      Assert (Condition => R_List.Length = 0,
              Message   => "List not empty");

      R_List.Put (Element => Ref_Request);
      Assert (Condition => R_List.Length = 1,
              Message   => "Unable to insert request");

      declare
         use type Alog.Log_Request.Instance;

         New_Request : Log_Request.Instance;
      begin
         R_List.Get (Element => New_Request);

         Assert (Condition => New_Request = Ref_Request,
                 Message   => "Request mismatch");
      end;
   end Log_Request_List_Get_Put;

end Protected_Container_Tests;
