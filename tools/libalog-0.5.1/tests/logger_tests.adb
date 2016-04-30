--
--  Copyright (c) 2008-2009,
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

with Ada.Directories;

with Ahven;

with Alog.Helpers;
with Alog.Logger;
with Alog.Facilities.File_Descriptor;
with Alog.Facilities.Syslog;
with Alog.Transforms.Casing;

package body Logger_Tests is

   use Ahven;
   use Alog;

   -------------------------------------------------------------------------

   procedure Attach_Facility is
      Log      : Logger.Instance (Init => False);
      Facility : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;

      procedure Check_Facility (Facility_Handle : Facilities.Handle);
      --  Verify that facility with given name is present in the logger.

      procedure Check_Facility (Facility_Handle : Facilities.Handle) is
         use type Facilities.Handle;
      begin
         Assert (Condition => Facility_Handle = Facility,
                 Message   => "facility mismatch");
      end Check_Facility;

   begin
      Assert (Condition => Log.Facility_Count = 0,
              Message   => "facility count not 0");

      begin
         Log.Update (Name    => Facility.Get_Name,
                     Process => Check_Facility'Access);

      exception
         when Logger.Facility_Not_Found =>
            null;
      end;

      Log.Attach_Facility (Facility => Facility);
      Assert (Condition => Log.Facility_Count = 1,
              Message => "could not attach facility");

      Log.Update (Name    => Facility.Get_Name,
                  Process => Check_Facility'Access);

      begin
         Log.Attach_Facility (Facility => Facility);
         Fail (Message => "attached duplicate facility");

      exception
         when Logger.Facility_Already_Present =>
            null;
      end;
   end Attach_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Transform is
      Log       : Logger.Instance (Init => False);
      Transform : constant Transforms.Handle := new Transforms.Casing.Instance;

      procedure Check_Transform (Transform_Handle : Transforms.Handle);
      --  Verify that transformy with given name is present in the logger.

      procedure Check_Transform (Transform_Handle : Transforms.Handle)
      is
         use type Transforms.Handle;
      begin
         Assert (Condition => Transform_Handle = Transform,
                 Message   => "transform mismatch");
      end Check_Transform;

   begin
      Assert (Condition => Log.Transform_Count = 0,
              Message   => "transform count not 0");

      begin
         Log.Update (Name    => Transform.Get_Name,
                     Process => Check_Transform'Access);

      exception
         when Logger.Transform_Not_Found =>
            null;
      end;

      Log.Attach_Transform (Transform => Transform);
      Assert (Condition => Log.Transform_Count = 1,
              Message => "could not attach transform");

      Log.Update (Name    => Transform.Get_Name,
                  Process => Check_Transform'Access);

      begin
         Log.Attach_Transform (Transform => Transform);

         Fail (Message => "attached duplicate transform");

      exception
         when Logger.Transform_Already_Present =>
            null;
      end;
   end Attach_Transform;

   -------------------------------------------------------------------------

   procedure Clear_A_Logger is
      Log       : Logger.Instance (Init => False);
      Facility  : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Transform : constant Transforms.Handle :=
        new Transforms.Casing.Instance;
   begin
      Log.Attach_Facility (Facility => Facility);
      Assert (Condition => Log.Facility_Count = 1,
              Message   => "could not attach facility");

      Log.Attach_Transform (Transform => Transform);
      Assert (Condition => Log.Transform_Count = 1,
              Message   => "could not attach transform");

      Log.Clear;
      Assert (Condition => Log.Facility_Count = 0,
              Message   => "facility count is not 0");
      Assert (Condition => Log.Transform_Count = 0,
              Message   => "transform count is not 0");
   end Clear_A_Logger;

   -------------------------------------------------------------------------

   procedure Default_Facility_Handling is
      Logger1 : Logger.Instance (Init => False);
      Logger2 : Logger.Instance (Init => True);
   begin
      Logger1.Attach_Default_Facility;
      Assert (Condition => Logger1.Facility_Count = 1,
              Message   => "Unable to attach facility");
      Logger1.Attach_Default_Facility;
      Assert (Condition => Logger1.Facility_Count = 1,
              Message   => "Attached facility twice");

      Logger1.Log_Message (Level => Debug,
                           Msg => "Testing default logger");

      Logger1.Detach_Default_Facility;
      Assert (Condition => Logger1.Facility_Count = 0,
              Message   => "Unable to detach facility");

      Logger2.Attach_Default_Facility;
      Assert (Condition => Logger2.Facility_Count = 1,
              Message   => "Attached facility to initialized logger");
      Logger2.Detach_Default_Facility;
      Assert (Condition => Logger2.Facility_Count = 0,
              Message   => "Unable to detach facility from logger");
   end Default_Facility_Handling;

   -------------------------------------------------------------------------

   procedure Detach_Facility_Instance is
      Log      : Logger.Instance (Init => False);
      Facility : constant Facilities.Handle :=
        new Facilities.Syslog.Instance;
   begin
      Facility.Set_Name ("Syslog_Facility");
      Log.Attach_Facility (Facility => Facility);
      Assert (Condition => Log.Facility_Count = 1,
              Message   => "could not attach");
      Log.Detach_Facility (Name => Facility.Get_Name);
      Assert (Condition => Log.Facility_Count = 0,
              Message   => "could not detach");
   end Detach_Facility_Instance;

   -------------------------------------------------------------------------

   procedure Detach_Facility_Unattached is
      Log      : Logger.Instance (Init => False);
      Facility : Facilities.Handle :=
        new Facilities.Syslog.Instance;
   begin
      Facility.Set_Name ("Syslog_Facility");
      Log.Detach_Facility (Name => Facility.Get_Name);
      Fail (Message => "could detach unattached facility");

   exception
      when Logger.Facility_Not_Found =>
         --  Free not attached facility, this is not done by the logger (since
         --  it was never attached).
         Alog.Logger.Free (Facility);
         --  Test passed.
   end Detach_Facility_Unattached;

   -------------------------------------------------------------------------

   procedure Detach_Transform_Instance is
      Log       : Logger.Instance (Init => False);
      Transform : constant Transforms.Handle := new Transforms.Casing.Instance;
   begin
      Transform.Set_Name ("Casing_Transform");
      Log.Attach_Transform (Transform => Transform);
      Assert (Condition => Log.Transform_Count = 1,
              Message   => "could not attach");
      Log.Detach_Transform (Name => Transform.Get_Name);
      Assert (Condition => Log.Transform_Count = 0,
              Message   => "could not detach");
   end Detach_Transform_Instance;

   -------------------------------------------------------------------------

   procedure Detach_Transform_Unattached is
      Log      : Logger.Instance (Init => False);
      Transform : Transforms.Handle :=
        new Transforms.Casing.Instance;
   begin
      Transform.Set_Name ("Casing_Transform");
      Log.Detach_Transform (Name => Transform.Get_Name);
      Fail (Message => "could detach unattached transform");

   exception
      when Logger.Transform_Not_Found =>
         --  Free not attached Transform, this is not done by the logger (since
         --  it was never attached).
         Alog.Logger.Free (Transform);
         --  Test passed.
   end Detach_Transform_Unattached;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for Logger");
      T.Add_Test_Routine
        (Routine => Attach_Facility'Access,
         Name    => "attach a facility");
      T.Add_Test_Routine
        (Routine => Update_Facility'Access,
         Name    => "update a facility");
      T.Add_Test_Routine
        (Routine => Detach_Facility_Instance'Access,
         Name    => "detach facility");
      T.Add_Test_Routine
        (Routine => Detach_Facility_Unattached'Access,
         Name    => "detach not attached facility");
      T.Add_Test_Routine
        (Routine => Attach_Transform'Access,
         Name    => "attach a transform");
      T.Add_Test_Routine
        (Routine => Update_Transform'Access,
         Name    => "update a transform");
      T.Add_Test_Routine
        (Routine => Detach_Transform_Instance'Access,
         Name    => "detach transform");
      T.Add_Test_Routine
        (Routine => Detach_Transform_Unattached'Access,
         Name    => "detach not attached transform");
      T.Add_Test_Routine
        (Routine => Clear_A_Logger'Access,
         Name    => "clear logger");
      T.Add_Test_Routine
        (Routine => Log_One_FD_Facility'Access,
         Name    => "log to one fd facility");
      T.Add_Test_Routine
        (Routine => Log_Multiple_FD_Facilities'Access,
         Name    => "log to multiple fd facilities");
      T.Add_Test_Routine
        (Routine => Log_FD_Facility_with_Transform'Access,
         Name    => "log to fd facility with lowercase transform");
      T.Add_Test_Routine
        (Routine => Verify_Logger_Initialization'Access,
         Name    => "logger initialization behavior");
      T.Add_Test_Routine
        (Routine => Default_Facility_Handling'Access,
         Name    => "default facility handling");
      T.Add_Test_Routine
        (Routine => Source_Based_Logging'Access,
         Name    => "source based logging");
      T.Add_Test_Routine
        (Routine => Loglevel_Handling'Access,
         Name    => "loglevel handling");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Log_FD_Facility_with_Transform is
      Log       : Logger.Instance (Init => False);
      Facility  : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Transform : constant Transforms.Handle :=
        new Transforms.Casing.Instance;
      Testfile  : constant String := "./data/Log_FD_Facility_Lowercase";
      Reffile   : constant String := "./data/Log_FD_Facility_Lowercase.ref";
   begin
      Facility.Toggle_Write_Timestamp (State => False);

      --  Call facility fd specific procedures.
      Facilities.File_Descriptor.Handle
        (Facility).Set_Logfile (Testfile);

      --  Call casing transform specific procedures.
      Transforms.Casing.Handle
        (Transform).Set_Name ("lowercase");

      Log.Attach_Facility (Facility => Facility);
      Log.Attach_Transform (Transform => Transform);

      Log.Log_Message (Level => Debug,
                       Msg   => "Logger Test Message, " &
                       "FD Facility With Lowercase Transform");

      Log.Clear;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files not equal");

      Ada.Directories.Delete_File (Name => Testfile);
   end Log_FD_Facility_with_Transform;

   -------------------------------------------------------------------------

   procedure Log_Multiple_FD_Facilities is
      Log       : Logger.Instance (Init => False);

      Facility1 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Testfile1 : constant String := "./data/Log_Multiple_FD_Facilities1";
      Reffile   : constant String := "./data/Log_Multiple_FD_Facilities.ref";

      Facility2 : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Testfile2 : constant String := "./data/Log_Multiple_FD_Facilities2";
   begin
      --  Set facility parameters.
      Facility1.Set_Name (Name => "Facility1");
      Facility1.Toggle_Write_Timestamp (State => False);
      Facility1.Toggle_Write_Loglevel (State => True);

      Facility2.Set_Name (Name => "Facility2");
      Facility2.Toggle_Write_Timestamp (State => False);
      Facility2.Toggle_Write_Loglevel (State => True);

      --  Call facility fd specific procedures.
      Facilities.File_Descriptor.Handle
        (Facility1).Set_Logfile (Testfile1);
      Facilities.File_Descriptor.Handle
        (Facility2).Set_Logfile (Testfile2);

      --  Attach both facilities to logger instance.
      Log.Attach_Facility (Facility => Facility1);
      Log.Attach_Facility (Facility => Facility2);

      --  Log two messages.
      Log.Log_Message (Level => Debug,
                       Msg   => "Logger testmessage, multiple facilities");
      Log.Log_Message (Level => Info,
                       Msg   => "Logger testmessage, multiple facilities");

      Log.Clear;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile1),
              Message   => "file1 not equal");

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile2),
              Message   => "file2 not equal");

      Ada.Directories.Delete_File (Name => Testfile1);
      Ada.Directories.Delete_File (Name => Testfile2);
   end Log_Multiple_FD_Facilities;

   -------------------------------------------------------------------------

   procedure Log_One_FD_Facility is
      Log      : Logger.Instance (Init => False);
      Facility : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Testfile : constant String := "./data/Log_One_FD_Facility";
      Reffile  : constant String := "./data/Log_One_FD_Facility.ref";
   begin
      Facility.Toggle_Write_Timestamp (State => False);

      --  Call facility fd specific procedures.
      Facilities.File_Descriptor.Handle
        (Facility).Set_Logfile (Testfile);

      Log.Attach_Facility (Facility => Facility);
      Log.Log_Message (Level => Debug,
                       Msg   => "Logger testmessage, one fd facility");

      Log.Clear;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files not equal");

      Ada.Directories.Delete_File (Name => Testfile);
   end Log_One_FD_Facility;

   -------------------------------------------------------------------------

   procedure Loglevel_Handling is
      Log      : Logger.Instance (Init => False);
      Facility : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Testfile : constant String := "./data/Logger_Loglevel";
      Reffile  : constant String := "./data/Logger_Loglevel.ref";
   begin
      Facility.Toggle_Write_Timestamp (State => False);
      Facility.Toggle_Write_Loglevel (State => True);

      --  Call facility fd specific procedures.
      Facilities.File_Descriptor.Handle
        (Facility).Set_Logfile (Testfile);
      Log.Attach_Facility (Facility => Facility);

      Log.Log_Message (Level  => Debug,
                       Msg    => "Debug message (logged)");
      Log.Log_Message (Level  => Notice,
                       Msg    => "Notice message (logged)");

      Log.Clear;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "Files not equal");

      Ada.Directories.Delete_File (Name => Testfile);
   end Loglevel_Handling;

   -------------------------------------------------------------------------

   procedure Source_Based_Logging is
      Log      : Logger.Instance (Init => False);
      Facility : constant Facilities.Handle :=
        new Facilities.File_Descriptor.Instance;
      Testfile : constant String := "./data/Log_Source_Loglevel";
      Reffile  : constant String := "./data/Log_Source_Loglevel.ref";
   begin
      Facility.Toggle_Write_Timestamp (State => False);
      Facility.Toggle_Write_Loglevel (State => True);
      Facilities.File_Descriptor.Handle
        (Facility).Set_Logfile (Testfile);
      Log.Attach_Facility (Facility => Facility);

      Log.Log_Message (Source => "Test",
                       Level  => Info,
                       Msg    => "Source test (logged)");
      Log.Log_Message (Level => Debug,
                       Msg   => "Default source (logged)");
      Log.Log_Message (Source => "Unknown",
                       Level  => Notice,
                       Msg    => "Unknown source (logged)");

      Log.Clear;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "Files not equal");

      Ada.Directories.Delete_File (Name => Testfile);
   end Source_Based_Logging;

   -------------------------------------------------------------------------

   procedure Update_Facility is

      procedure Do_Nothing (Facility_Handle : Facilities.Handle) is null;
      --  Just do nothing.

      Log : Logger.Instance (Init => False);
   begin
      begin
         Log.Update (Name    => "Nonexistent",
                     Process => Do_Nothing'Access);
         Fail (Message => "Expected Facility_Not_Found");

      exception
         when Logger.Facility_Not_Found =>
            null;
      end;

      declare
         Facility      : constant Facilities.Handle :=
           new Facilities.File_Descriptor.Instance;
         Facility_Name : constant String            :=
           "Test_Facility";

         procedure Update_Facility (Facility_Handle : Facilities.Handle);
         --  Set the facility's write timestamp flag to True.

         procedure Update_Facility (Facility_Handle : Facilities.Handle)
         is
         begin
            Facility_Handle.Toggle_Write_Timestamp (State => True);
         end Update_Facility;

      begin
         Facility.Set_Name (Name => Facility_Name);
         Facility.Toggle_Write_Timestamp (State => False);
         Assert (Condition => not Facility.Is_Write_Timestamp,
                 Message   => "Could not disable Timestamp");

         Log.Attach_Facility (Facility => Facility);
         Log.Update (Name    => Facility_Name,
                     Process => Update_Facility'Access);
         Assert (Condition => Facility.Is_Write_Timestamp,
                 Message   => "Update failed");
      end;

   end Update_Facility;

   -------------------------------------------------------------------------

   procedure Update_Transform is

      procedure Do_Nothing (Transform_Handle : Transforms.Handle) is null;
      --  Just do nothing.

      Log : Logger.Instance (Init => False);
   begin
      begin
         Log.Update (Name    => "Nonexistent",
                     Process => Do_Nothing'Access);
         Fail (Message => "Expected Transform_Not_Found");

      exception
         when Logger.Transform_Not_Found =>
            null;
      end;

      declare
         Transform      : constant Transforms.Handle :=
           new Transforms.Casing.Instance;
         Transform_Name : constant String            :=
           "Test_Transform";
         Suffix         : constant String            :=
           "_Updated";

         procedure Update_Transform (Transform_Handle : Transforms.Handle);
         --  Set the transform's name to a test string.

         procedure Update_Transform (Transform_Handle : Transforms.Handle)
         is
         begin
            Transform_Handle.Set_Name (Name => Transform_Name & Suffix);
         end Update_Transform;

      begin
         Transform.Set_Name (Name => Transform_Name);
         Assert (Condition => Transform.Get_Name = Transform_Name,
                 Message   => "Transform name mismatch");

         Log.Attach_Transform (Transform => Transform);
         Log.Update (Name    => Transform_Name,
                     Process => Update_Transform'Access);

         Assert (Condition => Transform.Get_Name = Transform_Name & Suffix,
                 Message   => "Transform update failed");
      end;

   end Update_Transform;

   -------------------------------------------------------------------------

   procedure Verify_Logger_Initialization is
      Logger1 : Logger.Instance (Init => False);
      Logger2 : Logger.Instance (Init => True);
   begin
      Assert (Condition => Logger1.Facility_Count = 0,
              Message   => "logger1 not empty");
      Assert (Condition => Logger2.Facility_Count = 1,
              Message   => "logger2 empty");
   end Verify_Logger_Initialization;

end Logger_Tests;
