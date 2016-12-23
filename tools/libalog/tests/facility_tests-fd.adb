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

with Ada.Text_IO;
with Ada.Directories;

with Ahven; use Ahven;

with Alog.Helpers;
with Alog.Log_Request;
with Alog.Policy_DB;
with Alog.Facilities.File_Descriptor;

package body Facility_Tests.FD is

   use Alog;
   use Alog.Log_Request;
   use Alog.Facilities;

   -------------------------------------------------------------------------

   procedure Dst_Loglevel_Handling is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Dst_Loglevel_Fd";
      Reffile  : constant String := "./data/Dst_Loglevel_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (State => False);
      F.Toggle_Write_Loglevel (State => True);
      F.Set_Logfile (Path => Testfile);

      F.Set_Name (Name => "Dst_Facility");

      Policy_DB.Set_Loglevel (Identifier => "Dst_Facility",
                              Level      => Warning);

      F.Process
        (Request => Create
           (Level   => Warning,
            Message => "Testmessage"));
      F.Process
        (Request => Create
           (Level   => Info,
            Source  => "Test",
            Message => "Testmessage"));

      F.Close_Logfile;
      Policy_DB.Reset;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files not equal");

      Ada.Directories.Delete_File (Name => Testfile);
   end Dst_Loglevel_Handling;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for FD Facility");
      T.Add_Test_Routine
        (Routine => Set_Valid_Logfile_Fd'Access,
         Name    => "set valid logfile");
      T.Add_Test_Routine
        (Routine => Set_Invalid_Logfile_Fd'Access,
         Name    => "set invalid logfile");
      T.Add_Test_Routine
        (Routine => Write_Message_Fd'Access,
         Name    => "log a fd message");
      T.Add_Test_Routine
        (Routine => Teardown_Fd'Access,
         Name    => "teardown fd facility");
      T.Add_Test_Routine
        (Routine => Toggle_Write_Timestamp_Fd'Access,
         Name    => "toggle fd timestamp writing");
      T.Add_Test_Routine
        (Routine => Toggle_Write_Loglevel_Fd'Access,
         Name    => "toggle fd loglevel writing");
      T.Add_Test_Routine
        (Routine => Toggle_Write_Source_Fd'Access,
         Name    => "toggle fd source writing");
      T.Add_Test_Routine
        (Routine => Trim_Loglevels_Fd'Access,
         Name    => "fd loglevel align");
      T.Add_Test_Routine
        (Routine => Verify_Append'Access,
         Name    => "append logic");
      T.Add_Test_Routine
        (Routine => Dst_Loglevel_Handling'Access,
         Name    => "destination loglevels");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Invalid_Logfile_Fd is
      F : File_Descriptor.Instance;
   begin
      F.Set_Logfile (Path => "./");
      Fail (Message => "expected Name_Error");

   exception
      when File_Descriptor.Open_File_Error =>
         null;
   end Set_Invalid_Logfile_Fd;

   -------------------------------------------------------------------------

   procedure Set_Valid_Logfile_Fd is
      use Ada.Text_IO;
      F : File_Descriptor.Instance;
   begin
      F.Set_Logfile (Path => "./data/Set_Valid_Logfile");
      Assert (Condition => Is_Open (F.Get_Logfile.all),
              Message   => "could not set logfile!");
      F.Close_Logfile (Remove => True);
   end Set_Valid_Logfile_Fd;

   -------------------------------------------------------------------------

   procedure Teardown_Fd is
      use Ada.Text_IO;
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Teardown_Fd";
   begin
      F.Set_Logfile (Path => Testfile);
      Assert (Condition => Is_Open (File => F.Get_Logfile.all),
              Message   => "could not set logfile!");
      F.Teardown;
      Assert (Condition => not Is_Open (File => F.Get_Logfile.all),
              Message   => "logfile still open!");

      Ada.Directories.Delete_File (Name => Testfile);
   end Teardown_Fd;

   -------------------------------------------------------------------------

   procedure Toggle_Write_Loglevel_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Toggle_Write_Loglevel_Fd";
      Reffile  : constant String := "./data/Toggle_Write_Loglevel_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (State => False);
      F.Toggle_Write_Loglevel (State => False);
      F.Set_Logfile (Path => Testfile);
      F.Process
        (Request => Create
           (Message => "This is a message without loglevel"));

      F.Close_Logfile;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "unable to disable");

      Ada.Directories.Delete_File (Name => Testfile);
   end Toggle_Write_Loglevel_Fd;

   -------------------------------------------------------------------------

   procedure Toggle_Write_Source_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Toggle_Write_Source_Fd";
      Reffile  : constant String := "./data/Toggle_Write_Source_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (State => False);
      F.Toggle_Write_Loglevel (State => True);
      F.Set_Logfile (Path => Testfile);

      F.Process
        (Request => Create
           (Level   => Warning,
            Message => "No source given"));
      F.Process
        (Request => Create
           (Level   => Info,
            Source  => "Test",
            Message => "Source 'Test'"));

      F.Toggle_Write_Source (State => False);

      F.Process
        (Request => Create
           (Level   => Info,
            Source  => "Test",
            Message => "Source 'Test', source writing disabled"));

      F.Close_Logfile;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "file mismatch");

      Ada.Directories.Delete_File (Name => Testfile);
   end Toggle_Write_Source_Fd;

   -------------------------------------------------------------------------

   procedure Toggle_Write_Timestamp_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Toggle_Write_Timestamp_Fd";
      Reffile  : constant String := "./data/Toggle_Write_Timestamp_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (State => False);
      F.Set_Logfile (Path => Testfile);
      F.Process
        (Request => Create
           (Message => "This is a message without timestamp"));

      F.Close_Logfile;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "unable to disable");

      Ada.Directories.Delete_File (Name => Testfile);
   end Toggle_Write_Timestamp_Fd;

   -------------------------------------------------------------------------

   procedure Trim_Loglevels_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Trim_Loglevels_Fd";
      Reffile  : constant String := "./data/Trim_Loglevels_Fd.ref";
   begin
      F.Toggle_Write_Timestamp (State => False);
      F.Toggle_Write_Loglevel (State => True);
      F.Set_Logfile (Path => Testfile);
      for Lvl in Alog.Log_Level loop
         F.Process
           (Request => Create
              (Level   => Lvl,
               Message => "Testmessage"));
      end loop;

      F.Close_Logfile;
      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "alignment incorrect");

      Ada.Directories.Delete_File (Name => Testfile);
   end Trim_Loglevels_Fd;

   -------------------------------------------------------------------------

   procedure Verify_Append is
   begin
      Append :
      declare
         F1       : File_Descriptor.Instance;
         F2       : File_Descriptor.Instance;
         Testfile : constant String := "./data/Log_Append_Fd";
         Reffile  : constant String := "./data/Log_Append_Fd.ref";
      begin
         F1.Toggle_Write_Timestamp (State => False);
         F1.Set_Logfile (Path => Testfile);
         F1.Process
           (Request => Create
              (Message => "Facility1"));
         F1.Close_Logfile;

         F2.Toggle_Write_Timestamp (State => False);
         F2.Set_Logfile (Path => Testfile);
         F2.Process
           (Request => Create
              (Message => "Facility2"));
         F2.Close_Logfile;

         Assert (Condition => Helpers.Assert_Files_Equal
                 (Filename1 => Testfile,
                  Filename2 => Reffile),
                 Message   => "append does not work");

         Ada.Directories.Delete_File (Name => Testfile);
      end Append;

      Overwrite :
      declare
         F1       : File_Descriptor.Instance;
         F2       : File_Descriptor.Instance;
         Testfile : constant String := "./data/Log_Overwrite_Fd";
         Reffile  : constant String := "./data/Log_Overwrite_Fd.ref";
      begin
         F1.Toggle_Write_Timestamp (State => False);
         F1.Set_Logfile (Path => Testfile);
         F1.Process
           (Request => Create
              (Message => "Facility1"));
         F1.Close_Logfile;

         F2.Toggle_Write_Timestamp (State => False);
         F2.Set_Logfile (Path   => Testfile,
                         Append => False);
         F2.Process
           (Request => Create
              (Message => "Facility2"));
         F2.Close_Logfile;

         Assert (Condition => Helpers.Assert_Files_Equal
                 (Filename1 => Testfile,
                  Filename2 => Reffile),
                 Message   => "overwrite does not work");

         Ada.Directories.Delete_File (Name => Testfile);
      end Overwrite;
   end Verify_Append;

   -------------------------------------------------------------------------

   procedure Write_Message_Fd is
      F        : File_Descriptor.Instance;
      Testfile : constant String := "./data/Write_Message_Fd";
      Reffile  : constant String := "./data/Write_Message_Fd.ref";
   begin
      --  We have to disable timestamps, since its changing all
      --  the time :)
      F.Toggle_Write_Timestamp (State => False);

      --  Open logfile, write test message.
      F.Set_Logfile (Path => Testfile);
      F.Process
        (Request => Create
           (Message => "This is a test log-message"));

      F.Close_Logfile;

      Assert (Condition => Helpers.Assert_Files_Equal
              (Filename1 => Reffile,
               Filename2 => Testfile),
              Message   => "files not equal");

      Ada.Directories.Delete_File (Name => Testfile);
   end Write_Message_Fd;

end Facility_Tests.FD;
