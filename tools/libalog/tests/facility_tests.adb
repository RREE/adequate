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

with Ada.Calendar.Time_Zones;

with Ahven; use Ahven;

with Alog.Facilities.File_Descriptor;

package body Facility_Tests is

   use Alog;
   use Alog.Facilities;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for Facilites");
      T.Add_Test_Routine
        (Routine => Set_Name'Access,
         Name    => "set facility name");
      T.Add_Test_Routine
        (Routine => Toggle_Loglevel'Access,
         Name    => "toggle loglevel");
      T.Add_Test_Routine
        (Routine => Toggle_Timestamp'Access,
         Name    => "toggle timestamp");
      T.Add_Test_Routine
        (Routine => Toggle_UTC_Timestamp'Access,
         Name    => "toggle UTC timestamp");
      T.Add_Test_Routine
        (Routine => Toggle_Source'Access,
         Name    => "toggle source writing");
      T.Add_Test_Routine
        (Routine => Timestamp_Creation'Access,
         Name    => "timestamp creation");
      T.Add_Test_Routine
        (Routine => Timestamp_Format_Setter'Access,
         Name    => "set timestamp format");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Name is
      F        : File_Descriptor.Instance;
      Expected : constant String := "TEST";
   begin
      F.Set_Name (Name => Expected);
      Assert (Condition => F.Get_Name = Expected,
              Message => "name not equal");
   end Set_Name;

   -------------------------------------------------------------------------

   procedure Timestamp_Creation is
      use Ada.Calendar;
      use Ada.Calendar.Time_Zones;

      F : File_Descriptor.Instance;

      Ref_Time  : constant Time   := Time_Of
        (Year    => 2009,
         Month   => 10,
         Day     => 10,
         Seconds => 7255.0);
      Ref_Stamp : constant String := "Oct 10 2009 02:00:55";

      --  Adding the UTC time offset to the reference time should lead to the
      --  same timestamp string when UTC timestamps are enabled since UTC time
      --  is timezone-dependent time minus the UTC offset at that given time.

      Ref_UTC_Time : constant Time := Ref_Time + Duration
        (UTC_Time_Offset (Ref_Time)) * 60;
   begin
      Assert (Condition => Ref_Stamp = F.Get_Timestamp (Time => Ref_Time),
              Message   => "Timestamp mismatch");

      F.Toggle_UTC_Timestamp (State => True);
      Assert (Condition => F.Get_Timestamp (Time => Ref_UTC_Time) = Ref_Stamp,
              Message   => "UTC timestamp mismatch!");
   end Timestamp_Creation;

   -------------------------------------------------------------------------

   procedure Timestamp_Format_Setter
   is
      use Ada.Calendar;
      use Ada.Calendar.Time_Zones;

      F : File_Descriptor.Instance;

      Ref_Time : constant Time := Time_Of
        (Year    => 2009,
         Month   => 10,
         Day     => 10,
         Seconds => 7255.0);
   begin
      F.Set_Timestamp_Format (Format => "%b %Y");
      Assert (Condition => F.Get_Timestamp (Time => Ref_Time) = "Oct 2009",
              Message   => "Timestamp mismatch");

      begin
         F.Set_Timestamp_Format (Format => "a b c %");
         Fail (Message => "Exception expected");

      exception
         when Invalid_Timestamp_Format => null;
      end;
   end Timestamp_Format_Setter;

   -------------------------------------------------------------------------

   procedure Toggle_Loglevel is
      F : File_Descriptor.Instance;
   begin
      Assert (Condition => not F.Is_Write_Loglevel,
              Message   => "Loglevel writing is 'True' by default");

      F.Toggle_Write_Loglevel (State => True);
      Assert (Condition => F.Is_Write_Loglevel,
              Message   => "Loglevel writing not 'True'");
   end Toggle_Loglevel;

   -------------------------------------------------------------------------

   procedure Toggle_Source is
      F : File_Descriptor.Instance;
   begin
      Assert (Condition => F.Is_Write_Source,
              Message   => "Source writing is 'False' by default");

      F.Toggle_Write_Source (State => False);
      Assert (Condition => not F.Is_Write_Source,
              Message   => "Source writing is still 'True'");
   end Toggle_Source;

   -------------------------------------------------------------------------

   procedure Toggle_Timestamp is
      F : File_Descriptor.Instance;
   begin
      Assert (Condition => F.Is_Write_Timestamp,
              Message   => "Timestamp writing is 'False' by default");
      F.Toggle_Write_Timestamp (State => False);
      Assert (Condition => not F.Is_Write_Timestamp,
              Message   => "Timestamp writing not 'False'");
   end Toggle_Timestamp;

   -------------------------------------------------------------------------

   procedure Toggle_UTC_Timestamp is
      F : File_Descriptor.Instance;
   begin
      Assert (Condition => not F.Is_UTC_Timestamp,
              Message   => "Default should be 'False'");

      F.Toggle_UTC_Timestamp (State => True);
      Assert (Condition => F.Is_UTC_Timestamp,
              Message   => "Expected 'True'");
   end Toggle_UTC_Timestamp;

end Facility_Tests;
