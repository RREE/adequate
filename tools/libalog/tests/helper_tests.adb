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

with Ahven;

with Alog.Helpers;
with Alog.Maps;

package body Helper_Tests is

   use Ahven;
   use Alog;
   use Alog.Helpers;

   -------------------------------------------------------------------------

   procedure Compare_Files is
      File1 : constant String := "./data/ref_file1";
      File2 : constant String := "./data/ref_file2";
      File3 : constant String := "./data/ref_file3";
      File4 : constant String := "./data/ref_file4";
   begin
      Assert
        (Condition => Assert_Files_Equal
           (Filename1 => File1,
            Filename2 => File2),
         Message   => "files should be equal");

      Assert
        (Condition => not Assert_Files_Equal
           (Filename1 => File1,
            Filename2 => File3),
         Message   => "files should not be equal");

      Assert
        (Condition => not Assert_Files_Equal
           (Filename1 => File1,
            Filename2 => File4),
         Message   => "filesize should not be equal");
   end Compare_Files;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for helpers");
      T.Add_Test_Routine
        (Routine => Compare_Files'Access,
         Name    => "test file equality helper");
      T.Add_Test_Routine
        (Routine => Read_Config'Access,
         Name    => "read loglevel config file");
      T.Add_Test_Routine
        (Routine => Read_Config_Nodefault'Access,
         Name    => "read config without default loglevel");
      T.Add_Test_Routine
        (Routine => Read_Config_Invalid_Loglevel'Access,
         Name    => "read config with invalid loglevel");
      T.Add_Test_Routine
        (Routine => Read_Invalid_Config'Access,
         Name    => "read invalid config");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Read_Config is
      Config_File   : constant String := "./data/Loglevel_Config.ref";
      Default_Level : Log_Level       := Debug;
      Ident_Map     : Maps.Wildcard_Level_Map;
   begin
      Read_Loglevels (Filename      => Config_File,
                      Default_Level => Default_Level,
                      Identifiers   => Ident_Map);

      Assert (Condition => Default_Level = Info,
              Message   => "default loglevel mismatch");

      Assert (Condition => Ident_Map.Element (Key => "Foo.*") = Debug,
              Message   => "Foo.* mismatch");
      Assert (Condition => Ident_Map.Element (Key => "Foo.Bar") = Alert,
              Message   => "Foo.Bar mismatch");
      Assert (Condition => Ident_Map.Element (Key => "Foo.Foo") = Notice,
              Message   => "Foo.Foo mismatch");
   end Read_Config;

   -------------------------------------------------------------------------

   procedure Read_Config_Invalid_Loglevel is
      Config_File   : constant String := "./data/Loglevel_Config_Invalid1.ref";
      Default_Level : Log_Level       := Debug;
      Ident_Map     : Maps.Wildcard_Level_Map;
   begin
      Read_Loglevels (Filename      => Config_File,
                      Default_Level => Default_Level,
                      Identifiers   => Ident_Map);
      Fail (Message => "expected Invalid_Config");

   exception
      when Invalid_Config =>
         null;
   end Read_Config_Invalid_Loglevel;

   -------------------------------------------------------------------------

   procedure Read_Config_Nodefault is
      Config_File   : constant String := "./data/Loglevel_Config_Nodef.ref";
      Default_Level : Log_Level       := Debug;
      Ident_Map     : Maps.Wildcard_Level_Map;
   begin
      Read_Loglevels (Filename      => Config_File,
                      Default_Level => Default_Level,
                      Identifiers   => Ident_Map);
      Assert (Condition => Default_Level = Debug,
              Message   => "default loglevel changed");

      Assert (Condition => Ident_Map.Element (Key => "Foo.*") = Debug,
              Message   => "Foo.* mismatch");
   end Read_Config_Nodefault;

   -------------------------------------------------------------------------

   procedure Read_Invalid_Config is
      Config_File   : constant String := "./data/Loglevel_Config_Invalid2.ref";
      Default_Level : Log_Level       := Debug;
      Ident_Map     : Maps.Wildcard_Level_Map;
   begin
      Read_Loglevels (Filename      => Config_File,
                      Default_Level => Default_Level,
                      Identifiers   => Ident_Map);
      Fail (Message => "expected Invalid_Config");

   exception
      when Invalid_Config =>
         null;
   end Read_Invalid_Config;

end Helper_Tests;
