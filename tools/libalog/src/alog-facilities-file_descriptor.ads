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

--  File_Descriptor facility. Used to log to a console or file. If no file is
--  specified by a Set_Logfile()-call, console logging is used.
package Alog.Facilities.File_Descriptor is

   type Instance is new Alog.Facilities.Instance with private;
   --  File Descriptor based logging facility.

   type Handle is access all Instance;

   overriding
   procedure Teardown (Facility : in out Instance);
   --  Implementation of Teardown-procedure.

   procedure Set_Logfile
     (Facility : in out Instance;
      Path     :        String;
      Append   :        Boolean := True);
   --  Set logfile to use. If not set, standard output is used for logging.
   --  Set Append to False if an existing logfile should be overwritten.

   function Get_Logfile (Facility : Instance) return Ada.Text_IO.File_Access;
   --  Get currently used logfile.

   procedure Close_Logfile
     (Facility : in out Instance;
      Remove   :        Boolean := False);
   --  Close opened logfile.

   Open_File_Error : exception;
   --  This exception is raised if an error occurs while trying to open a
   --  logfile.

private

   overriding
   procedure Write
     (Facility : Instance;
      Level    : Log_Level := Info;
      Msg      : String);
   --  Implementation of the Write procedure for FD.

   type Instance is new Alog.Facilities.Instance with record
      Log_File      : aliased Ada.Text_IO.File_Type;
      --  Logfile used for file based logging.

      Log_File_Ptr  : Ada.Text_IO.File_Access :=
        Ada.Text_IO.Standard_Output;
      --  Reference to actual log file. Default is Standard_Output.

      Log_File_Name : BS_Path.Bounded_String :=
        To_Bounded_String ("none");
      --  File name of log file.
   end record;

end Alog.Facilities.File_Descriptor;
