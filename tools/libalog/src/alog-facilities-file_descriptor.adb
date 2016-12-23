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
with Ada.Exceptions;

package body Alog.Facilities.File_Descriptor is

   -------------------------------------------------------------------------

   procedure Close_Logfile
     (Facility : in out Instance;
      Remove   :        Boolean := False)
   is
      use Ada.Text_IO;
   begin
      if Facility.Log_File_Ptr /= Standard_Output
        and Is_Open (File => Facility.Log_File)
      then
         if Remove then
            --  Close and delete.
            Delete (File => Facility.Log_File);
         else
            --  Close only.
            Close (File => Facility.Log_File);
         end if;
      end if;
   end Close_Logfile;

   -------------------------------------------------------------------------

   function Get_Logfile (Facility : Instance) return Ada.Text_IO.File_Access
   is
   begin
      return Facility.Log_File_Ptr;
   end Get_Logfile;

   -------------------------------------------------------------------------

   procedure Set_Logfile
     (Facility : in out Instance;
      Path     :        String;
      Append   :        Boolean := True)
   is
   begin
      if not Ada.Directories.Exists (Name => Path) then
         Ada.Text_IO.Create (File => Facility.Log_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => Path);
      else
         declare
            File_Mode : Ada.Text_IO.File_Mode := Ada.Text_IO.Append_File;
         begin
            if not Append then
               File_Mode := Ada.Text_IO.Out_File;
            end if;

            Ada.Text_IO.Open (File => Facility.Log_File,
                              Name => Path,
                              Mode => File_Mode);
         end;
      end if;

      --  Set logfile name and pointer to newly created file.

      Facility.Log_File_Name := To_Bounded_String (Path);

      --  Unchecked_Access is needed here since we use a pointer which is
      --  defined externaly in the Text_IO library.

      Facility.Log_File_Ptr := Facility.Log_File'Unchecked_Access;

   exception
      when E : others =>
         raise Open_File_Error with "Unable to open logfile '" & Path
           & "': " & Ada.Exceptions.Exception_Message (X => E);
   end Set_Logfile;

   -------------------------------------------------------------------------

   procedure Teardown (Facility : in out Instance) is
   begin
      Facility.Close_Logfile;
   end Teardown;

   -------------------------------------------------------------------------

   procedure Write
     (Facility : Instance;
      Level    : Log_Level := Info;
      Msg      : String)
   is
      pragma Unreferenced (Level);

      use type Ada.Text_IO.File_Access;
   begin
      if Facility.Log_File_Ptr = Ada.Text_IO.Standard_Output then
         Ada.Text_IO.Put_Line (Item => Msg);
         Ada.Text_IO.Flush;
      else
         Ada.Text_IO.Put_Line (File => Facility.Log_File_Ptr.all,
                               Item => Msg);
         Ada.Text_IO.Flush (File => Facility.Log_File_Ptr.all);
      end if;
   end Write;

end Alog.Facilities.File_Descriptor;
