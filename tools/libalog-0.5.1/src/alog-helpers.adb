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
with Ada.Strings.Fixed;

package body Alog.Helpers is

   -------------------------------------------------------------------------

   function Assert_Files_Equal
     (Filename1 : String;
      Filename2 : String)
      return Boolean
   is
      File1        : D_IO.File_Type;
      File2        : D_IO.File_Type;
      Char1, Char2 : My_Rec;
      Result       : Boolean := True;
   begin
      --  Open both files.
      Open (File => File1,
            Mode => In_File,
            Name => Filename1);

      Open (File => File2,
            Mode => In_File,
            Name => Filename2);

      --  Check length of files first.
      if Size (File1) /= Size (File2) then
         Close (File => File1);
         Close (File => File2);
         return False;
      end if;

      while not End_Of_File (File1) loop
         --  Read one byte from both files.
         Read (File => File1, Item => Char1);
         Read (File => File2, Item => Char2);
         --  Compare it.
         if Char1 /= Char2 then
            Result := False;
         end if;
      end loop;

      --  Close them files again.
      Close (File => File1);
      Close (File => File2);

      return Result;

   end Assert_Files_Equal;

   -------------------------------------------------------------------------

   procedure Read_Loglevels
     (Filename      :        String;
      Default_Level : in out Log_Level;
      Identifiers   :    out Maps.Wildcard_Level_Map)
   is
      Conf_File  : Ada.Text_IO.File_Type;
      Line_Count : Natural := 0;
      Line       : String (1 .. 1_024);
      Last       : Integer;
   begin
      Ada.Text_IO.Open (File => Conf_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Filename);

      while not Ada.Text_IO.End_Of_File (File => Conf_File) loop
         Ada.Text_IO.Get_Line
           (File => Conf_File,
            Item => Line,
            Last => Last);
         Line_Count := Line_Count + 1;

         if Last - Line'First >= 0 then
            declare
               Trimmed : constant String := Ada.Strings.Fixed.Trim
                 (Source => Line (Line'First .. Last),
                  Side   => Ada.Strings.Both);
               Eq      : Natural;
            begin
               if Trimmed'Length /= 0
                 and then Trimmed (Trimmed'First) /= '#'
               then
                  Eq := Ada.Strings.Fixed.Index
                    (Source  => Trimmed,
                     Pattern => "=");

                  if Eq not in Trimmed'First + 1 .. Trimmed'Last then
                     Ada.Text_IO.Close (File => Conf_File);
                     raise Invalid_Config with "Syntax error in file "
                       & Filename & " on line"
                       & Line_Count'Img & ": no assignment operator";
                  end if;

                  --  Line seems valid

                  declare
                     Key   : constant String := Ada.Strings.Fixed.Trim
                       (Source => Trimmed (Trimmed'First .. Eq - 1),
                        Side   => Ada.Strings.Both);
                     Value : constant String := Ada.Strings.Fixed.Trim
                       (Source => Trimmed (Eq + 1 .. Trimmed'Last),
                        Side   => Ada.Strings.Both);

                     Loglevel : Log_Level;
                  begin
                     begin
                        Loglevel := Log_Level'Value (Value);

                     exception
                        when others =>
                           Ada.Text_IO.Close (File => Conf_File);
                           raise Invalid_Config with "Syntax error in file "
                             & Filename & " on line"
                             & Line_Count'Img & ": invalid loglevel '"
                             & Value & "'";
                     end;

                     if Key = "Default" or Key = "default" then
                        Default_Level := Loglevel;
                     else
                        Identifiers.Insert (Key  => Key,
                                            Item => Loglevel);
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;

      Ada.Text_IO.Close (File => Conf_File);
   end Read_Loglevels;

end Alog.Helpers;
