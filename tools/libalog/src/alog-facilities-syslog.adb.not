--
--  Copyright (c) 2008-2011,
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

with Interfaces.C.Strings;

package body Alog.Facilities.Syslog is

   package C renames Interfaces.C;

   type L_Type is mod 2 ** 3;
   for L_Type'Size use 3;

   type F_Type is mod 2 ** 8;
   for F_Type'Size use 8;

   Level_Map : constant array (Log_Level) of L_Type
     := (Debug     => 7,
         Info      => 6,
         Notice    => 5,
         Warning   => 4,
         Error     => 3,
         Critical  => 2,
         Alert     => 1,
         Emergency => 0);

   Facility_Map : constant array (Syslog_Origin) of F_Type
     := (LOG_KERN     => 0,
         LOG_USER     => 8,
         LOG_MAIL     => 16,
         LOG_DAEMON   => 24,
         LOG_AUTH     => 32,
         LOG_SYSLOG   => 40,
         LOG_LPR      => 48,
         LOG_NEWS     => 56,
         LOG_UUCP     => 64,
         LOG_CRON     => 72,
         LOG_AUTHPRIV => 80,
         LOG_FTP      => 88,
         LOG_LOCAL0   => 128,
         LOG_LOCAL1   => 136,
         LOG_LOCAL2   => 144,
         LOG_LOCAL3   => 152,
         LOG_LOCAL4   => 160,
         LOG_LOCAL5   => 168,
         LOG_LOCAL6   => 176,
         LOG_LOCAL7   => 184);

   -------------------------------------------------------------------------

   function Get_Origin (Facility : Instance) return Syslog_Origin
   is
   begin
      return Facility.Origin;
   end Get_Origin;

   -------------------------------------------------------------------------

   procedure Set_Origin
     (Facility : in out Instance;
      Value    :        Syslog_Origin)
   is
   begin
      Facility.Origin := Value;
   end Set_Origin;

   -------------------------------------------------------------------------

   procedure Write
     (Facility : Instance;
      Level    : Log_Level := Info;
      Msg      : String)
   is
      use type C.int;

      procedure Syslog_Wrapper
        (Prio : C.int;
         Msg  : C.Strings.chars_ptr);
      pragma Import (C, Syslog_Wrapper, "syslog_wrapper");

      C_Msg  : C.Strings.chars_ptr := C.Strings.New_String (Str => Msg);
      C_Prio : constant C.int      := C.int (Level_Map (Level)) +
        C.int (Facility_Map (Facility.Origin));
   begin
      Syslog_Wrapper (Prio => C_Prio,
                      Msg  => C_Msg);

      C.Strings.Free (C_Msg);
   end Write;

end Alog.Facilities.Syslog;
