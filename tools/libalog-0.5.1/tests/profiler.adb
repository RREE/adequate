--
--  Copyright (c) 2009-2014,
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

with Alog.Logger;
with Alog.Policy_DB;

procedure Profiler is
   use Alog;

   Log : Logger.Instance (Init => False);
begin
   Policy_DB.Set_Loglevel (Identifier => "Foo.*",
                           Level      => Debug);

   for I in 1 .. 100000 loop
      Log.Log_Message (Source => "Foo.Foo.Foo.Foo",
                       Level  => Notice,
                       Msg    => "Test message");
   end loop;
end Profiler;
