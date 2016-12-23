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

with Alog.Policy_DB.Types;

package body Alog.Policy_DB is

   Instance : Types.Protected_Policy_DB;

   -------------------------------------------------------------------------

   function Accept_Dst
     (Identifier : String;
      Level      : Log_Level)
      return Boolean is
   begin
      return Instance.Accept_Dst (Identifier => Identifier, Level => Level);
   end Accept_Dst;

   -------------------------------------------------------------------------

   function Accept_Src
     (Identifier : String := "";
      Level      : Log_Level)
      return Boolean is
   begin
      return Instance.Accept_Src (Identifier => Identifier, Level => Level);
   end Accept_Src;

   -------------------------------------------------------------------------

   function Get_Default_Loglevel return Log_Level is
   begin
      return Instance.Get_Default_Loglevel;
   end Get_Default_Loglevel;

   -------------------------------------------------------------------------

   function Get_Loglevel (Identifier : String) return Log_Level is
   begin
      return Instance.Get_Loglevel (Identifier => Identifier);
   end Get_Loglevel;

   -------------------------------------------------------------------------

   function Lookup (Identifier : String) return Log_Level is
   begin
      return Instance.Lookup (Identifier => Identifier);
   end Lookup;

   -------------------------------------------------------------------------

   procedure Reset is
   begin
      Instance.Reset;
   end Reset;

   -------------------------------------------------------------------------

   procedure Set_Default_Loglevel (Level : Log_Level) is
   begin
      Instance.Set_Default_Loglevel (Level => Level);
   end Set_Default_Loglevel;

   -------------------------------------------------------------------------

   procedure Set_Loglevel
     (Identifier : String;
      Level      : Log_Level) is
   begin
      Instance.Set_Loglevel (Identifier => Identifier, Level => Level);
   end Set_Loglevel;

   -------------------------------------------------------------------------

   procedure Set_Loglevel (Identifiers : Maps.Wildcard_Level_Map) is
   begin
      Instance.Set_Loglevel (Identifiers => Identifiers);
   end Set_Loglevel;

end Alog.Policy_DB;
