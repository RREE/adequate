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

package body Alog.Policy_DB.Types is

   Current_Default_Loglevel : Log_Level := Alog_Default_Level;
   --  Current default loglevel.

   -------------------------------------------------------------------------

   protected body Protected_Policy_DB is

      -------------------------------------------------------------------------

      function Accept_Dst
        (Identifier : String;
         Level      : Log_Level)
         return Boolean
      is
         use type Alog.Maps.Cursor;
         Position : Maps.Cursor;
      begin
         Position := Ident_Levels.Lookup (Key => Identifier);

         if Position /= Maps.No_Element then
            return Level >= Maps.Element (Position => Position);
         end if;

         return True;
      end Accept_Dst;

      -------------------------------------------------------------------------

      function Accept_Src
        (Identifier : String := "";
         Level      : Log_Level)
         return Boolean
      is
      begin
         return Level >= Lookup (Identifier => Identifier);
      end Accept_Src;

      -------------------------------------------------------------------------

      function Get_Default_Loglevel return Log_Level is
      begin
         return Current_Default_Loglevel;
      end Get_Default_Loglevel;

      -------------------------------------------------------------------------

      function Get_Loglevel (Identifier : String) return Log_Level is
      begin
         return Ident_Levels.Element (Key => Identifier);

      exception
         when Constraint_Error =>
            raise No_Ident_Loglevel with
              "No loglevel for identifier '" & Identifier & "'";
      end Get_Loglevel;

      -------------------------------------------------------------------------

      function Lookup (Identifier : String) return Log_Level is
         use type Alog.Maps.Cursor;
         Position : Maps.Cursor;
      begin
         Position := Ident_Levels.Lookup (Key => Identifier);

         if Position /= Maps.No_Element then
            return Maps.Element (Position => Position);
         end if;

         return Current_Default_Loglevel;
      end Lookup;

      -------------------------------------------------------------------------

      procedure Reset is
      begin
         Current_Default_Loglevel := Alog_Default_Level;
         Ident_Levels.Clear;
      end Reset;

      -------------------------------------------------------------------------

      procedure Set_Default_Loglevel (Level : Log_Level) is
      begin
         Current_Default_Loglevel := Level;
      end Set_Default_Loglevel;

      -------------------------------------------------------------------------

      procedure Set_Loglevel
        (Identifier : String;
         Level      : Log_Level)
      is
      begin
         Ident_Levels.Insert (Key  => Identifier,
                            Item => Level);
      end Set_Loglevel;

      -------------------------------------------------------------------------

      procedure Set_Loglevel (Identifiers : Maps.Wildcard_Level_Map) is
      begin
         Ident_Levels := Identifiers;
      end Set_Loglevel;

   end Protected_Policy_DB;

end Alog.Policy_DB.Types;
