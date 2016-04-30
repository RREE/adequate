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

with Ada.Strings.Fixed;

package body Alog.Maps is

   -------------------------------------------------------------------------

   procedure Clear (Map : in out Wildcard_Level_Map) is
   begin
      Map.Data.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Element
     (Map : Wildcard_Level_Map;
      Key : String)
      return Log_Level
   is
   begin
      return Map.Data.Element (Key => To_Unbounded_String (Key));
   end Element;

   -------------------------------------------------------------------------

   function Element (Position : Cursor) return Log_Level is
   begin
      return MOLP.Element (Position => MOLP.Cursor (Position));
   end Element;

   -------------------------------------------------------------------------

   function Find
     (Map : Wildcard_Level_Map;
      Key : String)
      return Cursor
   is
   begin
      return Cursor (Map.Data.Find (Key => To_Unbounded_String (Key)));
   end Find;

   -------------------------------------------------------------------------

   procedure Insert
     (Map  : in out Wildcard_Level_Map;
      Key  :        String;
      Item :        Log_Level)
   is
      Position  : MOLP.Cursor;
      Insert_Ok : Boolean := False;
   begin
      Map.Data.Insert (Key      => To_Unbounded_String (Key),
                       New_Item => Item,
                       Position => Position,
                       Inserted => Insert_Ok);

      if not Insert_Ok then
         Map.Data.Replace_Element (Position => Position,
                                   New_Item => Item);
      end if;
   end Insert;

   -------------------------------------------------------------------------

   function Length (Map : Wildcard_Level_Map) return Natural is
   begin
      return Natural (Map.Data.Length);
   end Length;

   -------------------------------------------------------------------------

   function Lookup
     (Map : Wildcard_Level_Map;
      Key : String)
      return Cursor
   is
      Position : Cursor;
   begin

      --  Exact match

      Position := Map.Find (Key => Key);

      if Position /= No_Element then
         return Position;
      end if;

      Find_Closest_Match :
      declare
         Substr_Index : Integer := Key'Last;
      begin
         while Substr_Index > 0 loop
            Position := Map.Find
              (Key => Key (Key'First .. Substr_Index) & "." & Wildcard);

            if Position /= No_Element then
               return Position;
            end if;

            Substr_Index := Ada.Strings.Fixed.Index
              (Source  => Key,
               Pattern => ".",
               From    => Substr_Index,
               Going   => Ada.Strings.Backward) - 1;
         end loop;
      end Find_Closest_Match;

      return No_Element;
   end Lookup;

end Alog.Maps;
