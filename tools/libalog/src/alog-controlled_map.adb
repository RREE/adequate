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

with Ada.Unchecked_Deallocation;

package body Alog.Controlled_Map is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Element_Type,
      Name   => Element_Handle);
   --  Free memory occupied by an element.

   -------------------------------------------------------------------------

   procedure Clear (Container : in out Map) is

      procedure Do_Free (Position : MOEP.Cursor);
      --  Free the memory of an element.

      procedure Do_Free (Position : MOEP.Cursor) is
         Handle : Element_Handle :=
           MOEP.Element (Position => Position);
      begin
         Free (X => Handle);
      end Do_Free;

   begin
      Container.Data.Iterate (Do_Free'Access);
      Container.Data.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Contains
     (Container : Map;
      Key       : Key_Type)
      return Boolean
   is
   begin
      return Container.Data.Contains (Key => Key);
   end Contains;

   -------------------------------------------------------------------------

   procedure Delete
     (Container : in out Map;
      Key       :        Key_Type)
   is
      Handle : Element_Handle :=
        Container.Data.Element (Key => Key);
   begin
      Container.Data.Delete (Key => Key);
      Free (Handle);
   end Delete;

   -------------------------------------------------------------------------

   function Element
     (Container : Map;
      Key       : Key_Type)
      return Element_Handle
   is
   begin
      return Container.Data.Element (Key => Key);
   end Element;

   -------------------------------------------------------------------------

   procedure Finalize (Container : in out Map) is
   begin
      Container.Clear;
   end Finalize;

   -------------------------------------------------------------------------

   procedure Insert
     (Container : in out Map;
      Key       :        Key_Type;
      New_Item  :        Element_Handle)
   is
   begin
      Container.Data.Insert (Key      => Key,
                             New_Item => New_Item);
   end Insert;

   -------------------------------------------------------------------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.Data.Is_Empty;
   end Is_Empty;

   -------------------------------------------------------------------------

   procedure Iterate
     (Container : Map;
      Process   : not null access procedure (Handle : Element_Handle))
   is
      procedure Call_Process (Position : MOEP.Cursor);
      --  Call 'Process' for each element handle.

      procedure Call_Process (Position : MOEP.Cursor) is
         E_Handle : constant Element_Handle :=
           MOEP.Element (Position => Position);
      begin
         Process (Handle => E_Handle);
      end Call_Process;
   begin
      Container.Data.Iterate (Process => Call_Process'Access);
   end Iterate;

   -------------------------------------------------------------------------

   function Length (Container : Map) return Natural is
   begin
      return Natural (Container.Data.Length);
   end Length;

end Alog.Controlled_Map;
