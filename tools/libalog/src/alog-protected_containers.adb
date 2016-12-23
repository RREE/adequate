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

package body Alog.Protected_Containers is

   -------------------------------------------------------------------------

   protected body Log_Request_List is

      ----------------------------------------------------------------------

      entry All_Done when Requests.Is_Empty
        and then Pending_Counter = 0
      is
      begin
         null;
      end All_Done;

      ----------------------------------------------------------------------

      procedure Clear is
      begin
         Requests.Clear;
         Requests_Available := False;
         Pending_Counter    := 0;
      end Clear;

      ----------------------------------------------------------------------

      procedure Done is
      begin
         if Pending_Counter = 0 then
            return;
         end if;
         Pending_Counter := Pending_Counter - 1;
      end Done;

      ----------------------------------------------------------------------

      entry Get (Element : out Log_Request.Instance) when Requests_Available
      is
      begin
         Element := Requests.First_Element;
         Requests.Delete_First;
         Pending_Counter := Pending_Counter + 1;

         if Requests.Is_Empty then
            Requests_Available := False;
         end if;
      end Get;

      ----------------------------------------------------------------------

      function Length return Natural is
      begin
         return Natural (Requests.Length);
      end Length;

      ----------------------------------------------------------------------

      function Pending return Natural is
      begin
         return Pending_Counter;
      end Pending;

      ----------------------------------------------------------------------

      procedure Put (Element : Log_Request.Instance) is
      begin
         Requests.Append (New_Item => Element);
         Requests_Available := True;
      end Put;

   end Log_Request_List;

end Alog.Protected_Containers;
