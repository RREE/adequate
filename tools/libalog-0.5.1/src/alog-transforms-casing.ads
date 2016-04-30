--
--  Copyright (c) 2008,
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

--  Casing transform. Used to transform casing (lower/uppercase) of messages
package Alog.Transforms.Casing is

   type Operation_Mode is (Uppercase, Lowercase);

   type Instance is new Alog.Transforms.Instance with private;
   --  Casing transform.

   type Handle is access all Instance;

   overriding
   function Transform_Message
     (Transform : Instance;
      Level     : Log_Level := Info;
      Msg       : String)
      return String;
   --  Implementation of Transform_Message.

   procedure Set_Mode
     (Transform : in out Instance;
      Mode      :        Operation_Mode);
   --  Set operation mode of transform.

private

   type Instance is new Alog.Transforms.Instance with record
      Mode  : Operation_Mode := Lowercase;
      --  Mode of operation.
   end record;

end Alog.Transforms.Casing;
