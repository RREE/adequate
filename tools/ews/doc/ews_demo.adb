--  Copyright 2013, 2014, 2018 Simon Wright <simon@pushface.org>
--
--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO; use Ada.Text_IO;
with EWS.Dynamic;
with EWS.HTTP;
with EWS.Server;
with EWS.Types;
with GNAT.Calendar.Time_IO;
with GNAT.Command_Line;

with EWS_Htdocs;

procedure EWS_Demo is

   use EWS;

   function AJAX_Time
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class;
   function AJAX_Light_State
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class;
   function File_Input
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class;
   function AJAX_Status
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class;
   function AJAX_Change
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class;


   type Date_Format is (ISO, US, European, Locale);
   Current_Date_Format : Date_Format := ISO;
   type Light_State is (Red, Blue);
   Forward_Light : Light_State := Red;
   Aft_Light : Light_State := Red;
   Lamps : array (0 .. 1) of Boolean := (others => True);


   function AJAX_Time
     (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (From_Request);
      function Format return GNAT.Calendar.Time_IO.Picture_String;
      function Format return GNAT.Calendar.Time_IO.Picture_String is
      begin
         case Current_Date_Format is
            when ISO => return GNAT.Calendar.Time_IO.ISO_Date;
            when US => return GNAT.Calendar.Time_IO.US_Date;
            when European => return GNAT.Calendar.Time_IO.European_Date;
            when Locale => return "%c";
         end case;
      end Format;
   begin
      Dynamic.Set_Content_Type (Result, To => Types.Plain);
      Dynamic.Set_Content
        (Result,
         GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, Format));
      return Result;
   end AJAX_Time;
   function AJAX_Light_State
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (From_Request);
   begin
      Dynamic.Set_Content_Type (Result, To => Types.XML);
      Dynamic.Append (Result, "<lights>");
      Dynamic.Append_Element
        (Result,
         "forward-light",
         Ada.Strings.Fixed.Translate
           (Forward_Light'Img,
            Ada.Strings.Maps.Constants.Lower_Case_Map));
      Dynamic.Append_Element
        (Result,
         "aft-light",
         Ada.Strings.Fixed.Translate
           (Aft_Light'Img,
            Ada.Strings.Maps.Constants.Lower_Case_Map));
      Dynamic.Append (Result, "</lights>");
      return Result;
   end AJAX_Light_State;
   function File_Input
     (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class is
      function Upload_Result (Message : String)
        return Dynamic.Dynamic_Response'Class;
      function Upload_Result (Message : String)
        return Dynamic.Dynamic_Response'Class is
         Result : Dynamic.Dynamic_Response (From_Request);
      begin
         Dynamic.Set_Content_Type (Result, To => Types.HTML);
         Dynamic.Append (Result, "<body onload=""alert('");
         for C in Message'Range loop
            case Message (C) is
               when ASCII.CR | ASCII.NUL => null;
               when ASCII.LF => Dynamic.Append (Result, "\n");
               when others =>
                  Dynamic.Append (Result, String'(1 => Message (C)));
            end case;
         end loop;
         Dynamic.Append (Result, "')"">");
         return Result;
      end Upload_Result;
      C : HTTP.Cursor;
      Lines : Natural := 0;
      Line : String (1 .. 1024);
      Last : Natural;
      Attachments : constant HTTP.Attachments
        := HTTP.Get_Attachments (From_Request.all);
   begin
      Put_Line ("saw fileInput with attachment length"
                & HTTP.Get_Content (Attachments)'Length'Img);
      if HTTP.Get_Content (Attachments)'Length /= 0 then
         begin
            HTTP.Open (C, Attachments);
            while not HTTP.End_Of_File (C) loop
               Lines := Lines + 1;
               Put (Lines'Img & ": ");
               HTTP.Get_Line (C, Line, Last);
               Put_Line (Line (1 .. Last));
            end loop;
            HTTP.Close (C);
            return Upload_Result
              ("Upload complete," & Lines'Img & " lines.");
         exception
            when E : others =>
               begin
                  HTTP.Close (C);
               exception
                  when others => null;
               end;
               return Upload_Result
                 ("Upload failed: " & Ada.Exceptions.Exception_Message (E));
         end;
      else
         declare
            Result : Dynamic.Dynamic_Response (From_Request);
         begin
            Dynamic.Set_Content_Type (Result, To => Types.Plain);
            Dynamic.Set_Content (Result, "null");
            return Result;
         end;
      end if;
   end File_Input;
   function AJAX_Status
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (From_Request);
   begin
      Dynamic.Set_Content_Type (Result, To => Types.XML);
      Dynamic.Append (Result, "<state>");
      Dynamic.Append_Element
        (Result,
         "time-format",
         Ada.Strings.Fixed.Translate
           (Current_Date_Format'Img,
            Ada.Strings.Maps.Constants.Lower_Case_Map));
      Dynamic.Append_Element
        (Result,
         "forward-light",
         Ada.Strings.Fixed.Translate
           (Forward_Light'Img,
            Ada.Strings.Maps.Constants.Lower_Case_Map));
      Dynamic.Append_Element
        (Result,
         "aft-light",
         Ada.Strings.Fixed.Translate
           (Aft_Light'Img,
            Ada.Strings.Maps.Constants.Lower_Case_Map));
      for L in Lamps'Range loop
         Dynamic.Append_Element
           (Result,
            "lamp",
            Ada.Strings.Fixed.Translate
              (Lamps (L)'Img,
               Ada.Strings.Maps.Constants.Lower_Case_Map));
      end loop;
      Dynamic.Append (Result, "</state>");
      return Result;
   end AJAX_Status;
   function AJAX_Change
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (From_Request);
   begin
      Put_Line ("AJAX_Change called.");
      declare
         Property : constant String
           := EWS.HTTP.Get_Property ("timeFormat", From_Request.all);
      begin
         if Property /= "" then
            Put_Line ("saw timeFormat=" & Property);
            Current_Date_Format := Date_Format'Value (Property);
         end if;
      end;
      declare
         Property : constant String
           := EWS.HTTP.Get_Property ("forward-light", From_Request.all);
      begin
         if Property /= "" then
            Put_Line ("saw forward-light=" & Property);
            Forward_Light := Light_State'Value (Property);
         end if;
      end;
      declare
         Property : constant String
           := EWS.HTTP.Get_Property ("aft-light", From_Request.all);
      begin
         if Property /= "" then
            Put_Line ("saw aft-light=" & Property);
            Aft_Light := Light_State'Value (Property);
         end if;
      end;
      declare
         Lamp : constant String
           := EWS.HTTP.Get_Property ("lamp", From_Request.all);
      begin
         if Lamp /= "" then
            declare
               Checked : constant String
                 := EWS.HTTP.Get_Property ("checked", From_Request.all);
               Value : constant String
                 := EWS.HTTP.Get_Property ("value", From_Request.all);
            begin
               Put_Line ("saw lamp=" & Lamp
                         & " value=" & Value
                         & " checked=" & Checked);
               Lamps (Natural'Value (Lamp)) := Boolean'Value (Checked);
            end;
         end if;
      end;

      Dynamic.Set_Content_Type (Result, To => Types.Plain);
      Dynamic.Set_Content (Result, "OK");
      return Result;
   end AJAX_Change;


   Verbose : Boolean := False;

begin

   begin
      loop
         case GNAT.Command_Line.Getopt ("v") is
            when 'v' =>
               Verbose := True;
            when ASCII.NUL =>
               exit;
            when others =>
               null;  -- never taken
         end case;
      end loop;
   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Put_Line (Standard_Error,
                   "invalid switch -" & GNAT.Command_Line.Full_Switch);
         return;
   end;

   Dynamic.Register (AJAX_Time'Unrestricted_Access, "/ajaxTime");
   Dynamic.Register (AJAX_Light_State'Unrestricted_Access, "/lightState.xml");
   Dynamic.Register (File_Input'Unrestricted_Access, "/fileInput");
   Dynamic.Register (AJAX_Status'Unrestricted_Access, "/state.xml");
   Dynamic.Register (AJAX_Change'Unrestricted_Access, "/ajaxChange");


   Server.Serve (Using_Port => 8888,
                 With_Stack => 40_000,
                 Tracing => Verbose);

   delay 1_000_000.0;

end EWS_Demo;
