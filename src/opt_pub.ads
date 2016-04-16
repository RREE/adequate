--  options for aq_pub
with GNAT.Strings;                 use GNAT.Strings;

package Opt_Pub is
   Port             : aliased Integer := 1883;
   Topic_Text       : aliased String_Access;
   Message_Text     : aliased String_Access;
   Server_Name_Text : aliased String_Access := new String'("localhost");
   Trace_Level      : aliased Integer := 0;
   procedure Set_Options;
end Opt_Pub;
