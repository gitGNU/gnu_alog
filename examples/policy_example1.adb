with Alog.Policy_DB;
with Alog.Logger;

use Alog;

--  Alog source loglevel policy example.
procedure Policy_Example1 is
   Log : Logger.Instance (Init => True);
begin
   --  Set default loglevel to 'Info'.
   Policy_DB.Set_Default_Loglevel (Level => Info);
   --  Set source specific loglevel for all 'Example' sources to 'Debug'.
   Policy_DB.Set_Loglevel (Identifier => "Example.*",
                           Level      => Debug);

   --  This message will be logged because it matches a source specific
   --  loglevel (Example.*).
   Log.Log_Message (Source => "Example.Source1",
                    Level  => Debug,
                    Msg    => "This is a testmessage");

   --  This message will not be logged because of the configured default 'Info'
   --  loglevel. There's no configured source loglevel for 'Source2'.
   Log.Log_Message (Source => "Source2",
                    Level  => Debug,
                    Msg    => "This will not be logged");

   --  No source specified, will not be logged because of the default 'Info'
   --  loglevel.
   Log.Log_Message (Level => Debug,
                    Msg   => "This will not be logged");
end Policy_Example1;
