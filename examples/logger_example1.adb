with Alog.Logger;

use Alog;

--  Alog logger example.
procedure Logger_Example1 is
   --  Initialize logger instance with default file descriptor facility
   --  (logs to stdout).
   Log : Logger.Instance (Init => True);
begin
   --  Write a message with loglevel 'Info' to stdout.
   Log.Log_Message
     (Level => Info,
      Msg   => "This is a testmessage from Alog logger");
end Logger_Example1;
