--
--  Copyright (c) 2008,
--  Reto Buerki, Adrian-Ken Rueegsegger
--  secunet SwissIT AG
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

with Interfaces.C.Strings;
with Interfaces.C;

--  Syslog facility. Used to log to systems syslog.
--  At the moment, this facility is a thin binding to syslog
--  function calls. The implementation attempts to resemble
--  the native libc-functions of your system, so that anyone
--  being familiar with syslog.h should be able to use this
--  module right away.
package Alog.Facilities.Syslog is

   type Instance is new Alog.Facilities.Instance with private;
   --  Syslog based logging facility.

   type S_Facility is (
                       LOG_AUTH,
                       LOG_USER,
                       LOG_MAIL,
                       LOG_DAEMON,
                       LOG_SYSLOG,
                       LOG_CRON
                      );
   --  Corresponding Ada-Implementation of syslogs "facility" parameter for
   --  openlog() call. Only the important/usable facilities are mapped.

   for S_Facility use
     (
      LOG_AUTH   => 0,
      LOG_USER   => 8,
      LOG_MAIL   => 16,
      LOG_DAEMON => 24,
      LOG_SYSLOG => 40,
      LOG_CRON   => 72
     );
   --  Facility map table.

   type Handle is access all Instance;

   overriding
   procedure Write_Message (F     : in Instance;
                            Level : in Log_Level := INFO;
                            Msg   : in String);
   --  Implementation of Write_Message. If Write_Message is called without
   --  calling Openlog() first, probably system dependent default values
   --  will be used as arguments for an implicit call to libc openlog().

   overriding
   procedure Teardown (F : in out Instance);
   --  Implementation of Teardown-procedure.

private

   type Instance is limited new Alog.Facilities.Instance with null record;

end Alog.Facilities.Syslog;
