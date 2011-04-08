--
--  Copyright (c) 2008-2011,
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

--  Syslog facility: Used to log to systems syslog.
package Alog.Facilities.Syslog is

   type Instance is new Alog.Facilities.Instance with private;
   --  Syslog based logging facility.

   type Handle is access all Instance;

   type S_Facility is
     (LOG_AUTH,
      LOG_USER,
      LOG_MAIL,
      LOG_DAEMON,
      LOG_SYSLOG,
      LOG_CRON);
   --  Corresponding Ada-Implementation of syslogs "facility" parameter.
   --  Only the important facilities are mapped.

private

   overriding
   procedure Write
     (Facility : Instance;
      Level    : Log_Level := Info;
      Msg      : String);
   --  Implementation of the Write procedure for syslog.

   type Instance is new Alog.Facilities.Instance with null record;

end Alog.Facilities.Syslog;
