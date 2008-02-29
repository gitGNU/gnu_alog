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

--  SMTP-Logging facility. Used to send log-messages to a configurable
--  mailserver. AWS must be installed for this facility to work.
package Alog.Facilities.SMTP is

   type Instance is new Alog.Facilities.Instance with private;
   --  SMTP based logging facility.

   type Handle is access all Instance;

   overriding
   procedure Write_Message (F     : in Instance;
                            Level : in Log_Level := INFO;
                            Msg   : in String);
   --  Implementation of Write_Message.

   overriding
   procedure Teardown (F : in out Instance);
   --  Implementation of Teardown-procedure.

private

   type Instance is limited new Alog.Facilities.Instance with null record;


end Alog.Facilities.SMTP;
