--
--  Copyright (c) 2009,
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
--

with Alog.Facilities;

--  Tasked Logger instance. Facilities can be attached to this logger
--  instance in order to log to different targets simultaneously. This instance
--  provides task-safe concurrent logging.
package Alog.Logger.Tasking is

   task type Instance is

      entry Attach_Facility (Facility : Facilities.Handle);
      --  Attach a facility to tasked logger instance.

      entry Detach_Facility (Facility : Facilities.Handle);
      --  Detach a facility from tasked logger instance.

      entry Facility_Count (Count : out Natural);
      --  Return number of attached facilites.

      entry Log_Message (Level : Log_Level;
                         Msg   : String);
      --  Log a message. The Write_Message() procedure of all attached
      --  facilities is called. Depending on the Log-Threshold set, the message
      --  is logged to different targets (depending on the facilites)
      --  automatically.

      entry Clear;
      --  Clear tasked logger instance. Detach and teardown all attached
      --  facilities.

   end Instance;

end Alog.Logger.Tasking;
