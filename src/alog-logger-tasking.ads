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

with Ada.Exceptions;

with Alog.Facilities;

--  Tasked Logger instance. Facilities can be attached to this logger instance
--  in order to log to different targets simultaneously. This instance provides
--  task-safe concurrent logging.
package Alog.Logger.Tasking is

   task type Instance (Init : Boolean := False) is

      entry Attach_Facility (Facility : Facilities.Handle);
      --  Attach a facility to tasked logger instance.

      entry Detach_Facility (Name : String);
      --  Detach a facility from tasked logger instance.

      entry Facility_Count (Count : out Natural);
      --  Return number of attached facilites.

      entry Get_Facility (Name     :     String;
                          Facility : out Facilities.Handle);
      --  Return a facility specified by the string 'Name'.

      entry Log_Message (Level : Log_Level;
                         Msg   : String);
      --  Log a message. The Write_Message() procedure of all attached
      --  facilities is called. Depending on the Log-Threshold set, the message
      --  is logged to different targets (depending on the facilites)
      --  automatically. Clear the last exception occurrence for the caller if
      --  none occurred or replace existing occurrence with new raised
      --  exception.

      entry Clear;
      --  Clear tasked logger instance. Detach and teardown all attached
      --  facilities.

      entry Get_Last_Exception
        (Occurrence : out Ada.Exceptions.Exception_Occurrence);
      --  Return last known Exception_Occurrence. If no exception occured return
      --  Null_Occurrence.

   end Instance;
   --  Tasked logger instance. The Init discriminant defines whether or not a
   --  default 'stdout' (FD facility without logfile set) is attached
   --  automatically. Default is 'False'. Set Init to 'True' if you want to make
   --  sure minimal stdout logging is possible as soon as a new logger is
   --  instantiated.

end Alog.Logger.Tasking;
