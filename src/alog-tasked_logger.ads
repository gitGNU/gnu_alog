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
with Alog.Transforms;

--  Tasked Logger instance. Facilities can be attached to this logger instance
--  in order to log to different targets simultaneously. This instance provides
--  task-safe concurrent logging.
package Alog.Tasked_Logger is

   task type Instance (Init : Boolean := False) is

      entry Attach_Facility (Facility : Facilities.Handle);
      --  Attach a facility to tasked logger instance.

      entry Attach_Default_Facility;
      --  Attach default facility to tasked logger instance.

      entry Detach_Facility (Name : String);
      --  Detach a facility from tasked logger instance.

      entry Detach_Default_Facility;
      --  Detach default facility from tasked logger instance.

      entry Facility_Count (Count : out Natural);
      --  Return number of attached facilites.

      entry Attach_Transform (Transform : Transforms.Handle);
      --  Attach a transform to tasked logger instance.

      entry Detach_Transform (Name : String);
      --  Detach a transform from tasked logger instance.

      entry Transform_Count (Count : out Natural);
      --  Return number of attached transforms.

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
      --  facilities and transforms.

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

   type Handle is access all Instance;
   --  Handle to tasked logger type.

end Alog.Tasked_Logger;
