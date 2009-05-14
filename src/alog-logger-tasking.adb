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

with Ada.Strings.Unbounded;

with Alog.Logger;

package body Alog.Logger.Tasking is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   task body Instance is
      Logsink         : Logger.Instance;
      Current_Handle  : Facilities.Handle;
      Current_Level   : Log_Level;
      Current_Message : Unbounded_String;
   begin
      loop
         select

            ----------------------------------------------------------------

            accept Attach_Facility (Facility : Facilities.Handle) do
               Current_Handle := Facility;
            end Attach_Facility;
            Logsink.Attach_Facility (Facility => Current_Handle);
         or

            ----------------------------------------------------------------

            accept Detach_Facility (Facility : Facilities.Handle) do
               Current_Handle := Facility;
            end Detach_Facility;
            Logsink.Detach_Facility (Facility => Current_Handle);
         or

            ----------------------------------------------------------------

            accept Facility_Count (Count : out Natural) do
               Count := Logsink.Facility_Count;
            end Facility_Count;
         or

            ----------------------------------------------------------------

            accept Clear do
               Logsink.Clear;
            end Clear;
         or

            ----------------------------------------------------------------

            accept Log_Message
              (Level : Log_Level;
               Msg   : String)
            do
               Current_Level   := Level;
               Current_Message := To_Unbounded_String (Msg);
            end Log_Message;
            Logsink.Log_Message (Level => Current_Level,
                                 Msg   => To_String (Current_Message));
         or
            terminate;
         end select;
      end loop;
   end Instance;

end Alog.Logger.Tasking;
