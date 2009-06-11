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

package body Alog.Logger.Tasking is

   use Ada.Exceptions;

   -------------------------------------------------------------------------

   task body Instance is
      Logsink                   : Logger.Instance (Init => Init);
      Current_Level             : Log_Level;
      Current_Message           : Unbounded_String;
      Last_Exception_Occurrence : Exception_Occurrence;
   begin

      --  Initialize limited Last_Exception_Occurrence to Null_Occurrence.

      Save_Occurrence (Target => Last_Exception_Occurrence,
                       Source => Null_Occurrence);

      loop
         begin

            select

               ----------------------------------------------------------------

               accept Attach_Facility (Facility : Facilities.Handle) do
                  Logsink.Attach_Facility (Facility => Facility);
               end Attach_Facility;
            or

               ----------------------------------------------------------------

               accept Detach_Facility (Name : String) do
                  Logsink.Detach_Facility (Name => Name);
               end Detach_Facility;
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

               accept Get_Last_Exception
                 (Occurrence : out Exception_Occurrence)
               do
                  Save_Occurrence (Target => Occurrence,
                                   Source => Last_Exception_Occurrence);
               end Get_Last_Exception;
            or

               ----------------------------------------------------------------

               accept Log_Message
                 (Level : Log_Level;
                  Msg   : String)
               do
                  Current_Level   := Level;
                  Current_Message := To_Unbounded_String (Msg);
               end Log_Message;

               begin
                  Logsink.Log_Message (Level => Current_Level,
                                       Msg   => To_String (Current_Message));

                  Save_Occurrence (Target => Last_Exception_Occurrence,
                                   Source => Null_Occurrence);

               exception
                  when E : others =>
                     Ada.Exceptions.Save_Occurrence
                       (Target => Last_Exception_Occurrence,
                        Source => E);
               end;
            or

               terminate;
            end select;

            --  Exceptions raised during a rendezvous are raised here and in the
            --  calling task. Catch and ignore it so the tasked logger does not
            --  get terminated after an exception.

         exception
            when others =>
               null;
         end;
      end loop;
   end Instance;

end Alog.Logger.Tasking;
