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
with Alog.Protected_Containers;

package body Alog.Tasked_Logger is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   procedure F_Dummy (Facility_Handle : Facilities.Handle) is null;
   --  This procedure is needed to initialize the 'Current_Facility_Proc'
   --  handle of type Facility_Update_Handle since that type is defined as
   --  'not null'.

   -------------------------------------------------------------------------

   task body Instance is
      use type Ada.Task_Identification.Task_Id;

      Logsink               : Alog.Logger.Instance (Init => Init);
      Current_Level         : Log_Level;
      Current_Message       : Unbounded_String;
      Current_Caller        : Ada.Task_Identification.Task_Id;
      Current_Facility_Name : Unbounded_String;
      Current_Facility_Proc : Facility_Update_Handle := F_Dummy'Access;
      Exceptions            : Protected_Containers.Protected_Exception_Map;
   begin

      loop
         begin

            select

               ----------------------------------------------------------------

               accept Attach_Facility (Facility : Facilities.Handle) do
                  Logsink.Attach_Facility (Facility => Facility);
               end Attach_Facility;
            or

               ----------------------------------------------------------------

               accept Attach_Default_Facility do
                  Logsink.Attach_Default_Facility;
               end Attach_Default_Facility;
            or

               ----------------------------------------------------------------

               accept Detach_Facility (Name : String) do
                  Logsink.Detach_Facility (Name => Name);
               end Detach_Facility;
            or

               ----------------------------------------------------------------

               accept Detach_Default_Facility do
                  Logsink.Detach_Default_Facility;
               end Detach_Default_Facility;
            or

               ----------------------------------------------------------------

               accept Facility_Count (Count : out Natural) do
                  Count := Logsink.Facility_Count;
               end Facility_Count;
            or

               ----------------------------------------------------------------

               accept Update
                 (Name    : String;
                  Process : Facility_Update_Handle)
               do
                  Current_Facility_Name := To_Unbounded_String (Name);
                  Current_Facility_Proc := Process;
                  Current_Caller        := Instance.Update'Caller;
               end Update;

               if Exceptions.Contains (Key => Current_Caller) then
                  Exceptions.Delete (Key => Current_Caller);
               end if;

               begin
                  Logsink.Update (Name    => To_String (Current_Facility_Name),
                                  Process => Current_Facility_Proc);

               exception
                  when E : others =>
                     Exceptions.Insert
                       (Key  => Current_Caller,
                        Item => Save_Occurrence (Source => E));
               end;
            or

               ----------------------------------------------------------------

               accept Iterate (Process : Facility_Update_Handle) do
                  Current_Facility_Proc := Process;
                  Current_Caller        := Instance.Iterate'Caller;
               end Iterate;

               if Exceptions.Contains (Key => Current_Caller) then
                  Exceptions.Delete (Key => Current_Caller);
               end if;

               begin
                  Logsink.Iterate (Process => Current_Facility_Proc);

               exception
                  when E : others =>
                     Exceptions.Insert
                       (Key  => Current_Caller,
                        Item => Save_Occurrence (Source => E));
               end;
            or

               ----------------------------------------------------------------

               accept Attach_Transform (Transform : Transforms.Handle) do
                  Logsink.Attach_Transform (Transform => Transform);
               end Attach_Transform;
            or

               ----------------------------------------------------------------

               accept Detach_Transform (Name : String) do
                  Logsink.Detach_Transform (Name => Name);
               end Detach_Transform;

            or
               ----------------------------------------------------------------

               accept Transform_Count (Count : out Natural) do
                  Count := Logsink.Transform_Count;
               end Transform_Count;

            or

               ----------------------------------------------------------------

               accept Clear do
                  Logsink.Clear;
               end Clear;
            or

               ----------------------------------------------------------------

               accept Get_Last_Exception
                 (Occurrence : out Exception_Occurrence;
                  Caller     :     Ada.Task_Identification.Task_Id :=
                    Ada.Task_Identification.Null_Task_Id)
               do

                  --  Get_Last_Exception'Caller can not be used as default
                  --  parameter so we need to check for 'Null_Task_Id 'instead.

                  if Caller = Ada.Task_Identification.Null_Task_Id then
                     Current_Caller := Get_Last_Exception'Caller;
                  else
                     Current_Caller := Caller;
                  end if;

                  Exceptions.Get (Key     => Current_Caller,
                                  Element => Occurrence);
               end Get_Last_Exception;
            or

               ----------------------------------------------------------------

               accept Log_Message
                 (Level  : Log_Level;
                  Msg    : String;
                  Caller : Ada.Task_Identification.Task_Id :=
                    Ada.Task_Identification.Null_Task_Id)
               do
                  Current_Level   := Level;
                  Current_Message := To_Unbounded_String (Msg);

                  --  Log_Message'Caller can not be used as default parameter so
                  --  we need to check for 'Null_Task_Id' instead.

                  if Caller = Ada.Task_Identification.Null_Task_Id then
                     Current_Caller := Log_Message'Caller;
                  else
                     Current_Caller := Caller;
                  end if;
               end Log_Message;

               if Exceptions.Contains (Key => Current_Caller) then
                  Exceptions.Delete (Key => Current_Caller);
               end if;

               begin
                  Logsink.Log_Message
                    (Level => Current_Level,
                     Msg   => To_String (Current_Message));

               exception
                  when E : others =>
                     Exceptions.Insert
                       (Key  => Current_Caller,
                        Item => Save_Occurrence (Source => E));

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

end Alog.Tasked_Logger;