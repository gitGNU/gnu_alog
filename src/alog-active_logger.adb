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

with Ada.Task_Identification;

with Alog.Log_Request;

package body Alog.Active_Logger is

   -------------------------------------------------------------------------

   procedure All_Done (Logger : in out Instance) is
   begin
      Logger.Message_Queue.All_Done;
   end All_Done;

   -------------------------------------------------------------------------

   procedure Attach_Default_Facility (Logger : in out Instance)
   is
   begin
      Logger.Backend.Attach_Default_Facility;
   end Attach_Default_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Facility
     (Logger   : in out Instance;
      Facility :        Facilities.Handle)
   is
   begin
      Logger.Backend.Attach_Facility (Facility);
   end Attach_Facility;

   -------------------------------------------------------------------------

   procedure Attach_Transform
     (Logger    : in out Instance;
      Transform :        Transforms.Handle)
   is
   begin
      Logger.Backend.Attach_Transform (Transform);
   end Attach_Transform;

   -------------------------------------------------------------------------

   procedure Clear (Logger : in out Instance) is
   begin
      Logger.Backend.Clear;
      Logger.Exceptions.Clear;
   end Clear;

   -------------------------------------------------------------------------

   procedure Detach_Default_Facility (Logger : in out Instance)
   is
   begin
      Logger.Backend.Detach_Default_Facility;
   end Detach_Default_Facility;

   -------------------------------------------------------------------------

   procedure Detach_Facility
     (Logger : in out Instance;
      Name   :        String)
   is
   begin
      Logger.Backend.Detach_Facility (Name);
   end Detach_Facility;

   -------------------------------------------------------------------------

   procedure Detach_Transform
     (Logger : in out Instance;
      Name   :        String)
   is
   begin
      Logger.Backend.Detach_Transform (Name);
   end Detach_Transform;

   -------------------------------------------------------------------------

   function Facility_Count (Logger : Instance) return Natural is
   begin
      return Logger.Backend.Facility_Count;
   end Facility_Count;

   -------------------------------------------------------------------------

   procedure Finalize (Helper : in out Shutdown_Helper) is
   begin
      Helper.Logger.Shutdown;
   end Finalize;

   -------------------------------------------------------------------------

   procedure Get_Last_Exception
     (Logger     : in out Instance;
      Occurrence :    out Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
      use Ada.Task_Identification;

      Current_Task_ID : constant Task_Id := Current_Task;
   begin
      if Logger.Exceptions.Contains (Key => Current_Task_ID) then
         declare
            Exception_Handle : Exception_Occurrence;
         begin
            Logger.Exceptions.Get (Key     => Current_Task_ID,
                                   Element => Exception_Handle);
            Save_Occurrence (Target => Occurrence,
                             Source => Exception_Handle);
         end;
      else
         Save_Occurrence (Target => Occurrence,
                          Source => Null_Occurrence);
      end if;
   end Get_Last_Exception;

   -------------------------------------------------------------------------

   procedure Iterate
     (Logger  : in out Instance;
      Process : not null access
        procedure (Facility_Handle : in out Facilities.Handle))
   is
   begin
      Logger.Backend.Iterate (Process);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Iterate
     (Logger  : in out Instance;
      Process : not null access procedure
        (Transform_Handle : Transforms.Handle))
   is
   begin
      Logger.Backend.Iterate (Process);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Log_Message
     (Logger : in out Instance;
      Level  :        Log_Level;
      Msg    :        String)
   is
      New_Request : constant Log_Request.Instance :=
        Log_Request.Create (ID      => Ada.Task_Identification.Current_Task,
                            Level   => Level,
                            Message => Msg);
   begin
      Logger.Message_Queue.Put (Element => New_Request);
   end Log_Message;

   -------------------------------------------------------------------------

   procedure Shutdown
     (Logger : in out Instance;
      Flush  :        Boolean := True) is
   begin
      if Flush then
         Logger.Message_Queue.All_Done;
      end if;

      Logger.Clear;
      Logger.Trigger.Shutdown;
   end Shutdown;

   -------------------------------------------------------------------------

   function Transform_Count (Logger : Instance) return Natural is
   begin
      return Logger.Backend.Transform_Count;
   end Transform_Count;

   -------------------------------------------------------------------------

   procedure Update
     (Logger  : in out Instance;
      Name    :        String;
      Process : not null access
        procedure (Facility_Handle : in out Facilities.Handle))
   is
   begin
      Logger.Backend.Update (Name, Process);
   end Update;

   -------------------------------------------------------------------------

   procedure Update
     (Logger  : in out Instance;
      Name    :        String;
      Process : not null access
        procedure (Transform_Handle : Transforms.Handle))
   is
   begin
      Logger.Backend.Update (Name, Process);
   end Update;

   -------------------------------------------------------------------------

   protected body Trigger_Type is

      ----------------------------------------------------------------------

      procedure Shutdown is
      begin
         Shutdown_Requested := True;
      end Shutdown;

      ----------------------------------------------------------------------

      entry Stop when Shutdown_Requested is
      begin
         null;
      end Stop;

   end Trigger_Type;

   -------------------------------------------------------------------------

   task body Logging_Task is
   begin
      select
         Parent.Trigger.Stop;
      then abort
         Log_Loop :
         loop
            declare
               Current_Request : Log_Request.Instance;
            begin
               Parent.Message_Queue.Get
                 (Element => Current_Request);

               if Parent.Exceptions.Contains
                 (Key => Current_Request.Get_Caller_ID) then
                  Parent.Exceptions.Delete
                    (Key => Current_Request.Get_Caller_ID);
               end if;

               begin
                  Parent.Backend.Log_Message
                    (Level => Current_Request.Get_Log_Level,
                     Msg   => Current_Request.Get_Message);

               exception
                  when E : others =>
                     Parent.Exceptions.Insert
                       (Key  => Current_Request.Get_Caller_ID,
                        Item => Ada.Exceptions.Save_Occurrence (Source => E));
               end;

               Parent.Message_Queue.Done;

            exception
               when Program_Error =>

                  --  The Queue has terminated, let's shutdown.

                  exit Log_Loop;
                  when others =>

                  --  Ignore other errors and resume normal operation.

                  null;
            end;
         end loop Log_Loop;
      end select;

   end Logging_Task;

end Alog.Active_Logger;
