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

with Alog.Policy_DB;
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

   procedure Check_Exception (Logger : in out Instance) is
      EO : Ada.Exceptions.Exception_Occurrence;
   begin
      Logger.Backend.Get_Last_Exception (Occurrence => EO);
      Ada.Exceptions.Reraise_Occurrence (X => EO);
   end Check_Exception;

   -------------------------------------------------------------------------

   procedure Clear (Logger : in out Instance) is
   begin
      Logger.Backend.Clear;
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
      F_Count : Natural;
   begin
      Logger.Backend.Facility_Count (Count => F_Count);
      return F_Count;
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
   begin
      Logger.Backend.Get_Last_Exception
        (Occurrence => Occurrence,
         Caller     => Ada.Task_Identification.Current_Task);
   end Get_Last_Exception;

   -------------------------------------------------------------------------

   function Is_Terminated (Logger : Instance) return Boolean is
   begin
      return Logger.Backend'Terminated
        and then Logger.Logger_Task'Terminated;
   end Is_Terminated;

   -------------------------------------------------------------------------

   procedure Iterate
     (Logger  : in out Instance;
      Process : not null access
        procedure (Facility_Handle : Facilities.Handle))
   is
   begin
      Logger.Backend.Iterate (Process);
      Logger.Check_Exception;
   end Iterate;

   -------------------------------------------------------------------------

   procedure Log_Message
     (Logger : in out Instance;
      Source :        String := "";
      Level  :        Log_Level;
      Msg    :        String)
   is
   begin
      if not Policy_DB.Accept_Src
        (Source => Source,
         Level  => Level)
      then
         return;
      end if;

      declare
         New_Request : constant Log_Request.Instance :=
           Log_Request.Create
             (ID      => Ada.Task_Identification.Current_Task,
              Source  => Source,
              Level   => Level,
              Message => Msg);
      begin
         Logger.Message_Queue.Put (Element => New_Request);
      end;
   end Log_Message;

   -------------------------------------------------------------------------

   procedure Shutdown
     (Logger : in out Instance;
      Flush  :        Boolean := True) is
   begin
      if Logger.Is_Terminated then
         return;
      end if;

      if Flush then
         Logger.Message_Queue.All_Done;
      end if;

      Logger.Clear;
      Logger.Trigger.Shutdown;
      Logger.Backend.Shutdown;
   end Shutdown;

   -------------------------------------------------------------------------

   function Transform_Count (Logger : Instance) return Natural is
      T_Count : Natural;
   begin
      Logger.Backend.Transform_Count (Count => T_Count);
      return T_Count;
   end Transform_Count;

   -------------------------------------------------------------------------

   procedure Update
     (Logger  : in out Instance;
      Name    :        String;
      Process : not null access
        procedure (Facility_Handle : Facilities.Handle))
   is
   begin
      Logger.Backend.Update (Name    => Name,
                             Process => Process);
      Logger.Check_Exception;
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

               Parent.Backend.Log_Message
                 (Source => Current_Request.Get_Source,
                  Level  => Current_Request.Get_Log_Level,
                  Msg    => Current_Request.Get_Message,
                  Caller => Current_Request.Get_Caller_ID);

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
