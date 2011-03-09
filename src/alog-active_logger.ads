--
--  Copyright (c) 2009-2011,
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

with Ada.Finalization;

with Alog.Facilities;
with Alog.Transforms;
with Alog.Tasked_Logger;
with Alog.Protected_Containers;

--  Active Logger instance. This logger is an active object and implements
--  concurrent, asynchronous logging. It provides the same functionality as the
--  'simple' logger.
package Alog.Active_Logger is

   type Instance (Init : Boolean) is tagged limited private;
   --  Active logger instance. Incoming messages (via Log_Message) are put into
   --  a request queue. This queue is consumed by a logging task. Exceptions
   --  that might be thrown while logging are saved into a map on a per-caller
   --  basis.

   type Handle is access all Instance;
   --  Handle to active logger type.

   procedure Attach_Facility
     (Logger   : in out Instance;
      Facility :        Facilities.Handle);
   --  Attach a facility to logger instance.

   procedure Attach_Default_Facility (Logger : in out Instance);
   --  Attach default facility with name Default_Facility_Name to logger
   --  instance. If the default facility is already attached do nothing.

   procedure Detach_Facility
     (Logger : in out Instance;
      Name   :        String);
   --  Detach a facility with name 'Name' from logger instance. If the facility
   --  is not found a Facility_Not_Found exception is raised.

   procedure Detach_Default_Facility (Logger : in out Instance);
   --  Detach default facility with name Default_Facility_Name from logger
   --  instance. If the default facility is not attached do nothing.

   function Facility_Count (Logger : Instance) return Natural;
   --  Return number of attached facilites.

   procedure Update
     (Logger  : in out Instance;
      Name    :        String;
      Process :        Tasked_Logger.Facility_Update_Handle);
   --  Update a specific Facility identified by 'Name'. Call the 'Process'
   --  procedure to perform the update operation.

   procedure Iterate
     (Logger  : in out Instance;
      Process :        Tasked_Logger.Facility_Update_Handle);
   --  Call 'Process' for all attached facilities.

   procedure Attach_Transform
     (Logger    : in out Instance;
      Transform :        Transforms.Handle);
   --  Attach a transform to logger instance.

   procedure Detach_Transform
     (Logger : in out Instance;
      Name   :        String);
   --  Detach a transform with name 'Name' from logger instance. If the
   --  transform is not found a Transform_Not_Found exception is raised.

   function Transform_Count (Logger : Instance) return Natural;
   --  Return number of attached transforms.

   procedure Clear (Logger : in out Instance);
   --  Clear logger instance. Detach and teardown all attached facilities and
   --  transforms.

   procedure Log_Message
     (Logger : in out Instance;
      Source :        String := "";
      Level  :        Log_Level;
      Msg    :        String);
   --  Log the given message asynchronously. The message is put into a log
   --  request queue which is continuously consumed by a logging task.
   --
   --  This procedure is *safe* to call from protected actions (e.g. from an
   --  entry call statement or rendezvous).

   function Get_Queue_Length (Logger : Instance) return Natural;
   --  Returns the number of currently queued log messages.

   procedure Shutdown
     (Logger : in out Instance;
      Flush  :        Boolean := True);
   --  Shutdown the active logger. This procedure must be called in order for
   --  the logger task to be terminated properly. If 'Flush' is set to True the
   --  procedure will wait for all queued messages to be logged.

   function Is_Terminated (Logger : Instance) return Boolean;
   --  Returns True if active logger shutdown sequence is complete.

   procedure All_Done (Logger : in out Instance);
   --  This procedure blocks until all queued logging requests have been
   --  consumed.

   type Shutdown_Helper (Logger : not null access Instance) is private;
   --  This helper will call Shutdown on the logger given as discriminant when
   --  it goes out of scope. This relieves the user from having to excplicitly
   --  call shutdown on an instance of Alog active logger when wanting to
   --  terminate. Users must make sure to declare any shutdown helper in a
   --  smaller scope than the active logger on which the helper supposed to
   --  work.

private

   task type Logging_Task (Parent : not null access Instance);
   --  This task takes logging requests from the parent's message queue and
   --  logs them using the parent's backend logger.

   protected type Trigger_Type is
      procedure Shutdown;
      entry Stop;
   private
      Shutdown_Requested : Boolean := False;
   end Trigger_Type;
   --  This trigger is used to terminate the logger task by means of ATC.

   type Instance (Init : Boolean) is tagged limited record
      Logger_Task   : Logging_Task (Parent => Instance'Access);
      Backend       : Tasked_Logger.Instance (Init);
      Message_Queue : Protected_Containers.Log_Request_List;
      Trigger       : Trigger_Type;
   end record;

   type Shutdown_Helper (Logger : not null access Instance) is
     new Ada.Finalization.Controlled with null record;

   overriding
   procedure Finalize (Helper : in out Shutdown_Helper);
   --  Call shutdown on the active logger instance specified as discriminat.

end Alog.Active_Logger;
