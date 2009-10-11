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
with Ada.Finalization;

with Alog.Facilities;
with Alog.Transforms;
with Alog.Tasked_Logger;
with Alog.Protected_Containers;

--  Active Logger instance. This logger is an active object and implements
--  concurrent, asynchronous logging. It provides the same functionality as the
--  'simple' logger.
package Alog.Active_Logger is

   type Instance (Init : Boolean) is new
     Ada.Finalization.Limited_Controlled with private;
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
      Process : not null access
        procedure (Facility_Handle : Facilities.Handle));
   --  Update a specific Facility identified by 'Name'. Call the 'Process'
   --  procedure to perform the update operation.

   procedure Iterate
     (Logger  : in out Instance;
      Process : not null access
        procedure (Facility_Handle : Facilities.Handle));
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
   --  transforms and clear the exception map.

   procedure Log_Message
     (Logger : in out Instance;
      Level  :        Log_Level;
      Msg    :        String);
   --  Log a message. The Write_Message() procedure of all attached facilities
   --  is called. Depending on the Log-Threshold set, the message is logged to
   --  different targets (depending on the facilites) automatically.

   procedure Shutdown (Logger : in out Instance;
                       Flush  :        Boolean := True);
   --  Shutdown the active logger. This procedure must be called in order for
   --  the logger task to be terminated properly. If 'Flush' is set to True the
   --  procedure will wait for all queued messages to be logged.

   procedure All_Done (Logger : in out Instance);
   --  This procedure blocks until all queued logging requests have been
   --  consumed.

   procedure Get_Last_Exception
     (Logger     : in out Instance;
      Occurrence :    out Ada.Exceptions.Exception_Occurrence);
   --  Return last known Exception_Occurrence for caller. If no exception
   --  occured return Null_Occurrence.

   type Shutdown_Helper (Logger : not null access Instance) is
     new Ada.Finalization.Limited_Controlled with private;
   --  This helper will call Shutdown on the logger given as discriminant when
   --  it goes out of scope. This relieves the user from having to excplicitly
   --  call shutdown on an instance of Alog active logger when wanting to
   --  terminate. Users must make sure to declare any shutdown helper in a
   --  smaller scope than the active logger on which the helper supposed to
   --  work.

private

   procedure Check_Exception (Logger : in out Instance);
   --  Check if call to backend raised an exception. Explicitly reraise it if
   --  such an exception occured do nothing otherwise.

   task type Logging_Task (Parent : not null access Instance);
   --  This task takes logging requests from the parent's message queue and
   --  tries to actually log them to the parent's backend logger. If an
   --  exception occurrs it is stored in the parent's exception map.

   protected type Trigger_Type is
      procedure Shutdown;
      entry Stop;
   private
      Shutdown_Requested : Boolean := False;
   end Trigger_Type;
   --  This trigger is used to terminate the logger task by means of ATC.

   type Instance (Init : Boolean) is new
     Ada.Finalization.Limited_Controlled with record
      Logger_Task   : Logging_Task (Parent => Instance'Access);
      Backend       : Tasked_Logger.Instance (Init);
      Message_Queue : Protected_Containers.Log_Request_List;
      Trigger       : Trigger_Type;
   end record;

   overriding
   procedure Finalize (Helper : in out Shutdown_Helper);
   --  Call shutdown on the active logger instance specified as discriminat.

   type Shutdown_Helper (Logger : not null access Instance) is
     new Ada.Finalization.Limited_Controlled with null record;

end Alog.Active_Logger;
