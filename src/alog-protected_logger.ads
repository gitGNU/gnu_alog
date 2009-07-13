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

with Alog.Logger;
with Alog.Transforms;
with Alog.Facilities;

--  Protected logger instance. This instance provides task-safe concurrent
--  access to a logger.
package Alog.Protected_Logger is

   protected type Instance (Init : Boolean) is
      --  Protected logger type. This logger provides task-safe concurrent
      --  logging.

      procedure Attach_Facility (Facility : Facilities.Handle);
      --  Attach a facility to protected logger instance.

      procedure Attach_Default_Facility;
      --  Attach default facility with name Default_Facility_Name to protected
      --  logger instance. If the default facility is already attached do
      --  nothing.

      procedure Detach_Facility (Name : String);
      --  Detach a facility with name 'Name' from logger instance. If the
      --  facility is not found a Facility_Not_Found exception is raised.

      procedure Detach_Default_Facility;
      --  Detach default facility with name Default_Facility_Name from protected
      --  logger instance. If the default facility is not attached do nothing.

      function Facility_Count return Natural;
      --  Return number of attached facilites.

      procedure Update
        (Name    : String;
         Process : not null access
           procedure (Facility_Handle : in out Facilities.Handle));
      --  Update a specific Facility identified by 'Name'. Call the 'Process'
      --  procedure to perform the update operation.

      procedure Iterate
        (Process : not null access
           procedure (Facility_Handle : in out Facilities.Handle));
      --  Call 'Process' for all attached facilities.

      procedure Attach_Transform (Transform : Transforms.Handle);
      --  Attach a transform to protected logger instance.

      procedure Detach_Transform (Name : String);
      --  Detach a transform with name 'Name' from protected logger instance. If
      --  the transform is not found a Transform_Not_Found exception is raised.

      function Transform_Count return Natural;
      --  Return number of attached transforms.

      procedure Update
        (Name    : String;
         Process : not null access
           procedure (Transform_Handle : Transforms.Handle));
      --  Update a specific Transform identified by 'Name'. Call the 'Process'
      --  procedure to perform the update operation.

      procedure Iterate
        (Process : not null access
           procedure (Transform_Handle : Transforms.Handle));
      --  Call 'Process' for all attached transforms.

      procedure Clear;
      --  Clear logger instance. Detach and teardown all attached facilities and
      --  transforms.

      procedure Log_Message
        (Level : Log_Level;
         Msg   : String);
      --  Log a message. The Write_Message() procedure of all attached
      --  facilities is called. Depending on the Log-Threshold set, the message
      --  is logged to different targets (depending on the facilites)
      --  automatically.

   private
      Logsink : Logger.Instance (Init);
   end Instance;

   type Handle is access all Instance;
   --  Handle to protected logger type.

end Alog.Protected_Logger;
