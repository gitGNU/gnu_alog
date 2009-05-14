--
--  Copyright (c) 2008,
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
with Ada.Unchecked_Deallocation;
with Ada.Containers.Hashed_Sets;

with Alog.Facilities;
with Alog.Transforms;

--  Logger instance. Facilities can be attached to a logger
--  instance in order to log to different targets simultaneously.
--  A logger provides different helper functions for logging facilities
--  configuration.
package Alog.Logger is

   type Instance is new Ada.Finalization.Controlled with private;
   --  Logger instance.

   procedure Attach_Facility (Logger   : in out Alog.Logger.Instance;
                              Facility :        Alog.Facilities.Handle);
   --  Attach a facility to logger instance.

   procedure Detach_Facility (Logger   : in out Instance;
                              Facility :        Alog.Facilities.Handle);
   --  Detach a facility from logger instance.

   function Facility_Count (Logger : Instance) return Natural;
   --  Return number of attached facilites.

   procedure Attach_Transform (Logger    : in out Instance;
                               Transform :        Alog.Transforms.Handle);
   --  Attach a transform to logger instance.

   procedure Detach_Transform (Logger    : in out Instance;
                               Transform :        Alog.Transforms.Handle);
   --  Detach a transform from logger instance.

   function Transform_Count (Logger : Instance) return Natural;
   --  Return number of attached transforms.

   procedure Clear (L : in out Instance);
   --  Clear logger instance. Detach and teardown all attached
   --  facilities and transforms.

   procedure Log_Message (Logger : Instance;
                          Level  : Log_Level;
                          Msg    : String);
   --  Log a message. The Write_Message() procedure of all attached facilities
   --  is called. Depending on the Log-Threshold set, the message is logged to
   --  different targets (depending on the facilites) automatically.

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Alog.Facilities.Class,
      Name   => Alog.Facilities.Handle);
   --  Free memory allocated by a facility.

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Alog.Transforms.Class,
      Name   => Alog.Transforms.Handle);
   --  Free memory allocated by a transform.

   Facility_Not_Found : exception;
   --  Will be raised if a requested facility is not found.
   Transform_Not_Found : exception;
   --  Will be raised if a requested transform is not found.

private

   use Ada.Containers;
   use Alog.Facilities;
   use Alog.Transforms;

   procedure Finalize (Logger : in out Instance);
   --  Finalize procedure used to cleanup.

   package Facilities_Stack_Package is
     new Ada.Containers.Hashed_Sets
       (Element_Type        => Alog.Facilities.Handle,
        Hash                => Alog.Facilities.Hash,
        Equivalent_Elements => "=");
   --  Storage for attached facilities. Equal-Function is provided
   --  by facility.

   subtype Facilities_Stack is Facilities_Stack_Package.Set;
   --  Manages attached facilities for logger instance.

   package Transforms_Stack_Package is
     new Ada.Containers.Hashed_Sets
       (Element_Type        => Alog.Transforms.Handle,
        Hash                => Alog.Transforms.Hash,
        Equivalent_Elements => "=");
   --  Storage for attached transforms. Equal-Function is provided
   --  by transform.

   subtype Transforms_Stack is Transforms_Stack_Package.Set;
   --  Manages attached transforms for transforms instance.

   type Instance is new Ada.Finalization.Controlled with
      record
         F_Stack : Facilities_Stack;
         --  Stack of attached Facilities.
         T_Stack : Transforms_Stack;
         --  Stack of attached Transforms.
      end record;

end Alog.Logger;
