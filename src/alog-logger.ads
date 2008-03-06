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

--  Ada
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;
--  Alog
with Alog.Facilities;
with Alog.Facilities.File_Descriptor;

--  Logger instance. Facilities can be attached to a logger
--  instance in order to log to different targets simultaneously.
--  A logger provides different helper functions for logging facilities
--  configuration.
package Alog.Logger is

   type Instance is new Ada.Finalization.Controlled with private;
   --  Logger instance.

   procedure Attach_Facility (Logger   : in out Alog.Logger.Instance;
                              Facility : in     Alog.Facilities.Handle);
   --  Attach a facility to logger instance.

   procedure Detach_Facility (Logger   : in out Instance;
                              Facility : in Alog.Facilities.Handle);
   --  Detach a facility by name.

   function Facility_Count (Logger : in Instance) return Natural;
   --  Return number of attached facilites.

   procedure Clear (L : in out Instance);
   --  Clear logger instance. Detach and teardown all attached
   --  facilities.

   procedure Log_Message (Logger : in Instance;
                          Level  : in Log_Level;
                          Msg    : in String);
   --  Log a message. Write_Message() procedure of all attached logger is
   --  called. Depending on the Log-Threshold set, the message is logged
   --  to different targets (depending on the facilites) automatically.

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Alog.Facilities.Class,
      Name   => Alog.Facilities.Handle);
   --  Free memory allocated by a facility.

   Facility_Not_Found : exception;
   --  Will be raised if a requested facility is not found.

private

   use Ada.Containers;
   use Alog.Facilities;

   procedure Finalize (Logger : in out Instance);
   --  Finalize procedure used to cleanup.

   function Hash_Facility (Element : Alog.Facilities.Handle)
                           return Hash_Type;
   --  Helper function to hash facility names.

   package Facilities_Stack_Package is
     new Ada.Containers.Hashed_Sets
       (Element_Type        => Alog.Facilities.Handle,
        Hash                => Hash_Facility,
        Equivalent_Elements => "=");
   --  Storage for attached facilities. Equal-Function is provided
   --  by facility.

   subtype Facilities_Stack is Facilities_Stack_Package.Set;
   --  Manages attached facilities for logger instance.

   type Instance is new Ada.Finalization.Controlled with
      record
         F_Stack : Facilities_Stack;
         --  Stack of attached Facilities.
      end record;

end Alog.Logger;
