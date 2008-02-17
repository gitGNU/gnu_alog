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
with Alog.Facilities;
with Alog.Facilities.File_Descriptor;

with Ada.Unchecked_Deallocation;

--  Logger instance. Facilities can be attached to a logger
--  instance in order to log to different targets simultaneously.
--  A logger provides different helper functions for logging facilities
--  configuration.
package Alog.Logger is

   type Instance is new Ada.Finalization.Controlled with private;
   --  Logger instance.

   procedure Attach_Facility (L : in out Alog.Logger.Instance;
                              F : in     Alog.Facilities.Handle);
   --  Attach a facility to logger instance.

   function Facility_Count (L : in Instance) return Natural;
   --  Return number of attached facilites.

   Max_Facilites_Reached : exception;
   --  Exception is raised if attached Max_Facilities is already reached
   --  when calling Attach_Facility().

private

   subtype Index_Range is Natural range 0 .. Max_Facilities;
   --  Allowed range of array, index.

   type Facility_Array is array (Index_Range)
     of Alog.Facilities.Handle;
   --  Manages attached facilities for logger instance.

   type Instance is new Ada.Finalization.Controlled with
      record
         F_Index : Index_Range := 0;
         --  Stores number of attached facilities. Default ist 0.

         F_Array : Facility_Array;
      end record;

   procedure Finalize (L : in out Instance);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Alog.Facilities.File_Descriptor.Instance,
      Name   => Alog.Facilities.File_Descriptor.Handle);

end Alog.Logger;
