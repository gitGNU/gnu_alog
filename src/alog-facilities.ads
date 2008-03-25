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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with Ada.Calendar;

--  System
with System.OS_Interface;

use Ada;

--  GNAT
with GNAT.Calendar.Time_IO;

--  Abstract package Facilities. Provides common data and
--  methods used by all Alog facilities.
package Alog.Facilities is

   type Instance is abstract tagged limited private;
   --  Abstract type facility instance. All facilities in the
   --  Alog framework must implement this type.

   subtype Class is Instance'Class;

   type Handle is access all Class;

   function "=" (Left  : Handle;
                 Right : Handle) return Boolean;
   --  Equal function.

   procedure Set_Name (Facility : in out Instance'Class; Name : in String);
   --  Set facility name.

   function Get_Name (Facility : in Instance'Class) return String;
   --  Get facility name.

   procedure Set_Threshold (Facility : in out Instance'Class;
                            Level    : in Log_Level);
   --  Set facility log level treshold.

   function Get_Threshold (Facility : in Instance'Class) return Log_Level;
   --  Get facility log level treshold.

   function Get_Timestamp (Facility : in Instance'Class) return String;
   --  Creates a timestamp and returns it as String.

   function Get_Pid (Facility : in Instance'Class) return Integer;
   --  Get PID of process logging the message.

   procedure Write_Message (Facility : in Instance;
                            Level    : in Log_Level;
                            Msg      : in String) is abstract;
   --  Write message with specified log level.

   procedure Teardown (Facility : in out Instance) is abstract;
   --  Each facility must provide a Teardown-procedure. These procedures
   --  are called by Logger instances when detaching Facilities or when
   --  the logger object gets out of scope.


   package BS_Path is new Generic_Bounded_Length (Max_Path_Length);
   use BS_Path;
   --  Bounded string with length Max_Path_Length. Used in methods
   --  which involve filesystem operations.

private

   type Instance is abstract tagged
   limited record
      Name      : Unbounded_String :=
        To_Unbounded_String (Ada.Command_Line.Command_Name);
      --  Facility Name. Defaults to command-name (first argument).
      --  If multiple facilities are used, names must be set differently.

      Threshold : Log_Level := DEBU;
      --  Facility default threshold.

      Timestamp_Format : String (1 .. 14) := "%d. %b. %Y %T ";
      --  Default timestamp format to use in this facility.
   end record;

end Alog.Facilities;
