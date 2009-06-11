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

with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;

with Alog.Transforms;

--  Abstract package Facilities. Provides common data and methods used by all
--  Alog facilities.
package Alog.Facilities is

   use Ada.Strings.Bounded;
   use Ada.Strings.Unbounded;

   type Instance is abstract tagged limited private;
   --  Abstract type facility instance. All facilities in the Alog framework
   --  must implement this type.

   subtype Class is Instance'Class;

   type Handle is access all Class;

   function "=" (Left  : Handle;
                 Right : Handle) return Boolean;
   --  Equal function.

   procedure Set_Name (Facility : in out Class;
                       Name     :        String);
   --  Set facility name.

   function Get_Name (Facility : Class) return String;
   --  Get facility name.

   procedure Set_Threshold (Facility : in out Class;
                            Level    :        Log_Level);
   --  Set facility log level treshold.

   function Get_Threshold (Facility : Class) return Log_Level;
   --  Get facility log level treshold.

   function Get_Timestamp (Facility : Class) return String;
   --  Creates a timestamp and returns it as String.

   procedure Write_Message (Facility : Instance;
                            Level    : Log_Level;
                            Msg      : String) is abstract;
   --  Write message with specified log level.

   procedure Toggle_Write_Timestamp (Facility : in out Class;
                                     State    :        Boolean);
   --  Enable/disable whether a timestamp is written for log messages.

   function Is_Write_Timestamp (Facility : Class) return Boolean;
   --  Returns the current value of Write_Timestamp.

   procedure Toggle_Write_Loglevel (Facility : in out Class;
                                    State    :        Boolean);
   --  Enable/disable whether the loglevel is written for log messages.

   function Is_Write_Loglevel (Facility : Class) return Boolean;
   --  Returns the current value of Write_Loglevel.

   procedure Setup (Facility : in out Instance) is abstract;
   --  Each facility must provide a Setup-procedure. These procedures are called
   --  by Logger instances when attaching Facilities. All needed operations
   --  prior to writing log messages should be done here.

   procedure Teardown (Facility : in out Instance) is abstract;
   --  Each facility must provide a Teardown-procedure. These procedures are
   --  called by Logger instances when detaching Facilities or when the logger
   --  object gets out of scope.

   procedure Add_Transform (Facility  : in out Class;
                            Transform :        Transforms.Handle);
   --  Adds a Transform to the facility's transform list.

   procedure Remove_Transform (Facility : in out Class;
                               Name     :        String);
   --  Removes a transform with name 'Name' from the facility's transform list.
   --  If the transform is not found Transform_Not_Found exception is raised.

   function Transform_Count (Facility : Class) return Natural;
   --  Returns the number of transforms in the facility's transform list.

   function Get_Transform
     (Facility : Class;
      Name     : String)
      return Alog.Transforms.Handle;
   --  Return a transform specified by the string 'Name'.

   procedure Iterate
     (Facility : Class;
      Process  : not null access procedure (Transform : Transforms.Handle));
   --  Call 'Process' for all attached transforms.

   package BS_Path is new Generic_Bounded_Length (Max_Path_Length);
   use BS_Path;
   --  Bounded string with length Max_Path_Length. Used in methods which
   --  involve filesystem operations.

   Transform_Not_Found       : exception;
   --  Will be raised if a requested transform is not found.
   Transform_Already_Present : exception;
   --  Will be raised if a transform is already present.

private

   use type Alog.Transforms.Handle;

   package Transform_Map_Package is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => Transforms.Handle);
   --  Transforms list.

   type Instance is abstract tagged limited record
      Name             : Unbounded_String :=
        To_Unbounded_String (Ada.Command_Line.Command_Name);
      --  Facility Name. Defaults to command-name (first argument). If multiple
      --  facilities are used, names must be set differently.

      Threshold        : Log_Level := DEBU;
      --  Facility default threshold.

      Timestamp_Format : String (1 .. 11) := "%b %d %Y %T";
      --  Default timestamp format to use in this facility.

      Write_Timestamp  : Boolean := True;
      --  If True, a timestamp is written with the log message. Default is True.

      Write_Loglevel   : Boolean := False;
      --  If True, the loglevel associated with the log message is written.
      --  Default is False.

      Transforms       : Transform_Map_Package.Map;
      --  List of transforms.
   end record;

end Alog.Facilities;
