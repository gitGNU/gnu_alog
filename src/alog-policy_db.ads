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

with Alog.Maps;

--  Logging policy database package. This DB stores logging policies. Policies
--  in the database are used inside the framework for logging decisions.
package Alog.Policy_DB is

   Alog_Default_Level : constant Log_Level := Debug;
   --  Framework default loglevel.

   procedure Set_Default_Loglevel (Level : Log_Level);
   --  Set given loglevel as default loglevel.

   function Get_Default_Loglevel return Log_Level;
   --  Return current default loglevel.

   procedure Set_Src_Loglevel
     (Source : String;
      Level  : Log_Level);
   --  Set given loglevel for specified source string. If source is already
   --  present the loglevel is updated. Source strings are case-sensitive.
   --
   --  Use wildcards to specify a loglevel for a range of log-sources. Source
   --  hierarchies are separated by dots, the wildcard is '*'. The following
   --  example sets a Debug loglevel for all log-sources in Foo.Bar
   --  (including Foo.Bar).
   --
   --  Example:
   --     Foo.Bar.* = Debug
   --
   --  Direct matches take precedence over wildcard matches. In the following
   --  example the loglevel for source 'Foo.Bar' is explicitly set to Info.
   --
   --  Example:
   --     Foo.Bar   = Info
   --     Foo.Bar.* = Debug

   procedure Set_Src_Loglevel (Sources : Maps.Wildcard_Level_Map);
   --  Apply source loglevels stored in map.

   function Get_Src_Loglevel (Source : String) return Log_Level;
   --  Return loglevel for given source string. Raises No_Source_Loglevel
   --  exception if no entry for given source is found (exact match only, no
   --  wildcard lookup).

   procedure Reset;
   --  Reset the logging policy database to the initial state.

   function Accept_Src
     (Source : String := "";
      Level  : Log_Level)
      return Boolean;
   --  Returns True if the given loglevel is accepted for a source string. If
   --  no source is given, the loglevel is verified against the default
   --  loglevel.

   No_Source_Loglevel : exception;
   --  Will be raised if loglevel is not found for a requested source.

end Alog.Policy_DB;
