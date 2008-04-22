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

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with APQ;
with APQ.PostgreSQL.Client;

use APQ.PostgreSQL.Client;
use Ada;

--  PGSQL facility. Used to log to
--  a Postgresql database
package Alog.Facilities.Pgsql is

   type Instance is new Alog.Facilities.Instance with private;
   --  PGSQL logging facility.

   type Handle is access all Instance;

   overriding
   procedure Write_Message (Facility : in Instance;
                            Level    : in Log_Level := INFO;
                            Msg      : in String);
   --  Implementation of Write_Message.

   overriding
   procedure Setup (Facility : in out Instance);
   --  Implementation of Setup-procedure.

   overriding
   procedure Teardown (Facility : in out Instance);
   --  Implementation of Teardown-procedure.

   procedure Set_Host_Name (Facility : in out Instance; Hostname : String);
   --  Set hostname of database server.

   function Get_Host_Name (Facility : in Instance) return String;
   --  Get hostname of database server.

   procedure Set_Host_Address (Facility : in out Instance; Address : String);
   --  Set ip address of database server.

   procedure Set_Host_Port (Facility : in out Instance; Port : Natural);
   --  Set port of database server.

   function Get_Host_Port (Facility : in Instance) return Natural;
   --  Get port of database server.

   procedure Set_DB_Name (Facility : in out Instance; DB_Name : String);
   --  Set name of database.

   function Get_DB_Name (Facility : in Instance) return String;
   --  Get name of database.

   procedure Set_Table_Name (Facility : in out Instance; Table_Name : String);
   --  Set name of database table.

   function Get_Table_Name (Facility : in Instance) return String;
   --  Get name of database table.

   procedure Set_Level_Column_Name (Facility    : in out Instance;
                                    Column_Name : String);
   --  Set name of log level column.

   function Get_Level_Column_Name (Facility : in Instance) return String;
   --  Get name of log level column.

   procedure Set_Timestamp_Column_Name (Facility    : in out Instance;
                                        Column_Name : String);
   --  Set name of log level column.

   function Get_Timestamp_Column_Name (Facility : in Instance) return String;
   --  Get name of timestamp column.

   procedure Set_Message_Column_Name (Facility    : in out Instance;
                                      Column_Name : String);
   --  Set name of log message column.

   function Get_Message_Column_Name (Facility : in Instance) return String;
   --  Get name of log message column.

   procedure Set_Credentials (Facility : in out Instance;
                              Username : String;
                              Password : String);
   --  Set credentials for the database connection.

   function Get_Credentials (Facility : in Instance) return String;
   --  Get credentials of database connection.
   --  Only the username is returned

   procedure Close_Connection (Facility : in out Instance);
   --  Close open database connection.

   procedure Toggle_Write_Timestamp (Facility : in out Instance;
                                     Set      : in Boolean);
   --  Enable/disable whether a timestamp is written for log messages.

   function Is_Write_Timestamp (Facility : in Instance) return Boolean;
   --  Returns the current value of Write_Timestamp.

   procedure Toggle_Write_Loglevel (Facility : in out Instance;
                                    Set      : in Boolean);
   --  Enable/disable whether the loglevel is written for log messages.

   function Is_Write_Loglevel (Facility : in Instance) return Boolean;
   --  Returns the current value of Write_Loglevel.

private

   type Log_SQL_Table is tagged
      record
         Name             : Unbounded_String :=
                              To_Unbounded_String ("alog");
         Level_Column     : Unbounded_String :=
                              To_Unbounded_String ("level");
         Timestamp_Column : Unbounded_String :=
                              To_Unbounded_String ("timestamp");
         Message_Column   : Unbounded_String :=
                              To_Unbounded_String ("message");
      end record;
   --  Holds Table/Column name information.

   type Instance is limited new Alog.Facilities.Instance with
      record
         Log_Connection  : APQ.PostgreSQL.Client.Connection_Type;
         --  Database connection used for logging.

         Log_Table       : Log_SQL_Table;
         --  Table to insert messages

         Write_Timestamp : Boolean := True;
         --  If True, a timestamp is written with the log message.
         --  Default is True.

         Write_Loglevel  : Boolean := True;
         --  If True, the loglevel associated with the log message is
         --  written. Default is True.
      end record;

end Alog.Facilities.Pgsql;
