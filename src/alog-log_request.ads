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

with Ada.Task_Identification;
with Ada.Strings.Unbounded;

--  Log request type. Log request objects are used for asynchronous logging and
--  hold all relevant information of a log request.
package Alog.Log_Request is

   type Instance is tagged private;
   --  A log request contains all related information to log asynchronously
   --  (Caller identification, loglevel and message).

   function Create
     (ID      : Ada.Task_Identification.Task_Id;
      Level   : Log_Level := Debug;
      Message : String)
      return Instance;
   --  Create a log request object from the specified parameters.

   function Get_Caller_ID
     (Request : Instance)
      return Ada.Task_Identification.Task_Id;
   --  Return the caller ID of the request object.

   function Get_Log_Level (Request : Instance) return Log_Level;
   --  Return the loglevel of the request object.

   function Get_Message (Request : Instance) return String;
   --  Return the log message of the request object.

private

   type Instance is tagged record
      Caller_ID : Ada.Task_Identification.Task_Id :=
        Ada.Task_Identification.Null_Task_Id;
      Level     : Log_Level                       := Info;
      Message   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Alog.Log_Request;
