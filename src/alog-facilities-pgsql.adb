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

package body Alog.Facilities.Pgsql is

   -------------------------------------------------------------------------

   procedure Close_Connection (Facility : in out Instance) is
   begin
      Facility.Log_Connection.Reset;
   end Close_Connection;

   -------------------------------------------------------------------------

   function Get_Credentials (Facility : Instance) return String is
   begin
      return Facility.Log_Connection.User;
   end Get_Credentials;

   -------------------------------------------------------------------------

   function Get_DB_Name (Facility : Instance) return String is
   begin
      return Facility.Log_Connection.DB_Name;
   end Get_DB_Name;

   -------------------------------------------------------------------------

   function Get_Host_Name (Facility : Instance) return String is
   begin
      return Facility.Log_Connection.Host_Name;
   end Get_Host_Name;

   -------------------------------------------------------------------------

   function Get_Host_Port (Facility : Instance) return Natural is
      Port : constant Natural := Facility.Log_Connection.Port;
   begin
      return Port;
   end Get_Host_Port;

   -------------------------------------------------------------------------

   function Get_Level_Column_Name (Facility : Instance) return String is
   begin
      return To_String (Facility.Log_Table.Level_Column);
   end Get_Level_Column_Name;

   -------------------------------------------------------------------------

   function Get_Message_Column_Name (Facility : Instance) return String is
   begin
      return To_String (Facility.Log_Table.Message_Column);
   end Get_Message_Column_Name;

   -------------------------------------------------------------------------

   function Get_Table_Name (Facility : Instance) return String is
   begin
      return To_String (Facility.Log_Table.Name);
   end Get_Table_Name;

   -------------------------------------------------------------------------

   function Get_Timestamp_Column_Name (Facility : Instance) return String is
   begin
      return To_String (Facility.Log_Table.Timestamp_Column);
   end Get_Timestamp_Column_Name;

   -------------------------------------------------------------------------

   function Is_SQL_Trace (Facility : Instance) return Boolean is
   begin
      return Facility.Log_Connection.Is_Trace;
   end Is_SQL_Trace;

   -------------------------------------------------------------------------

   procedure Set_Credentials
     (Facility : in out Instance;
      Username :        String;
      Password :        String)
   is
   begin
      Facility.Log_Connection.Set_User_Password
        (User_Name     => Username,
         User_Password => Password);
   end Set_Credentials;

   -------------------------------------------------------------------------

   procedure Set_DB_Name (Facility : in out Instance; DB_Name : String) is
   begin
      Facility.Log_Connection.Set_DB_Name (DB_Name => DB_Name);
   end Set_DB_Name;

   -------------------------------------------------------------------------

   procedure Set_Host_Address
     (Facility : in out Instance;
      Address  :        String)
   is
   begin
      Facility.Log_Connection.Set_Host_Address (Address);
   end Set_Host_Address;

   -------------------------------------------------------------------------

   procedure Set_Host_Name (Facility : in out Instance; Hostname : String) is
   begin
      Facility.Log_Connection.Set_Host_Name (Hostname);
   end Set_Host_Name;

   -------------------------------------------------------------------------

   procedure Set_Host_Port (Facility : in out Instance; Port : Natural) is
   begin
      Facility.Log_Connection.Set_Port (Port);
   end Set_Host_Port;

   -------------------------------------------------------------------------

   procedure Set_Level_Column_Name
     (Facility    : in out Instance;
      Column_Name :        String)
   is
   begin
      Facility.Log_Table.Level_Column :=
        To_Unbounded_String (Column_Name);
   end Set_Level_Column_Name;

   -------------------------------------------------------------------------

   procedure Set_Message_Column_Name
     (Facility    : in out Instance;
      Column_Name :        String)
   is
   begin
      Facility.Log_Table.Message_Column :=
        To_Unbounded_String (Column_Name);
   end Set_Message_Column_Name;

   -------------------------------------------------------------------------

   procedure Set_SQL_Trace
     (Facility : in out Instance;
      Filename :        String;
      Mode     :        APQ.Trace_Mode_Type)
   is
   begin
      Facility.Trace_Filename := To_Unbounded_String (Filename);
      Facility.Trace_Mode := Mode;
   end Set_SQL_Trace;

   -------------------------------------------------------------------------

   procedure Set_Table_Name
     (Facility   : in out Instance;
      Table_Name :        String)
   is
   begin
      Facility.Log_Table.Name := To_Unbounded_String (Table_Name);
   end Set_Table_Name;

   -------------------------------------------------------------------------

   procedure Set_Timestamp_Column_Name
     (Facility    : in out Instance;
      Column_Name :        String)
   is
   begin
      Facility.Log_Table.Timestamp_Column :=
        To_Unbounded_String (Column_Name);
   end Set_Timestamp_Column_Name;

   -------------------------------------------------------------------------

   procedure Setup (Facility : in out Instance) is
   begin
      Facility.Log_Connection.Set_Trace (False);
   end Setup;

   -------------------------------------------------------------------------

   procedure Teardown (Facility : in out Instance) is
   begin
      --  Close db connection if still open.
      Facility.Close_Connection;
   end Teardown;

   -------------------------------------------------------------------------

   procedure Toggle_SQL_Trace
     (Facility : in out Instance;
      Set      :        Boolean)
   is
   begin
      Facility.Log_Connection.Set_Trace (Set);
   end Toggle_SQL_Trace;

   -------------------------------------------------------------------------

   procedure Write_Message
     (Facility : Instance;
      Level    : Log_Level := INFO;
      Msg      : String)
   is
      use APQ.PostgreSQL.Client;

      C : Connection_Type;
      Q : Query_Type;
   begin
      --  Clone connection since Facility is an "in" parameter
      C.Connect (Same_As => Facility.Log_Connection);

      --  Open SQL trace if enabled
      if Facility.Is_SQL_Trace then
         C.Open_DB_Trace (Filename => To_String (Facility.Trace_Filename),
                          Mode     => Facility.Trace_Mode);
      end if;

      Q.Prepare (SQL => "INSERT INTO ");

      Q.Append (SQL   => Facility.Get_Table_Name,
                After => " (");

      Q.Append (SQL   => Facility.Get_Level_Column_Name,
                After => ", ");

      Q.Append (SQL   => Facility.Get_Timestamp_Column_Name,
                After => ", ");

      Q.Append (SQL   => Facility.Get_Message_Column_Name,
                After => ") ");
      Q.Append (SQL   => "VALUES (");

      Q.Append (SQL   => "'" & Log_Level'Image (Level) & "'",
                After => ", ");

      Q.Append (SQL   => "now()",
                After => ", ");

      Q.Append (SQL   => "'" & Msg &"'",
                After => ");");

      Execute (Query      => Q,
               Connection => C);

      --  Close SQL trace if enabled
      if Facility.Is_SQL_Trace then
         C.Close_DB_Trace;
      end if;

      C.Disconnect;
   end Write_Message;

end Alog.Facilities.Pgsql;
