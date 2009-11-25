--
--  Copyright (c) 2008-2009,
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

with Ahven; use Ahven;

with Alog.Facilities.Pgsql;

package body Facility_Tests.PGSQL is

   use Alog;
   use Alog.Facilities;

   -------------------------------------------------------------------------

   procedure Disable_Write_Loglevel is
      F     : Alog.Facilities.Pgsql.Instance;
      State : constant Boolean := False;
   begin
      F.Toggle_Write_Loglevel (State => State);
      Assert (Condition => (F.Is_Write_Loglevel = State),
              Message   => "unable to set to " & Boolean'Image (State));
      F.Toggle_Write_Loglevel (State => not State);
      Assert (Condition => (F.Is_Write_Loglevel = not State),
              Message   => "unable to set to " & Boolean'Image (not State));
   end Disable_Write_Loglevel;

   -------------------------------------------------------------------------

   procedure Disable_Write_Timestamp is
      F     : Alog.Facilities.Pgsql.Instance;
      State : constant Boolean := False;
   begin
      F.Toggle_Write_Timestamp (State => State);
      Assert (Condition => (F.Is_Write_Timestamp = State),
              Message   => "unable to set to " & Boolean'Image (State));
      F.Toggle_Write_Timestamp (State => not State);
      Assert (Condition => (F.Is_Write_Timestamp = not State),
              Message   => "unable to set to " & Boolean'Image (not State));
   end Disable_Write_Timestamp;

   -------------------------------------------------------------------------

   procedure Enable_SQL_Trace is
      F     : Alog.Facilities.Pgsql.Instance;
      State : constant Boolean := True;
   begin
      F.Toggle_SQL_Trace (Set => State);
      Assert (Condition => (F.Is_SQL_Trace = State),
              Message   => "unable to set to " & Boolean'Image (State));
      F.Toggle_SQL_Trace (Set => not State);
      Assert (Condition => (F.Is_SQL_Trace = not State),
              Message   => "unable to set to " & Boolean'Image (not State));
   end Enable_SQL_Trace;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for Alog Facility PGSQL");
      T.Add_Test_Routine
        (Routine => Set_Host_Name'Access,
         Name    => "set hostname");
      T.Add_Test_Routine
        (Routine => Set_Host_Address'Access,
         Name    => "set host address");
      T.Add_Test_Routine
        (Routine => Set_Host_Port'Access,
         Name    => "set host port");
      T.Add_Test_Routine
        (Routine => Set_DB_Name'Access,
         Name    => "set database name");
      T.Add_Test_Routine
        (Routine => Set_Table_Name'Access,
         Name    => "set table name");
      T.Add_Test_Routine
        (Routine => Set_Level_Column_Name'Access,
         Name    => "set level column name");
      T.Add_Test_Routine
        (Routine => Set_Timestamp_Column_Name'Access,
         Name    => "set timestamp column name");
      T.Add_Test_Routine
        (Routine => Set_Message_Column_Name'Access,
         Name    => "set message column name");
      T.Add_Test_Routine
        (Routine => Set_Credentials'Access,
         Name    => "set credentials");
      T.Add_Test_Routine
        (Routine => Enable_SQL_Trace'Access,
         Name    => "toggle sql trace");
      T.Add_Test_Routine
        (Routine => Disable_Write_Timestamp'Access,
         Name    => "toggle timestamp");
      T.Add_Test_Routine
        (Routine => Disable_Write_Loglevel'Access,
         Name    => "toggle loglevel");
      T.Add_Test_Routine
        (Routine => Write_Message'Access,
         Name    => "log a message to PGSQL database");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Credentials is
      F : Alog.Facilities.Pgsql.Instance;
      Username : constant String := "foo";
      Password : constant String := "bar";
   begin
      F.Set_Credentials (Username => Username,
                         Password => Password);
      Assert (Condition => (F.Get_Credentials = Username),
              Message   => "non matching username");
   end Set_Credentials;

   -------------------------------------------------------------------------

   procedure Set_DB_Name is
      F : Alog.Facilities.Pgsql.Instance;
      DB_Name : constant String := "FOODB";
   begin
      F.Set_DB_Name (DB_Name);
      Assert (Condition => (F.Get_DB_Name = DB_Name),
              Message   => "non matching database name");
   end Set_DB_Name;

   -------------------------------------------------------------------------

   procedure Set_Host_Address is
      F : Alog.Facilities.Pgsql.Instance;
      Host_Address : constant String := "127.0.0.1";
   begin
      F.Set_Host_Address (Host_Address);
   end Set_Host_Address;

   -------------------------------------------------------------------------

   procedure Set_Host_Name is
      F : Alog.Facilities.Pgsql.Instance;
      Hostname : constant String := "foohost";
   begin
      F.Set_Host_Name (Hostname);
      Assert (Condition => (F.Get_Host_Name = Hostname),
              Message   => "non matching hostname");
   end Set_Host_Name;

   -------------------------------------------------------------------------

   procedure Set_Host_Port is
      F : Alog.Facilities.Pgsql.Instance;
      Host_Port : constant Natural := 1024;
   begin
      F.Set_Host_Port (Host_Port);
      Assert (Condition => (F.Get_Host_Port = Host_Port),
              Message   => "non matching host port");
   end Set_Host_Port;

   -------------------------------------------------------------------------

   procedure Set_Level_Column_Name is
      F : Alog.Facilities.Pgsql.Instance;
      Level_Column_Name : constant String := "foocolumn";
   begin
      F.Set_Level_Column_Name (Level_Column_Name);
      Assert (Condition => (F.Get_Level_Column_Name = Level_Column_Name),
              Message   => "non matching level column name");
   end Set_Level_Column_Name;

   -------------------------------------------------------------------------

   procedure Set_Message_Column_Name is
      F : Alog.Facilities.Pgsql.Instance;
      Message_Column_Name : constant String := "foobarcolumn";
   begin
      F.Set_Message_Column_Name (Message_Column_Name);
      Assert (Condition =>
                (F.Get_Message_Column_Name = Message_Column_Name),
              Message   => "non matching message column name");
   end Set_Message_Column_Name;

   -------------------------------------------------------------------------

   procedure Set_Table_Name is
      F : Alog.Facilities.Pgsql.Instance;
      Table_Name : constant String := "footable";
   begin
      F.Set_Table_Name (Table_Name);
      Assert (Condition => (F.Get_Table_Name = Table_Name),
              Message   => "non matching table name");
   end Set_Table_Name;

   -------------------------------------------------------------------------

   procedure Set_Timestamp_Column_Name is
      F : Alog.Facilities.Pgsql.Instance;
      Timestamp_Column_Name : constant String := "barcolumn";
   begin
      F.Set_Timestamp_Column_Name (Timestamp_Column_Name);
      Assert (Condition =>
                (F.Get_Timestamp_Column_Name = Timestamp_Column_Name),
              Message   => "non matching timestamp column name");
   end Set_Timestamp_Column_Name;

   -------------------------------------------------------------------------

   procedure Write_Message is
      F : Alog.Facilities.Pgsql.Instance;
   begin
      F.Toggle_Write_Timestamp (State => True);

      F.Set_DB_Name (DB_Name => "alog");
      F.Set_Credentials (Username => "alog",
                         Password => "foobar");

      --  Setup facility (open db connection, etc)
      F.Setup;

      F.Write_Message (Msg => "Test message");

      F.Teardown;
   exception
      when others =>
         Fail (Message => "could not write msg to database");
   end Write_Message;

end Facility_Tests.PGSQL;
