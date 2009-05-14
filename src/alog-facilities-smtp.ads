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

with Ada.Characters.Latin_1;

with AWS;

with GNAT.Sockets;

--  SMTP-Logging facility. Used to send log-messages to a configurable
--  mailserver. AWS must be installed for this facility to work.
package Alog.Facilities.SMTP is

   type Instance is new Alog.Facilities.Instance with private;
   --  SMTP based logging facility.

   type Handle is access all Instance;

   overriding
   procedure Write_Message (Facility : Instance;
                            Level    : Log_Level := INFO;
                            Msg      : String);
   --  Implementation of Write_Message.

   overriding
   procedure Setup (Facility : in out Instance);
   --  Implementation of Setup-procedure.

   overriding
   procedure Teardown (Facility : in out Instance);
   --  Implementation of Teardown-procedure.

   procedure Set_Recipient (Facility : in out Instance;
                            Name     :        String;
                            EMail    :        String);
   --  Set recipient for log-messages. This procedure MUST be called before
   --  subsequent calls to Write_Message().

   procedure Set_Server (Facility : in out Instance;
                         Name     :        String);
   --  Set server for log-messages. This procedure MUST be called before
   --  subsequent calls to Write_Message().

   procedure Set_Header (Facility : in out Instance;
                         Header   :        String);
   --  Set Message-Header of log-messages.

   function Get_Header (Facility : Instance) return String;
   --  Get actual Message-Header of log-messages.

   --  Exceptions.

   No_Recipient    : exception;
   --  No recipient specified. Cannot send mail.

   No_Server       : exception;
   --  No server specified. Cannot send mail.

   Delivery_Failed : exception;
   --  Mail could not be delivered.

private

   function Format_Message (Facility : Instance;
                            Level    : Log_Level;
                            Msg      : String)
                            return String;
   --  Compose a message from Msg, Header, Loglevel, Timestamp, PID.

   EOL : constant Character := Ada.Characters.Latin_1.LF;
   --  EOL used in mail-messages.

   type Mail_Address is tagged record
      Name  : Unbounded_String;
      EMail : Unbounded_String;
   end record;
   --  Holds Sender / Recipient information.

   type Instance is new Alog.Facilities.Instance with record
      Server       : Unbounded_String;
      --  Server to connect when sending log-mails.

      Is_Server    : Boolean := False;
      --  Indicates whether a server is set.

      Recipient    : Mail_Address;
      --  Recipient for log-mails. Must be specified before calling
      --  Write_Message(), else No_Recipient exception is thrown.

      Is_Recipient : Boolean := False;
      --  Indicates whether a recipient is set.

      Sender       : Mail_Address :=
        (Name  => To_Unbounded_String ("alog"),
         EMail => To_Unbounded_String ("alog@" &
           GNAT.Sockets.Host_Name));
      --  Notification sender address/name.

      Subject      : Unbounded_String := To_Unbounded_String
        ("Log-Message");
      --  Subject of messages from Alog-System (default: Alog: Log-Message).

      Header       : Unbounded_String := To_Unbounded_String
        ("This is a message from the alog-logsystem running on '"
         & GNAT.Sockets.Host_Name & "' :" & EOL & EOL);
      --  Message-Header. Can be set by calling Set_Header().
   end record;

end Alog.Facilities.SMTP;
