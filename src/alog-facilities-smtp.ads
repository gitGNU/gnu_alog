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

with AWS;
with AWS.SMTP.Client;

--  SMTP-Logging facility. Used to send log-messages to a configurable
--  mailserver. AWS must be installed for this facility to work.
package Alog.Facilities.SMTP is

   type Instance is new Alog.Facilities.Instance with private;
   --  SMTP based logging facility.

   type Handle is access all Instance;

   overriding
   procedure Write_Message (Facility : in Instance;
                            Level    : in Log_Level := INFO;
                            Msg      : in String);
   --  Implementation of Write_Message.

   overriding
   procedure Teardown (Facility : in out Instance);
   --  Implementation of Teardown-procedure.


   procedure Set_Recipient (Facility : in out Instance;
                            Name     : in     String;
                            EMail    : in     String);
   --  Set recipient for log-messages. This procedure MUST be called
   --  before subsequent calls to Write_Message().

   procedure Set_Server (Facility : in out Instance;
                         Name     : in     String);
   --  Set server for log-messages. This procedure MUST be called
   --  before subsequent calls to Write_Message().

   No_Recipient    : exception;
   --  No recipient specified. Cannot send mail.

   No_Server       : exception;
   --  No server specified. Cannot send mail.

   Delivery_Failed : exception;
   --  Mail could not be delivered.

private

   type Mail_Address is tagged
      record
         Name  : Unbounded_String;
         EMail : Unbounded_String;
      end record;
   --  Holds Sender / Recipient information.

   type Instance is limited new Alog.Facilities.Instance with
      record
         Server       : Unbounded_String;
         --  Server to connect when sending log-mails.

         Is_Server    : Boolean := False;
         --  Indicates whether a server is set.

         Recipient    : Mail_Address;
         --  Recipient for log-mails. Must be specified before
         --  calling Write_Message(), else No_Recipient exception
         --  is thrown.

         Is_Recipient : Boolean := False;
         --  Indicates whether a recipient is set.

         Sender       : Mail_Address :=
           (Name  => To_Unbounded_String ("Alog-Alert"),
            EMail => To_Unbounded_String ("alog@localhost"));
         --  Notification sender address/name.

         Subject      : Unbounded_String := To_Unbounded_String
           ("Alog: Log-Message");
         --  Subject of messages from Alog-System
         --  (default: Alog: Log-Message).
      end record;

end Alog.Facilities.SMTP;
