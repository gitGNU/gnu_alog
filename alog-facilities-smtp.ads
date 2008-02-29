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

--  SMTP-Logging facility. Used to send log-messages to a configurable
--  mailserver. AWS must be installed for this facility to work.
package Alog.Facilities.SMTP is

   type Instance is new Alog.Facilities.Instance with private;
   --  SMTP based logging facility.

   type Handle is access all Instance;

   overriding
   procedure Write_Message (F     : in Instance;
                            Level : in Log_Level := INFO;
                            Msg   : in String);
   --  Implementation of Write_Message.

   overriding
   procedure Teardown (F : in out Instance);
   --  Implementation of Teardown-procedure.

   type Mail_Address is tagged
      record
         Name  : Unbounded_String;
         EMail : Unbounded_String;
      end record;
   --  Holds Sender / Recipient information.

   No_Recipient    : exception;
   --  No recipient specified. Cannot send mail.

   Delivery_Failed : exception;
   --  Mail could not be delivered.

private

   type Instance is limited new Alog.Facilities.Instance with
      record
         Server       : Unbounded_String;
         --  Server to connect when sending log-mails.

         Sender       : Mail_Address :=
           (Name  => To_Unbounded_String ("Alog-Alert"),
            EMail => To_Unbounded_String ("alog@localhost"));
         --  Notification sender address/name.

         Recipient    : Mail_Address;
         --  Recipient for log-mails. Must be specified before
         --  calling Write_Message(), else No_Recipient exception
         --  is thrown.

         Is_Recipient : Boolean := False;
         --  Indicates wheter a recipient is set.

         Subject      : Unbounded_String := To_Unbounded_String
           ("Alog: Log-Message");
         --  Subject of messages from Alog-System
         --  (default: Alog: Log-Message).
      end record;

end Alog.Facilities.SMTP;
