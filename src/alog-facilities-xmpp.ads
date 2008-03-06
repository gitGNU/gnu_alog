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
with AWS.Jabber;

with Ada.Exceptions;

--  XMPP-Logging (jabber) facility.
--  Send log-messages to a configured Jabber ID via a given jabber server.
--  AWS must be installed for this facility to work.
package Alog.Facilities.XMPP is

   type Instance is new Alog.Facilities.Instance with private;
   --  XMPP based logging facility.

   type Handle is access all Instance;

   overriding
   procedure Write_Message (Facility : in Instance;
                            Level    : in Log_Level := INFO;
                            Msg      : in String);
   --  Implementation of Write_Message.

   overriding
   procedure Teardown (Facility : in out Instance);
   --  Implementation of Teardown-procedure.

   procedure Set_Sender (Facility : in out Instance;
                         JID      : in     String;
                         Password : in     String);
   --  Set sender for log messages. This procedure MUST be called
   --  before subsequent calls to Write_Message().

   procedure Set_Recipient (Facility : in out Instance;
                            JID      : in     String);
   --  Set recipient for log-messages. This procedure MUST be called
   --  before subsequent calls to Write_Message().

   procedure Set_Server (Facility : in out Instance;
                         Name     : in     String);
   --  Set server for log-messages. This procedure MUST be called
   --  before subsequent calls to Write_Message().

   No_Sender                  : exception;
   --  No sender ID specified. Cannot send message.

   No_Recipient               : exception;
   --  No recipient specified. Cannot send message.

   No_Server                  : exception;
   --  No server specified. Cannot send message.

   Recipient_Not_Present      : exception;
   --  Recipient can not be reached through specified server.

   Delivery_Failed            : exception;
   --  Message could not be delivered.

private

   type Sender_Account is tagged
      record
         JID      : Unbounded_String;
         Password : Unbounded_String;
      end record;
   --  Holds sender information.

   type Instance is limited new Alog.Facilities.Instance with
      record
         Sender       : Sender_Account :=
           (JID  => To_Unbounded_String ("alog@localhost"),
            Password => To_Unbounded_String (""));
         --  Notification sender JID/password.

         Is_Sender    : Boolean := False;
         --  Indicates whether sender id is set.

         Server       : Unbounded_String;
         --  Server to connect to.

         Is_Server    : Boolean := False;
         --  Indicates whether a server is set.

         Recipient    : Unbounded_String;
         --  Recipient for log-mails. Must be specified before
         --  calling Write_Message(), else No_Recipient exception
         --  is thrown.

         Is_Recipient : Boolean := False;
         --  Indicates whether a recipient is set.

         Subject      : Unbounded_String := To_Unbounded_String
           ("Alog: Log-Message");
         --  Subject of messages from Alog-System
         --  (default: Alog: Log-Message).
      end record;

end Alog.Facilities.XMPP;
