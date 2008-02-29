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

with Ada.Text_IO;

package body Alog.Facilities.SMTP is

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message (F     : in Instance;
                            Level : in Log_Level := INFO;
                            Msg   : in String) is

      Status      : AWS.SMTP.Status;
      SMTP_Server : AWS.SMTP.Receiver
        := AWS.SMTP.Client.Initialize ("mailx.swiss-it.ch");
   begin
      --  Raise No_Recipient if no recipient has been set
      --  by calling Set_Recipient().
      if not F.Is_Recipient then
         raise No_Recipient;
      end if;

      --  Raise No_Server if no server has been set by calling
      --  Set_Server().
      if not F.Is_Server then
         raise No_Server;
      end if;

      --  Try to send message.
      AWS.SMTP.Client.Send
        (SMTP_Server,
         From    => AWS.SMTP.E_Mail
           (To_String (F.Sender.Name), To_String (F.Sender.EMail)),
         To      => AWS.SMTP.E_Mail
           (To_String (F.Recipient.Name), To_String (F.Recipient.EMail)),
         Subject => To_String (F.Subject),
         Message => Msg,
         Status  => Status);

      --  Raise Delivery_Failure exception if SMTP-Status is not O.K.
      if not AWS.SMTP.Is_Ok (Status) then
         raise Delivery_Failed with AWS.SMTP.Status_Message (Status);
      end if;

   end Write_Message;

   --------------
   -- Teardown --
   --------------

   procedure Teardown (F : in out Instance) is
   begin
      --  Nothing to do for now.
      null;
   end Teardown;

   -------------------
   -- Set_Recipient --
   -------------------

   procedure Set_Recipient (F      : in out Instance;
                            Name   : in     String;
                            EMail  : in     String) is
   begin
      F.Recipient := (Name  => To_Unbounded_String (Name),
                      EMail => To_Unbounded_String (EMail));
      F.Is_Recipient := True;
   end Set_Recipient;

   ----------------
   -- Set_Server --
   ----------------

   procedure Set_Server (F      : in out Instance;
                         Name   : in     String) is
   begin
      F.Server := To_Unbounded_String (Name);
      F.Is_Server := True;
   end Set_Server;

end Alog.Facilities.SMTP;
