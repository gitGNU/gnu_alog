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

package body Alog.Facilities.XMPP is

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message (Facility : in Instance;
                            Level    : in Log_Level := INFO;
                            Msg      : in String) is
      use AWS.Jabber;
      Server : AWS.Jabber.Server;
      Status : AWS.Jabber.Presence_Status;
   begin
      --  Raise exception if no sender has been set.
      if not Facility.Is_Sender then
         raise No_Sender;
      end if;

      --  Raise exception if no recipient has been set.
      if not Facility.Is_Recipient then
         raise No_Recipient;
      end if;

      --  Raise exception if no server has been set.
      if not Facility.Is_Server then
         raise No_Server;
      end if;

      --  Check threshold first.
      if Level > Facility.Get_Threshold then return; end if;

      --  Init xmpp server.
      AWS.Jabber.Connect (Server,
                          To_String (Facility.Server),
                          To_String (Facility.Sender.JID),
                          To_String (Facility.Sender.Password));

      --  Check presence of recipient
      AWS.Jabber.Check_Presence (Server,
                                 To_String (Facility.Recipient),
                                 Status);
      if Status not in Available .. Do_Not_Disturb then
         raise Recipient_Not_Present;
      end if;

      --  Try to send message.
      Send_Message
        (Server,
         JID     => To_String (Facility.Recipient),
         Subject => To_String (Facility.Subject),
         Content => Msg);

      AWS.Jabber.Close (Server);

   exception
      when Error : Server_Error =>
         --  Make sure that connecion to server is closed.
         AWS.Jabber.Close (Server);
         --  Raise Delivery_Failure exception, something went wrong.
         raise Delivery_Failed with Ada.Exceptions.Exception_Message (Error);
   end Write_Message;

   --------------
   -- Teardown --
   --------------

   procedure Teardown (Facility : in out Instance) is
   begin
      --  Nothing to do for now.
      null;
   end Teardown;

   -------------------
   -- Set_Sender --
   -------------------

   procedure Set_Sender (Facility : in out Instance;
                         JID      : in     String;
                         Password : in     String) is
   begin
      Facility.Sender    := (JID      => To_Unbounded_String (JID),
                             Password => To_Unbounded_String (Password));
      Facility.Is_Sender := True;
   end Set_Sender;

   -------------------
   -- Set_Recipient --
   -------------------

   procedure Set_Recipient (Facility : in out Instance;
                            JID      : in     String) is
   begin
      Facility.Recipient    := To_Unbounded_String (JID);
      Facility.Is_Recipient := True;
   end Set_Recipient;

   ----------------
   -- Set_Server --
   ----------------

   procedure Set_Server (Facility : in out Instance;
                         Name     : in     String) is
   begin
      Facility.Server    := To_Unbounded_String (Name);
      Facility.Is_Server := True;
   end Set_Server;

end Alog.Facilities.XMPP;
