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

with Alog.Log_Request;
with Alog.Facilities.XMPP;

package body Facility_Tests.XMPP is

   use Alog;
   use Alog.Log_Request;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for XMPP Facility");
      T.Add_Test_Routine
        (Routine => Send_No_Sender'Access,
         Name    => "send with no sender");
      T.Add_Test_Routine
        (Routine => Send_No_Recipient'Access,
         Name    => "send with no recipient");
      T.Add_Test_Routine
        (Routine => Send_No_Server'Access,
         Name    => "send with no server");
--        T.Add_Test_Routine
--          (Routine => Send_XMPP_Message'Access,
--           Name    => "send XMPP message");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Send_No_Recipient is
      F : Alog.Facilities.XMPP.Instance;
   begin
      F.Set_Sender (JID      => "alog@localhost",
                    Password => "foobar");

      --  Try to send a log-message with no recipient specified first, should
      --  raise No_Recipient exception.
      F.Process
        (Request => Create
           (Level   => Debug,
            Message => "this should not work"));

      Fail (Message => "exception not thrown");

   exception
      when Alog.Facilities.XMPP.No_Recipient =>
         null;
   end Send_No_Recipient;

   -------------------------------------------------------------------------

   procedure Send_No_Sender is
      F : Alog.Facilities.XMPP.Instance;
   begin
      --  Try to send a log-message with no recipient specified first, should
      --  raise No_Recipient exception.
      F.Process
        (Request => Create
           (Level   => Debug,
            Message => "this should not work"));

      Fail (Message => "exception not thrown");

   exception
      when Alog.Facilities.XMPP.No_Sender =>
         null;
   end Send_No_Sender;

   -------------------------------------------------------------------------

   procedure Send_No_Server is
      F : Alog.Facilities.XMPP.Instance;
   begin
      F.Set_Sender (JID      => "alog@localhost",
                    Password => "foobar");
      F.Set_Recipient (JID   => "recipient@localhost");

      --  Try to send a log-message with no server specified first, should
      --  raise No_Server exception.
      F.Process
        (Request => Create
           (Level   => Debug,
            Message => "this should not work"));

      Fail (Message => "exception not thrown");

   exception
      when Alog.Facilities.XMPP.No_Server =>
         null;
   end Send_No_Server;

   -------------------------------------------------------------------------

   procedure Send_XMPP_Message is
      F : Alog.Facilities.XMPP.Instance;
   begin
      F.Set_Sender (JID      => "alog@localhost",
                    Password => "foobar");
      F.Set_Recipient (JID   => "recipient@localhost");
      F.Set_Server (Name     => "localhost");

      F.Process
        (Request => Create
           (Level   => Debug,
            Message => "This is a test message from Alog!"));

      Fail (Message => "not yet implemented");

   exception
      when Alog.Facilities.XMPP.Delivery_Failed =>
         Fail (Message => "could not deliver msg");
   end Send_XMPP_Message;

end Facility_Tests.XMPP;
