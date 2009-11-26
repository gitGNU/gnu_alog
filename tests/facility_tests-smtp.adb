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
with Alog.Facilities.SMTP;

package body Facility_Tests.SMTP is

   use Alog;
   use Alog.Log_Request;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Tests for SMTP Facility");
      T.Add_Test_Routine
        (Routine => Send_No_Recipient'Access,
         Name    => "send with no recipient");
      T.Add_Test_Routine
        (Routine => Send_No_Server'Access,
         Name    => "send with no server");
      T.Add_Test_Routine
        (Routine => Set_Header'Access,
         Name    => "set message header");
--        T.Add_Test_Routine
--          (Routine => Send_Simple_Mail'Access,
--           Name    => "send simple mail");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Send_No_Recipient is
      F : Alog.Facilities.SMTP.Instance;
   begin
      --  Try to send a log-message with no recipient specified first, should
      --  raise No_Recipient exception.
      F.Process
        (Request => Create
           (Level   => Debug,
            Message => "this should not work"));

      Fail (Message => "exception not thrown");

   exception
      when Alog.Facilities.SMTP.No_Recipient =>
         null;
   end Send_No_Recipient;

   -------------------------------------------------------------------------

   procedure Send_No_Server is
      F : Alog.Facilities.SMTP.Instance;
   begin
      F.Set_Recipient (Name  => "Send_No_Server",
                       EMail => "Testcase");
      --  Try to send a log-message with no server specified first, should
      --  raise No_Server exception.
      F.Process
        (Request => Create
           (Level   => Debug,
            Message => "this should not work"));

      Fail (Message => "exception not thrown");

   exception
      when Alog.Facilities.SMTP.No_Server =>
         null;
   end Send_No_Server;

   -------------------------------------------------------------------------

   procedure Send_Simple_Mail is
      F : Alog.Facilities.SMTP.Instance;
   begin
      F.Set_Recipient (Name  => "Facility-Test",
                       EMail => "test@example.ch");
      F.Set_Server (Name => "mta.example.ch");

      F.Process
        (Request => Create
           (Level   => Debug,
            Message => "Testmessage"));

   exception
      when Alog.Facilities.SMTP.Delivery_Failed =>
         Fail (Message => "could not deliver msg");
   end Send_Simple_Mail;

   -------------------------------------------------------------------------

   procedure Set_Header is
      F : Alog.Facilities.SMTP.Instance;
      H : constant String := "Expected Header";
   begin
      F.Set_Header (Header => H);
      Assert (Condition => F.Get_Header = H,
              Message   => "headers not equal");
   end Set_Header;

end Facility_Tests.SMTP;
