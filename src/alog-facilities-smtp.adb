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

package body Alog.Facilities.SMTP is

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message (Facility : in Instance;
                            Level    : in Log_Level := INFO;
                            Msg      : in String) is
   begin
      --  Raise No_Recipient if no recipient has been set
      --  by calling Set_Recipient().
      if not Facility.Is_Recipient then
         raise No_Recipient;
      end if;

      --  Raise No_Server if no server has been set by calling
      --  Set_Server().
      if not Facility.Is_Server then
         raise No_Server;
      end if;

      --  Check threshold first.
      if Level > Facility.Get_Threshold then return; end if;

      declare
         Status      : AWS.SMTP.Status;
         SMTP_Server : AWS.SMTP.Receiver;
         Message     : String := Facility.Get_Header
           & "( " & Log_Level'Image (Level) & " ) : " & Msg
           & EOL & EOL & "Generated: " & Facility.Get_Timestamp;
      begin
         --  Init receiving server.
         SMTP_Server := AWS.SMTP.Client.Initialize
           (To_String (Facility.Server));

         --  Try to send message.
         AWS.SMTP.Client.Send
           (SMTP_Server,
            From    => AWS.SMTP.E_Mail
              (To_String (Facility.Sender.Name),
               To_String (Facility.Sender.EMail)),
            To      => AWS.SMTP.E_Mail
              (To_String (Facility.Recipient.Name),
               To_String (Facility.Recipient.EMail)),
            Subject => To_String (Facility.Subject & " (" &
              Log_Level'Image (Level) & ")"),
            Message => Message,
            Status  => Status);

         --  Raise Delivery_Failure exception if SMTP-Status is not O.K.
         if not AWS.SMTP.Is_Ok (Status) then
            raise Delivery_Failed with AWS.SMTP.Status_Message (Status);
         end if;
      end;
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
   -- Set_Recipient --
   -------------------

   procedure Set_Recipient (Facility : in out Instance;
                            Name     : in     String;
                            EMail    : in     String) is
   begin
      Facility.Recipient    := (Name  => To_Unbounded_String (Name),
                                EMail => To_Unbounded_String (EMail));
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

   ----------------
   -- Set_Header --
   ----------------

   procedure Set_Header (Facility : in out Instance;
                         Header   : in     String) is
   begin
      Facility.Header := To_Unbounded_String (Header);
   end Set_Header;

   ----------------
   -- Get_Header --
   ----------------

   function Get_Header (Facility : Instance) return String is
   begin
      return To_String (Facility.Header);
   end Get_Header;

end Alog.Facilities.SMTP;
