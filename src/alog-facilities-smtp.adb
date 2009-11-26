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

with AWS.SMTP.Client;

package body Alog.Facilities.SMTP is

   -------------------------------------------------------------------------

   function Format_Message
     (Facility : Instance;
      Level    : Log_Level;
      Msg      : String)
      return String
   is
      Message : constant String :=
      --  Header
        Facility.Get_Header
      --  Log-Level
        & "( " & Log_Level'Image (Level) & " ) : "
      --  Log-Message
        & Msg & EOL & EOL
      --  Footer
        & "Generated: " & Facility.Get_Timestamp
        & "by " & Facility.Get_Name;
   begin
      return Message;
   end Format_Message;

   -------------------------------------------------------------------------

   function Get_Header (Facility : Instance) return String is
   begin
      return To_String (Facility.Header);
   end Get_Header;

   -------------------------------------------------------------------------

   procedure Set_Header
     (Facility : in out Instance;
      Header   :        String)
   is
   begin
      Facility.Header := To_Unbounded_String (Header);
   end Set_Header;

   -------------------------------------------------------------------------

   procedure Set_Recipient
     (Facility : in out Instance;
      Name     :        String;
      EMail    :        String)
   is
   begin
      Facility.Recipient    := (Name  => To_Unbounded_String (Name),
                                EMail => To_Unbounded_String (EMail));
      Facility.Is_Recipient := True;
   end Set_Recipient;

   -------------------------------------------------------------------------

   procedure Set_Server
     (Facility : in out Instance;
      Name     :        String)
   is
   begin
      Facility.Server    := To_Unbounded_String (Name);
      Facility.Is_Server := True;
   end Set_Server;

   -------------------------------------------------------------------------

   procedure Write
     (Facility : Instance;
      Level    : Log_Level := Info;
      Msg      : String)
   is
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

      declare
         Status      : AWS.SMTP.Status;
         SMTP_Server : AWS.SMTP.Receiver;
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
            Message => Facility.Format_Message (Level => Level,
                                                Msg   => Msg),
            Status  => Status);

         --  Raise Delivery_Failure exception if SMTP-Status is not O.K.
         if not AWS.SMTP.Is_Ok (Status) then
            raise Delivery_Failed with AWS.SMTP.Status_Message (Status);
         end if;
      end;
   end Write;

end Alog.Facilities.SMTP;
