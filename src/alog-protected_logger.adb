--
--  Copyright (c) 2009,
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

package body Alog.Protected_Logger is

   -------------------------------------------------------------------------

   protected body Instance is

      ----------------------------------------------------------------------

      procedure Attach_Default_Facility is
      begin
         Logsink.Attach_Default_Facility;
      end Attach_Default_Facility;

      ----------------------------------------------------------------------

      procedure Attach_Facility (Facility : Facilities.Handle) is
      begin
         Logsink.Attach_Facility (Facility);
      end Attach_Facility;

      ----------------------------------------------------------------------

      procedure Attach_Transform (Transform : Transforms.Handle) is
      begin
         Logsink.Attach_Transform (Transform);
      end Attach_Transform;

      ----------------------------------------------------------------------

      procedure Clear is
      begin
         Logsink.Clear;
      end Clear;

      ----------------------------------------------------------------------

      procedure Detach_Default_Facility is
      begin
         Logsink.Detach_Default_Facility;
      end Detach_Default_Facility;

      ----------------------------------------------------------------------

      procedure Detach_Facility (Name : String) is
      begin
         Logsink.Detach_Facility (Name);
      end Detach_Facility;

      ----------------------------------------------------------------------

      procedure Detach_Transform (Name : String) is
      begin
         Logsink.Detach_Transform (Name);
      end Detach_Transform;

      ----------------------------------------------------------------------

      function Facility_Count return Natural is
      begin
         return Logsink.Facility_Count;
      end Facility_Count;

      ----------------------------------------------------------------------

      procedure Iterate
        (Process : not null access
           procedure (Facility_Handle : in out Facilities.Handle))
      is
      begin
         Logsink.Iterate (Process);
      end Iterate;

      ----------------------------------------------------------------------

      procedure Iterate
        (Process : not null access
           procedure (Transform_Handle : in out Transforms.Handle))
      is
      begin
         Logsink.Iterate (Process);
      end Iterate;

      ----------------------------------------------------------------------

      procedure Log_Message
        (Level : Log_Level;
         Msg   : String)
      is
      begin
         Logsink.Log_Message (Level, Msg);
      end Log_Message;

      ----------------------------------------------------------------------

      function Transform_Count return Natural is
      begin
         return Logsink.Transform_Count;
      end Transform_Count;

      ----------------------------------------------------------------------

      procedure Update
        (Name    : String;
         Process : not null access
           procedure (Facility_Handle : in out Facilities.Handle))
      is
      begin
         Logsink.Update (Name, Process);
      end Update;

      ----------------------------------------------------------------------

      procedure Update
        (Name    : String;
         Process : not null access
           procedure (Transform_Handle : in out Transforms.Handle))
      is
      begin
         Logsink.Update (Name, Process);
      end Update;

   end Instance;

end Alog.Protected_Logger;
