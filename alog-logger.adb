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
--

package body Alog.Logger is

   ----------------------
   --  Attach_Facility --
   ----------------------

   procedure Attach_Facility (L : in out Alog.Logger.Instance;
                              F : in     Alog.Facilities.Handle) is
   begin
      L.F_Array (L.F_Index) := F;
      L.F_Index := L.F_Index + 1;
   end Attach_Facility;

   ---------------------
   --  Facility_Count --
   ---------------------

   function Facility_Count (L : in Instance) return Natural is
   begin
      return L.F_Index;
   end Facility_Count;

   procedure Finalize (L : in out Instance) is
      Counter : Natural := 0;
   begin
      while Counter < L.F_Index loop
         L.F_Array (Counter).Teardown;
         Free (L.F_Array (Counter));
         Counter := Counter + 1;
      end loop;
   end Finalize;

   ------------
   --  Clear --
   ------------

   procedure Clear (L : in out Instance) is
   begin
      L.Finalize;
      L.F_Index := 0;
   end Clear;

   ------------------
   --  Log_Message --
   ------------------

   procedure Log_Message (L     : in Instance;
                          Level : in Log_Level;
                          Msg   : in String) is
      --  TODO: write iterator.
      Counter : Natural := 0;
   begin
      while Counter < L.F_Index loop
         L.F_Array (Counter).Write_Message (Level => Level,
                                            Msg   => Msg);
         Counter := Counter + 1;
      end loop;
   end Log_Message;

end Alog.Logger;
