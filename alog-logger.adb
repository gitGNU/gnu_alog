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

   function Facility_Count (L : in Alog.Logger.Instance) return Natural is
   begin
      return L.F_Index;
   end Facility_Count;

end Alog.Logger;
