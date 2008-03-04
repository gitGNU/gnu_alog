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

   procedure Attach_Facility (Logger   : in out Alog.Logger.Instance;
                              Facility : in     Alog.Facilities.Handle) is
   begin
      Logger.F_Stack.Insert (New_Item => Facility);
   end Attach_Facility;

   ----------------------
   --  Detach_Facility --
   ----------------------

   procedure Detach_Facility (Logger   : in out Instance;
                              Facility : in Alog.Facilities.Handle) is
   begin
      null;
   end Detach_Facility;

   ---------------------
   --  Facility_Count --
   ---------------------

   function Facility_Count (Logger : in Instance) return Natural is
   begin
      return Natural (Logger.F_Stack.Length);
   end Facility_Count;

   procedure Finalize (Logger : in out Instance) is
      use Facilities_Stack_Package;

      --  Forward specs.
      procedure Free_Facility (Position : Cursor);

      procedure Free_Facility (Position : Cursor) is
         Facility_Handle : Alog.Facilities.Handle := Element (Position);
      begin
         --  Cleanup this facility.
         Facility_Handle.Teardown;
         Free (Facility_Handle);
      end Free_Facility;
   begin
      --  Iterate over all attached facilities.
      Iterate (Container => Logger.F_Stack,
               Process => Free_Facility'Access);
      Logger.F_Stack.Clear;
   end Finalize;

   ------------
   --  Clear --
   ------------

   procedure Clear (L : in out Instance) is
   begin
      L.Finalize;
   end Clear;

   ------------------
   --  Log_Message --
   ------------------

   procedure Log_Message (Logger : in Instance;
                          Level  : in Log_Level;
                          Msg    : in String) is
      use Facilities_Stack_Package;

      Position : Cursor := Logger.F_Stack.First;
      Item     : Alog.Facilities.Handle;
   begin
      while Has_Element (Position) loop
         Item := Element (Position);
         Item.Write_Message (Level => Level,
                             Msg   => Msg);
         Next (Position);
      end loop;
   end Log_Message;

   --------------------
   --  Hash_Facility --
   --------------------

   function Hash_Facility (Element : Alog.Facilities.Handle)
                           return Hash_Type is
   begin
      return Ada.Strings.Unbounded.Hash
        (Key => Ada.Strings.Unbounded.To_Unbounded_String (Element.Get_Name));
   end Hash_Facility;

end Alog.Logger;
