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

package body Alog.Facilities.Syslog is

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message (F     : in Instance;
                            Level : in Log_Level := INFO;
                            Msg   : in String) is

      procedure C_Syslog (Prio : Natural;
                          Msg  : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, C_Syslog, "syslog");
      Char_Ptr : Interfaces.C.Strings.chars_ptr;
   begin
      if Level <= F.Get_Threshold then
         Char_Ptr := Interfaces.C.Strings.New_String (Str => Msg);
         C_Syslog (Prio => Log_Level'Pos (Level),
                   Msg  => Char_Ptr);

         --  Free message memory.
         Interfaces.C.Strings.Free (Char_Ptr);
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

end Alog.Facilities.Syslog;
