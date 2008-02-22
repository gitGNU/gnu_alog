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

--  Alog: Ada Logging Components. Provides easy to use API for
--  different logging facilities.
package Alog is

   type Log_Level is (DEBUG, INFO, NOTICE, WARNING,
                      ERROR, CRITICAL, ALERT, EMERGENCY);
   --  Provided log levels. These log levels can be used
   --  to categorize log messages.

   Max_Facility_Name_Length : constant Natural := 8;
   --  Maximal length of facility names.

   Max_Path_Length : constant Natural := 80;
   --  Maximal length of path names.

   Max_Facilities : constant Natural := 5;
   --  Maximum number of facilities a single logger instance
   --  can manage.

private
   Version : constant Float := 0.1;
   --  Alog library version.

end Alog;
