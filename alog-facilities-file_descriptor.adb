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

package body Alog.Facilities.File_Descriptor is

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message (F     : in Instance;
                            Msg   : in String;
                            Level : Log_Level := INFO)
   is
      use GNAT.Calendar.Time_IO;
      Logfile   : Text_IO.File_Type renames F.Log_File_Ptr.all;
      Timestamp : String := Image
        (Date    => Calendar.Clock,
         Picture => Picture_String (F.Timestamp_Format));
   begin
      if Level >= F.Get_Threshold then
         if F.Is_Write_Timestamp then
            Text_IO.Put (File  => Logfile,
                         Item  => Timestamp);
         end if;
         if F.Is_Write_Loglevel then
            Text_IO.Put (File  => Logfile,
                         Item  => Log_Level'Image (Level));
         end if;
         Text_IO.Put (File  => Logfile,
                      Item  => " " & Msg);

         Text_IO.New_Line (File => Logfile);
      end if;
   end Write_Message;

   --------------
   -- Teardown --
   --------------

   procedure Teardown (F : in out Instance) is
   begin
      --  Close logfile if still open.
      F.Close_Logfile;
   end Teardown;

   -----------------
   -- Set_Logfile --
   -----------------

   procedure Set_Logfile (F : in out Instance; Path : String) is
      use Ada.IO_Exceptions;
   begin
      --  Somehow it's not possible to use Create() with Append_File,
      --  the file always gets truncated.
      if not Text_IO.Is_Open (File => F.Log_File) then
         Text_IO.Open (File => F.Log_File,
                       Name => Path,
                       Mode => Text_IO.Append_File);
      end if;

      F.Log_File_Name := To_Bounded_String (Path);

      --  Set logfile name and pointer to newly created file.
      --  Unrestricted_Access is needed here since we use a pointer
      --  which is defined externaly in the Text_IO library.
      F.Log_File_Ptr  := F.Log_File'Unrestricted_Access;

      F.Write_Message (Level => INFO,
                       Msg   => "** Alog: new logging session initialized.");
   exception
      when Name_Error =>
         --  Create file and re-call Set_Logfile.
         Text_IO.Create (File => F.Log_File,
                         Mode => Text_IO.Append_File,
                         Name => Path);
         F.Set_Logfile (Path => Path);

   end Set_Logfile;

   -----------------
   -- Get_Logfile --
   -----------------

   function Get_Logfile (F : in Instance) return Text_IO.File_Type is
   begin
      return F.Log_File_Ptr.all;
   end Get_Logfile;

   -------------------
   -- Close_Logfile --
   -------------------

   procedure Close_Logfile (F      : in out Instance;
                            Remove : in Boolean := False) is
      use Ada.Text_IO;
   begin
      if F.Log_File_Ptr /= Standard_Output
        and Is_Open (File => F.Log_File) then
         if Remove then
            --  Close and delete.
            Delete (File => F.Log_File);
         else
            --   Close only.
            Close (File => F.Log_File);
         end if;
      end if;
   end Close_Logfile;

   ----------------------------
   -- Toggle_Write_Timestamp --
   ----------------------------

   procedure Toggle_Write_Timestamp (F   : in out Instance;
                                     Set : in Boolean) is
   begin
      F.Write_Timestamp := Set;
   end Toggle_Write_Timestamp;

   ------------------------
   -- Is_Write_Timestamp --
   ------------------------

   function Is_Write_Timestamp (F : in Instance) return Boolean is
   begin
      return F.Write_Timestamp;
   end Is_Write_Timestamp;


   ---------------------------
   -- Toggle_Write_Loglevel --
   ---------------------------

   procedure Toggle_Write_Loglevel (F   : in out Instance;
                                    Set : in Boolean) is
   begin
      F.Write_Loglevel := Set;
   end Toggle_Write_Loglevel;

   -----------------------
   -- Is_Write_Loglevel --
   -----------------------

   function Is_Write_Loglevel (F : in Instance) return Boolean is
   begin
      return F.Write_Loglevel;
   end Is_Write_Loglevel;

end Alog.Facilities.File_Descriptor;
