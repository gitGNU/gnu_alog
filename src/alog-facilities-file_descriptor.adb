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

   procedure Write_Message (Facility : in Instance;
                            Level    : Log_Level := INFO;
                            Msg      : in String)
   is
      Logfile   : Text_IO.File_Type renames Facility.Log_File_Ptr.all;
      Timestamp : String := Facility.Get_Timestamp;
   begin
      if Level <= Facility.Get_Threshold then
         if Facility.Is_Write_Timestamp then
            Text_IO.Put (File  => Logfile,
                         Item  => Timestamp);
         end if;
         if Facility.Is_Write_Loglevel then
            Text_IO.Put (File  => Logfile,
                         Item  => "[" & Log_Level'Image (Level) & "]");
         end if;
         Text_IO.Put (File  => Logfile,
                      Item  => " " & Msg);

         Text_IO.New_Line (File => Logfile);
      end if;
   end Write_Message;

   -----------
   -- Setup --
   -----------

   procedure Setup (Facility : in out Instance) is
   begin
      --  Nothing to do for now.
      null;
   end Setup;

   --------------
   -- Teardown --
   --------------

   procedure Teardown (Facility : in out Instance) is
   begin
      --  Close logfile if still open.
      Facility.Close_Logfile;
   end Teardown;

   -----------------
   -- Set_Logfile --
   -----------------

   procedure Set_Logfile (Facility : in out Instance; Path : String) is
      use Ada.IO_Exceptions;
   begin
      --  Somehow it's not possible to use Create() with Append_File,
      --  the file always gets truncated.
      if not Text_IO.Is_Open (File => Facility.Log_File) then
         Text_IO.Open (File => Facility.Log_File,
                       Name => Path,
                       Mode => Text_IO.Append_File);
      end if;

      Facility.Log_File_Name := To_Bounded_String (Path);

      --  Set logfile name and pointer to newly created file.
      --  Unrestricted_Access is needed here since we use a pointer
      --  which is defined externaly in the Text_IO library.
      Facility.Log_File_Ptr  := Facility.Log_File'Unrestricted_Access;

      Facility.Write_Message
        (Level => INFO,
         Msg   => "=> Alog: new logging session initialized.");
   exception
      when Name_Error =>
         --  Create file and re-call Set_Logfile.
         Text_IO.Create (File => Facility.Log_File,
                         Mode => Text_IO.Append_File,
                         Name => Path);
         Facility.Set_Logfile (Path => Path);

   end Set_Logfile;

   -----------------
   -- Get_Logfile --
   -----------------

   function Get_Logfile (Facility : in Instance) return Text_IO.File_Access is
   begin
      return Facility.Log_File_Ptr;
   end Get_Logfile;

   -------------------
   -- Close_Logfile --
   -------------------

   procedure Close_Logfile (Facility : in out Instance;
                            Remove   : in Boolean := False) is
      use Ada.Text_IO;
   begin
      if Facility.Log_File_Ptr /= Standard_Output
        and Is_Open (File => Facility.Log_File) then
         if Remove then
            --  Close and delete.
            Delete (File => Facility.Log_File);
         else
            --   Close only.
            Close (File => Facility.Log_File);
         end if;
      end if;
   end Close_Logfile;

   ----------------------------
   -- Toggle_Write_Timestamp --
   ----------------------------

   procedure Toggle_Write_Timestamp (Facility : in out Instance;
                                     Set      : in Boolean) is
   begin
      Facility.Write_Timestamp := Set;
   end Toggle_Write_Timestamp;

   ------------------------
   -- Is_Write_Timestamp --
   ------------------------

   function Is_Write_Timestamp (Facility : in Instance) return Boolean is
   begin
      return Facility.Write_Timestamp;
   end Is_Write_Timestamp;


   ---------------------------
   -- Toggle_Write_Loglevel --
   ---------------------------

   procedure Toggle_Write_Loglevel (Facility : in out Instance;
                                    Set      : in Boolean) is
   begin
      Facility.Write_Loglevel := Set;
   end Toggle_Write_Loglevel;

   -----------------------
   -- Is_Write_Loglevel --
   -----------------------

   function Is_Write_Loglevel (Facility : in Instance) return Boolean is
   begin
      return Facility.Write_Loglevel;
   end Is_Write_Loglevel;

end Alog.Facilities.File_Descriptor;
