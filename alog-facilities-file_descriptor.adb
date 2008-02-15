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
                            Level : Log_Level)
   is
      use GNAT.Calendar.Time_IO;
      Logfile   : Text_IO.File_Type renames F.Log_File_Ptr.all;
      Timestamp : String := Image
        (Date    => Calendar.Clock,
         Picture => Picture_String (F.Timestamp_Format));
   begin
      if Level >= F.Get_Threshold then
         Text_IO.Put (File  => Logfile,
                      Item  => Timestamp);
         Text_IO.Put (File  => Logfile,
                      Item  => Log_Level'Image (Level));
         Text_IO.Put (File  => Logfile,
                      Item  => " => ");
         Text_IO.Put (File  => Logfile,
                      Item  => Msg);

         Text_IO.New_Line (File => Logfile);
      end if;
   end Write_Message;

   -------------
   -- Cleanup --
   -------------

   procedure Teardown (F : in out Instance) is
   begin
      F.Close_Logfile;
      --  Close logfile if still open.
   end Teardown;

   -----------------
   -- Set_Logfile --
   -----------------

   procedure Set_Logfile (F : in out Instance; Path : String) is
   begin
      Text_IO.Create (File => F.Log_File,
                      Name => Path,
                      Mode => Text_IO.Append_File);

      F.Log_File_Name := To_Bounded_String (Path);
      F.Log_File_Ptr  := F.Log_File'Unrestricted_Access;
      --  Set logfile name and pointer to newly created file.
      --  TODO: Unrestricted_Access is needed here since we use
      --        a pointer which is defined in a library (File_Access).

      F.Write_Message (Level => INFO,
                       Msg   => "** Alog: new logging session initialized.");
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
        and Text_IO.Is_Open (File => F.Log_File) then
         if Remove then
            Text_IO.Delete (File => F.Log_File);
            --  Close and delete.
         else
            Text_IO.Close (File => F.Log_File);
            --   Close only.
         end if;
      end if;
   end Close_Logfile;

end Alog.Facilities.File_Descriptor;
