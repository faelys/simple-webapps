------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Simple_Webapps.Upload_Servers provides a basic implementation of an HTTP --
-- file server designed around upload.                                      --
-- The main page ("/") contains a file upload form. After file upload, the  --
-- user is redirected to a report page where they can check details about   --
-- the uploaded file (Hash checksum, recorded MIME type and name, etc).     --
-- The file can then either be used directly from the server filesystem, or --
-- downloaded using a secret URI generated from a server-wide key and the   --
-- file hash. Secret download URI prevents service abuse by limiting access --
-- to people knowing the secret key.                                        --
------------------------------------------------------------------------------

with AWS.Dispatchers;
with AWS.Status;
with AWS.Response;

private with Ada.Calendar;
private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Ordered_Sets;
private with Ada.Directories;
private with Ada.Strings.Unbounded;
private with Natools.Cron;
private with Natools.References;
private with Natools.Storage_Pools;
private with Natools.S_Expressions.Atom_Refs;
private with Natools.S_Expressions.Lockable;
private with Natools.S_Expressions.Printers.Pretty;

package Simple_Webapps.Upload_Servers is

   type Handler is new AWS.Dispatchers.Handler with private;

   overriding function Dispatch
     (Dispatcher : Handler;
      Request : AWS.Status.Data)
     return AWS.Response.Data;

   overriding function Clone (Dispatcher : Handler) return Handler;

   procedure Reset
     (Dispatcher : in out Handler;
      Config_File : in String;
      Debug : in Boolean := False);


   type Log_Procedure is not null access procedure (Message : in String);

   procedure Discard_Log (Message : in String) is null;

   Log : Log_Procedure := Discard_Log'Access;
      --  Note that Log.all is called in procedures of the proctected backend
      --  objects. So Log must not be potentially blocking, but can be
      --  non-reentrant if only one backend is created.

private

   package S_Expressions renames Natools.S_Expressions;

   subtype String_Holder is Ada.Strings.Unbounded.Unbounded_String;
   function Hold (S : String) return String_Holder
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function To_String (H : String_Holder) return String
     renames Ada.Strings.Unbounded.To_String;

   subtype URI_Key is String (1 .. 27);

   type Size_Time is range 0 .. 10 ** 15;
      --  Product of a file size and a time, used to cap expiration times

   package Backend is
      package Atom_Refs renames Natools.S_Expressions.Atom_Refs;

      type File is tagged private;

      function Is_Empty (Self : File) return Boolean;
      function Name (Self : File) return String;
      function Path (Self : File) return String;
      function Comment (Self : File) return String;
      function Report (Self : File) return URI_Key;
      function Download (Self : File) return URI_Key;
      function Hash_Type (Self : File) return String;
      function Hex_Digest (Self : File) return String;
      function MIME_Type (Self : File) return String;
      function Upload (Self : File) return Ada.Calendar.Time;
      function Expiration (Self : File) return Ada.Calendar.Time;

      type File_Set is private;
      type Config_Data is private;

      protected type Database is
         function Report (Key : URI_Key) return File;

         function Download (Key : URI_Key) return File;

         function Index_Template return String;
         function Max_Expiration return Size_Time;
         function Report_Template return String;

         function Debug_Activated return Boolean;

         function Iterate
           (Process : not null access procedure (F : in File))
            return Boolean;

         procedure Add_File
           (Local_Path : in String;
            Name : in String;
            Comment : in String;
            MIME_Type : in String;
            Expiration : in Ada.Calendar.Time;
            Report : out URI_Key);
            --  Add a new file to the internal database

         procedure Purge_Expired;
            --  Remove expired entries from database

         procedure Reset
           (New_Config : in out S_Expressions.Lockable.Descriptor'Class;
            New_Debug : in Boolean := False);
            --  Reset database to a clean state with the given parameters
      private
         Config : Config_Data;
         Files : File_Set;
         Debug : Boolean := False;
      end Database;

   private

      function Path
        (Directory : Atom_Refs.Immutable_Reference;
         Report : URI_Key)
        return String;

      type File_Data is record
         Name : String_Holder;
         Comment : String_Holder;
         MIME_Type : String_Holder;
         Report : URI_Key;
         Download : URI_Key;
         Upload : Ada.Calendar.Time;
         Expiration : Ada.Calendar.Time;
         Directory : Atom_Refs.Immutable_Reference;
      end record;

      package File_Refs is new Natools.References
        (File_Data,
         Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
         Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

      type File is tagged record
         Ref : File_Refs.Immutable_Reference;
      end record;

      function Expire_Before (Left, Right : File) return Boolean;

      package File_Maps is new Ada.Containers.Ordered_Maps (URI_Key, File);
      package Time_Sets is new Ada.Containers.Ordered_Sets
        (File, Expire_Before);

      type File_Set is record
         Reports : File_Maps.Map;
         Downloads : File_Maps.Map;
         Expires : Time_Sets.Set;
      end record;

      procedure Read
        (Self : out File_Set;
         Input : in out S_Expressions.Lockable.Descriptor'Class;
         Directory : in Atom_Refs.Immutable_Reference);

      type Config_Data is record
         Storage_File : String_Holder;
         Index_Template : String_Holder;
         Report_Template : String_Holder;
         Directory : Atom_Refs.Immutable_Reference;
         HMAC_Key : String_Holder;
         Max_Expiration : Size_Time := 368_640_000;  --  100 kB.h
         Input_Dir : String_Holder;
         Printer_Param : S_Expressions.Printers.Pretty.Parameters;
      end record;

   end Backend;

   function Expiration
     (DB : Backend.Database;
      Request_Number : Natural;
      Request_Unit : String;
      Size : Ada.Directories.File_Size)
     return Ada.Calendar.Time;

   package Database_Refs is new Natools.References
     (Backend.Database,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   package Timer_Refs is new Natools.References
     (Natools.Cron.Cron_Entry,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Handler is new AWS.Dispatchers.Handler with record
      DB : Database_Refs.Reference;
      Timer : Timer_Refs.Reference;
   end record;


   type Purge_Callback is new Natools.Cron.Callback with record
      DB : Database_Refs.Reference;
   end record;

   overriding procedure Run (Self : in out Purge_Callback);

end Simple_Webapps.Upload_Servers;
