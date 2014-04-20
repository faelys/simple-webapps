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

private with Ada.Containers.Ordered_Maps;
private with Ada.Strings.Unbounded;
private with Natools.References;
private with Natools.Storage_Pools;
private with Natools.S_Expressions.Atom_Buffers;

package Simple_Webapps.Upload_Servers is

   type Handler is new AWS.Dispatchers.Handler with private;

   overriding function Dispatch
     (Dispatcher : Handler;
      Request : AWS.Status.Data)
     return AWS.Response.Data;

   overriding function Clone (Dispatcher : Handler) return Handler;

   procedure Reset
     (Dispatcher : in out Handler;
      Directory : in String;
      HMAC_Key : in String);

private

   subtype String_Holder is Ada.Strings.Unbounded.Unbounded_String;
   function Hold (S : String) return String_Holder
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function To_String (H : String_Holder) return String
     renames Ada.Strings.Unbounded.To_String;

   subtype URI_Key is String (1 .. 27);

   package Backend is
      package Atom_Refs renames Natools.S_Expressions.Atom_Buffers.Atom_Refs;

      type File is tagged private;

      function Is_Empty (Self : File) return Boolean;
      function Name (Self : File) return String;
      function Path (Self : File) return String;
      function Report (Self : File) return URI_Key;
      function Download (Self : File) return URI_Key;
      function Hash_Type (Self : File) return String;
      function Hex_Digest (Self : File) return String;

      type File_Set is private;

      protected type Database is
         function Report (Key : URI_Key) return File;

         function Download (Key : URI_Key) return File;

         procedure Add_File
           (Local_Path : in String;
            Name : in String;
            Report : out URI_Key);
            --  Add a new file to the internal database

         procedure Reset
           (New_Directory : in String;
            New_HMAC_Key : in String);
            --  Reset database to a clean state with the given parameters
      private
         Directory : Atom_Refs.Immutable_Reference;
         HMAC_Key : String_Holder;
         Files : File_Set;
      end Database;

   private

      function Path
        (Directory : Atom_Refs.Immutable_Reference;
         Report : URI_Key)
        return String;

      type File_Data is record
         Name : String_Holder;
         Report : URI_Key;
         Download : URI_Key;
         Directory : Atom_Refs.Immutable_Reference;
      end record;

      package File_Refs is new Natools.References
        (File_Data,
         Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
         Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

      type File is tagged record
         Ref : File_Refs.Immutable_Reference;
      end record;

      package File_Maps is new Ada.Containers.Ordered_Maps (URI_Key, File);

      type File_Set is record
         Reports : File_Maps.Map;
         Downloads : File_Maps.Map;
      end record;

   end Backend;

   package Database_Refs is new Natools.References
     (Backend.Database,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Handler is new AWS.Dispatchers.Handler with record
      DB : Database_Refs.Reference;
   end record;

end Simple_Webapps.Upload_Servers;
