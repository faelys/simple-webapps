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
private with Ada.Streams;
private with Ada.Strings.Unbounded;
private with Natools.References;
private with Natools.Storage_Pools;

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

   type File is record
      Name : String_Holder;
      Report : URI_Key;
      Download : URI_Key;
   end record;

   package File_Refs is new Natools.References
     (File,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   package File_Maps is new Ada.Containers.Ordered_Maps
     (URI_Key, File_Refs.Immutable_Reference, "=" => File_Refs."=");

   protected type Database is
      function Report (Key : URI_Key) return File_Refs.Immutable_Reference;

      function Download (Key : URI_Key) return File_Refs.Immutable_Reference;

      function Path (Ref : File_Refs.Immutable_Reference) return String;
      function Hex_Digest (Ref : File_Refs.Immutable_Reference) return String;
      function Hash_Name (Ref : File_Refs.Immutable_Reference) return String;

      procedure Post_File
        (Request : in AWS.Status.Data;
         Report : out URI_Key);
         --  Process Request to add a new file to the Database

      procedure Reset
        (New_Directory : in String;
         New_HMAC_Key : in String);
         --  Reset database to a clean state with the given parameters
   private
      function Path (Report : URI_Key) return String;

      Directory : String_Holder;
      HMAC_Key : String_Holder;
      Reports : File_Maps.Map;
      Downloads : File_Maps.Map;
   end Database;

   package Database_Refs is new Natools.References
     (Database,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Handler is new AWS.Dispatchers.Handler with record
      DB : Database_Refs.Reference;
   end record;

   Digit_62 : constant Ada.Streams.Stream_Element := Character'Pos ('-');
   Digit_63 : constant Ada.Streams.Stream_Element := Character'Pos ('_');
      --  Special digits for base-64 URI (RFC 4648)


end Simple_Webapps.Upload_Servers;
