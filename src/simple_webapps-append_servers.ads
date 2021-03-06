------------------------------------------------------------------------------
-- Copyright (c) 2016, Natacha Porté                                        --
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
-- Simple_Webapps.Append_Servers provides an HTTP server that handle POST   --
-- request by appending signed data to a given local file.                  --
------------------------------------------------------------------------------

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;
with Natools.S_Expressions.Lockable;

private with Ada.Strings.Unbounded;
private with Natools.Constant_Indefinite_Ordered_Maps;
private with Natools.References;
private with Natools.Storage_Pools;
private with Natools.S_Expressions.Atom_Refs;
private with Natools.S_Expressions.Printers.Pretty;

package Simple_Webapps.Append_Servers is

   type Handler is new AWS.Dispatchers.Handler with private;

   overriding function Dispatch
     (Dispatcher : in Handler;
      Request : in AWS.Status.Data)
     return AWS.Response.Data;

   overriding function Clone (Dispatcher : in Handler) return Handler;

   not overriding procedure Reset
     (Dispatcher : in out Handler;
      Config_File : in String);

   not overriding procedure Reset
     (Dispatcher : in out Handler;
      Config : in out Natools.S_Expressions.Lockable.Descriptor'Class;
      File_Name : in String);


   type Log_Procedure is not null access procedure (Message : in String);

   procedure Discard_Log (Message : in String) is null;

   Log : Log_Procedure := Discard_Log'Access;
      --  Note that unlike its namesake in Simple_Webapps.Upload_Server,
      --  this procedure is called from an AWS reponse callback without
      --  any protection, so it should be made task-safe.

private

   package Sx renames Natools.S_Expressions;

   subtype String_Holder is Ada.Strings.Unbounded.Unbounded_String;
   function Hold (S : String) return String_Holder
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function To_String (H : String_Holder) return String
     renames Ada.Strings.Unbounded.To_String;

   type Separator_Action is
     (No_Separator,           --  Received data is appended as-is
      Force_Separator,        --  Separator data is appended after each post
      Separator_If_Needed);   --  Separator data is appended when it's not a
                              --   suffix of the posted data

   type Separator_Data (Action : Separator_Action := No_Separator) is record
      case Action is
         when No_Separator => null;
         when Force_Separator | Separator_If_Needed =>
            Data : Sx.Atom_Refs.Immutable_Reference;
      end case;
   end record;

   type Endpoint is record
      Data_Path : String_Holder;
      Invalid_Log : String_Holder;
      Key : Sx.Atom_Refs.Immutable_Reference;
      Separator : Separator_Data;
      Redirect : String_Holder;
      Pretty_Printer : Sx.Printers.Pretty.Parameters;
   end record;

   procedure Append_Data
     (Self : in Endpoint;
      Data : in Sx.Atom);

   procedure Log_Invalid
     (Self : in Endpoint;
      Data : in Sx.Atom;
      Given_Signature : in Sx.Atom;
      Expected_Signature : in Sx.Atom);

   package Execution_Results is
      type Enum is (OK, Invalid_Signature, File_Error);

      type Data (State : Enum) is record
         case State is
            when OK =>
               Redirect : String_Holder;
            when Invalid_Signature | File_Error =>
               null;
         end case;
      end record;
   end Execution_Results;

   function Execute
     (Self : in Endpoint;
      Data : in Sx.Atom;
      Signature : in Sx.Atom)
     return Execution_Results.Data;

   package Endpoint_Maps is new Natools.Constant_Indefinite_Ordered_Maps
     (String, Endpoint);

   type Server_Data is record
      Default_Printer : Sx.Printers.Pretty.Parameters;
      Endpoints : Endpoint_Maps.Constant_Map;
      Static_Path : String_Holder;
      Template : String_Holder;
   end record;

   package Server_Refs is new Natools.References
     (Server_Data,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Handler is new AWS.Dispatchers.Handler with record
      Ref : Server_Refs.Immutable_Reference;
   end record;

end Simple_Webapps.Append_Servers;
