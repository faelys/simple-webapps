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

package body Simple_Webapps is

   function HTML_Escape (Unsafe_Input : String) return String is
      function Length (Input : String) return Natural;

      function Length (Input : String) return Natural is
         Result : Natural := Input'Length;
      begin
         for I in Input'Range loop
            case Input (I) is
               when '<' | '>' =>
                  Result := Result + 3;
               when '&' =>
                  Result := Result + 4;
               when '"' =>
                  Result := Result + 5;
               when others =>
                  null;
            end case;
         end loop;

         return Result;
      end Length;

      Result : String (1 .. Length (Unsafe_Input));
      O : Positive := Result'First;
   begin
      for I in Unsafe_Input'Range loop
         case Unsafe_Input (I) is
            when '<' =>
               Result (O .. O + 3) := "&lt;";
               O := O + 4;
            when '>' =>
               Result (O .. O + 3) := "&gt;";
               O := O + 4;
            when '&' =>
               Result (O .. O + 4) := "&amp;";
               O := O + 5;
            when '"' =>
               Result (O .. O + 5) := "&quot;";
               O := O + 6;
            when others =>
               Result (O) := Unsafe_Input (I);
               O := O + 1;
         end case;
      end loop;

      return Result;
   end HTML_Escape;

end Simple_Webapps;
