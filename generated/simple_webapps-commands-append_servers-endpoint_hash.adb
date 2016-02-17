with Interfaces; use Interfaces;

package body Simple_Webapps.Commands.Append_Servers.Endpoint_Hash is

   P : constant array (0 .. 1) of Natural :=
     (1, 5);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (22, 6);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (3, 19);

   G : constant array (0 .. 22) of Unsigned_8 :=
     (0, 6, 0, 0, 10, 0, 0, 8, 0, 0, 0, 6, 0, 5, 1, 0, 0, 0, 2, 0, 0, 4, 6);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 23;
         F2 := (F2 + Natural (T2 (K)) * J) mod 23;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 11;
   end Hash;

end Simple_Webapps.Commands.Append_Servers.Endpoint_Hash;
