with Interfaces; use Interfaces;

package body Simple_Webapps.Commands.Append_Servers.Server_Hash is

   P : constant array (0 .. 1) of Natural :=
     (1, 7);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (4, 1);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (7, 6);

   G : constant array (0 .. 10) of Unsigned_8 :=
     (0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 2);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 11;
         F2 := (F2 + Natural (T2 (K)) * J) mod 11;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 4;
   end Hash;

end Simple_Webapps.Commands.Append_Servers.Server_Hash;
