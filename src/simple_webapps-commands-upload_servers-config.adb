with Interfaces; use Interfaces;

package body Simple_Webapps.Commands.Upload_Servers.Config is

   P : constant array (0 .. 1) of Natural :=
     (1, 8);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (8, 0);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (14, 21);

   G : constant array (0 .. 21) of Unsigned_8 :=
     (1, 5, 0, 0, 9, 4, 5, 0, 0, 0, 1, 0, 0, 5, 0, 4, 0, 0, 8, 0, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 22;
         F2 := (F2 + Natural (T2 (K)) * J) mod 22;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 10;
   end Hash;

end Simple_Webapps.Commands.Upload_Servers.Config;
