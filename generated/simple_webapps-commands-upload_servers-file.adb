with Interfaces; use Interfaces;

package body Simple_Webapps.Commands.Upload_Servers.File is

   P : constant array (0 .. 0) of Natural :=
     (0 .. 0 => 1);

   T1 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 0);

   T2 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 13);

   G : constant array (0 .. 13) of Unsigned_8 :=
     (0, 0, 0, 4, 0, 0, 0, 0, 0, 5, 0, 3, 2, 1);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 14;
         F2 := (F2 + Natural (T2 (K)) * J) mod 14;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 6;
   end Hash;

end Simple_Webapps.Commands.Upload_Servers.File;
