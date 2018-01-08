--AUTEUR:	Ramusi Michael
--SECTION:	ITI 1ère année
--DATE:		Decembre 2017
--COURS:	Labo prog
--PROJET:	TP7 - Fractions / Polynomes / Reed
with gestion_fraction; use gestion_fraction;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package gestion_polynomes is

   subtype T_Degre is natural range 0..10000;
   type T_Coeff is array(T_Degre range <>) of T_Fraction;
   
   type T_Polynome(Degre: T_Degre := 0) is record
      Coeff: T_Coeff(0..Degre);
   end record;

  procedure Get(P: in out T_polynome; Coeffs: in T_Coeff);
   procedure Put(P: T_Polynome);
   function "+"(P1, P2: T_Polynome) return T_Polynome;
   function "-"(P1, P2: T_Polynome) return T_Polynome;
   function "*"(P1, P2: T_Polynome) return T_Polynome;
   function "*"(F: T_Fraction; P: T_Polynome) return T_Polynome;
   function "*"(P: T_Polynome; F: T_Fraction) return T_Polynome;
   function "/"(P1, P2: T_Polynome) return T_Polynome;
   function Reste(P1, P2: T_Polynome) return T_Polynome;
   function Eval(P : T_Polynome; F: T_Fraction) return T_Fraction;
   function Alloc_Polyn(N: T_Degre) return T_Polynome;
   function Reduce(P: T_Polynome) return T_Polynome;
end gestion_polynomes;
