--AUTEUR:	Ramusi Michael
--SECTION:	ITI 1ère année
--DATE:		Decembre 2017
--COURS:	Labo prog
--PROJET:	TP7 - Fractions / Polynomes / Reed
package gestion_fraction is
   type T_Fraction is record
      num : Integer  := 0;
      den : Positive := 1;
   end record;

   DIV_PAR_ZERO, TOO_BIG, DENOMINATOR_TOO_LITTLE : exception;

   procedure Get(Frac : out T_Fraction; Num, Den: in Integer);
   procedure Put(Frac : in T_Fraction);
	
   function "+"(Frac1, Frac2 : T_Fraction) return T_Fraction;
   function "-"(Frac1, Frac2 : T_Fraction) return T_Fraction;
   function "*"(Frac1, Frac2 : T_Fraction) return T_Fraction;
   function "*"(N : Integer; Frac : T_Fraction) return T_Fraction;
   function "*"(Frac : T_Fraction; N : Integer) return T_Fraction;   
   function "/"(Frac1, Frac2 : T_Fraction) return T_Fraction;
   function "/"(N : Integer; Frac : T_Fraction) return T_Fraction;
   function "/"(Frac : T_Fraction; N : Integer) return T_Fraction;
   function "**"(Frac : T_Fraction; N : Integer) return T_Fraction;
   function Reduce(F: T_Fraction) return T_Fraction;
   function Reel(Frac : T_Fraction) return Float;
   function PGCD(A, B: Positive) return Positive;
	
end gestion_fraction;
