with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body gestion_polynomes is   
   -- # FUNCTION: "+"
   --   Addition of two polynomials
   function "+"(P1, P2: T_Polynome) return T_Polynome is
      r, smaller : T_Polynome;      
   begin
      
      if P1.coeff'Length <= P2.coeff'Length then
         r := P2;
         smaller := P1;
      else
         r := P1;
         smaller := P2;
      end if;
      
      for I in smaller.Coeff'range loop
         r.Coeff(I) := r.Coeff(I) + smaller.Coeff(I);        
      end loop;
      
      return r;
   end "+";
      
   -- # FUNCTION: "-"
   --   Soustraction of two polynomials
   function "-"(P1, P2: T_Polynome) return T_Polynome is
      F: T_Fraction := (-1,1);
   begin      
      return P1+(F*P2);
   end "-";
      
   -- # FUNCTION: "Eval"
   --   Evaluation of a polynom for a certain F
   function Eval(P: T_Polynome; F: T_Fraction) return T_Fraction is
      r : T_Fraction := (0,1);
   begin
      -- Horner's Method
      for I in reverse P.Coeff'Range loop
         r := r * F + P.Coeff(I);
      end loop;      
      return r;
   end Eval;
   
      
   -- # FUNCTION: "/"
   --   Division of two polynomials
   function "/"	(P1, P2: T_Polynome) return T_Polynome is
      N : T_Polynome := P1;
      D : T_Polynome := P2;
      Q : T_Polynome;
      Qi : T_Polynome;
      DIV_BY_GREATER: exception;
   begin
      if P2.Coeff'Last = 0 and P2.Coeff(0).num = 0 then
         raise DIV_PAR_ZERO;
      end if;
      
      if N.Coeff'Length < D.Coeff'Length then
         raise DIV_BY_GREATER;
      end if;
      -- Q is the quotient
      Q := Alloc_Polyn(P1.Coeff'Length-P2.Coeff'Length);
      
      for i in Q.Coeff'Range loop
         Q.Coeff(Q.Coeff'Length-i-1) := N.Coeff(N.Coeff'Length-i-1) / D.Coeff(D.Coeff'Length-1);
         -- Qi is a monom based on one element of Q that will multiply the divisor
         Qi := Alloc_Polyn(Q.Coeff'Length-1-i);
         Qi.Coeff(Q.Coeff'Length-1-i) := Q.Coeff(Q.Coeff'Length-1-i);         
         N := N - ( Qi * D ); 
      end loop;
      return Q;      
   end "/";
      
   -- # FUNCTION: "Reste"
   --   Reste of the division of two polynomials
   function Reste(P1, P2: T_Polynome) return T_Polynome is
      Q: T_Polynome := P1 / P2;
      F: T_Fraction := (-1,1);
   begin
      return F*Q*P2+P1;
   end Reste;
   
      
   -- # FUNCTION: "*"
   --   Multiplication of two polynomials
   Function "*"(P1, P2: T_Polynome) return T_Polynome is
      R : T_Polynome( (P1.Coeff'Length-1)+(P2.Coeff'Length-1) );
      
   begin
      for I in P1.Coeff'Range loop
         for J in P2.Coeff'Range loop
            R.Coeff(I+J) := R.Coeff(I+J) + P1.Coeff(I)*P2.Coeff(J);
         end loop;
      end loop;
      return R;
   end "*";
      
   -- # FUNCTION: "*"
   --   Multiplication of a polynomial and a fraction
   function "*"	(F: T_Fraction; P: T_Polynome) return T_Polynome is
      R : T_Polynome(P.Coeff'Length-1);
   begin
      for I in P.Coeff'Range loop
         R.Coeff(I) := F * P.Coeff(I);
      end loop;
      return R;
   end "*";
      
   -- # FUNCTION: "*"
   --   Multiplication of a polynomial and a fraction
   function "*"	(P: T_Polynome; F: T_Fraction) return T_Polynome is
      R : T_Polynome(P.Coeff'Length-1);
   begin
      for I in P.Coeff'Range loop
         R.Coeff(I) := F * P.Coeff(I);
      end loop;
      return R;
   end "*";
      
   -- # FUNCTION: "Alloc_Polyn"
   --   returns a polynomial of a certain degree
   function Alloc_Polyn(N: T_Degre) return T_Polynome is
      Poly : T_Polynome(N);
   begin
      return Poly;
   end Alloc_Polyn;
      
   -- # FUNCTION: "Reduce"
   --   Remove parts of a polynomial
   function Reduce(P: T_Polynome) return T_Polynome is      
      Cpt: Natural := 0;
      R: T_Polynome;
   begin
      -- start at end of polynomial to delete extra degree
      for i in reverse p.Coeff'First..P.Coeff'Last loop
         if P.Coeff(I).num = 0 then
            Cpt := cpt + 1;
         else
            exit;
         end if;               
      end loop;
      
      --Creation of a polynomial of the right size
      if cpt > P.Coeff'last then
         R := Alloc_Polyn(0);
      else
         R := Alloc_Polyn(P.Coeff'Last-Cpt);
      end if;
      -- Fill the polynomial
      for i in R.Coeff'Range loop
         R.Coeff(i) := P.Coeff(I);
      end loop;
      return r;
   end Reduce;
      
   -- # PROCEDURE: "Put"
   --   Print a polynomial
   Procedure Put(P: T_Polynome) is
   begin      
      New_Line;      
      for I in  P.Coeff'Range loop
         if I = 0 then
            gestion_fraction.Put(P.Coeff(I));
         elsif I = 1 then
            gestion_fraction.Put(P.Coeff(I)); Put("x ");
         else            
            gestion_fraction.Put(P.Coeff(I)); Put("x ^" &Integer'Image(I));
         end if;
      end loop;        
      New_Line;
   end Put;
      
   -- # FUNCTION: "Get"
   --   Creates a polynomial
   procedure Get(P: in out T_polynome; Coeffs: in T_Coeff) is
   begin
      P := Alloc_Polyn(Coeffs'Last);
      
      for i in Coeffs'Range loop
         P.Coeff(I) := Coeffs(i);
      end loop;      
   end get;
end gestion_polynomes;
