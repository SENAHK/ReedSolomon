with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body gestion_fraction is
   --------------------------------------------------------------------------------
   -- # FUNCTION: Reduce
   --   return the reduced fraction 
   function Reduce(F: T_Fraction) return T_Fraction is
      d: Positive := 1;
   begin
      if F.num = 0 then
         return T_Fraction'(0, 1);
      else
         d := PGCD(abs(F.num), F.den);
         return T_Fraction'(F.num/d, F.den/d);
      end if;
   end Reduce;   
   --------------------------------------------------------------------------------
   -- # PROCEDURE: Get
   --   Creates a T_Fraction
   procedure Get(Frac : out T_Fraction; Num, Den: in Integer) is
   begin
      if Frac.den = 0 then
         raise DIV_PAR_ZERO;
      else
         Frac := Reduce(Frac);
         
      end if;
      
      if Den = 0 then
         raise DIV_PAR_ZERO;
      else
         if Den < 0 then
            raise DENOMINATOR_TOO_LITTLE;
         else            
            if Num > Integer'Last or Den > Integer'Last then
               raise TOO_BIG;
            else
               Frac.Den := Den;
               Frac.Num := Num;
               Frac := Reduce(Frac);
            end if;
         end if;
      end if;
   end Get;
   --------------------------------------------------------------------------------
   -- # PROCEDURE: Put
   --   Print a T_Fraction
   procedure Put(Frac : in T_Fraction) is
   begin
      Text_IO.Put("   " & Integer'Image(Frac.num) & " /" & Integer'Image(Frac.den) & " " );
   end Put;
   --------------------------------------------------------------------------------
   -- # FUNCTION: "*"
   --   Multiply two fraction
   function "*" (Frac1, Frac2: T_Fraction) return T_Fraction is
   begin      
      return Reduce( T_Fraction'(Frac1.num*Frac2.num, Frac1.den*Frac2.den) );
   end "*";
   --------------------------------------------------------------------------------
   -- # FUNCTION: "*"
   --   Multiply a fraction and an integer	
   function "*"(N : Integer; Frac : T_Fraction) return T_Fraction is
   begin
      return Reduce( T_Fraction'(N*Frac.num, Frac.den) );
   end "*";
   -------------------------------------------------------------------------------- 
   -- # FUNCTION: "*"
   --   Multiply a fraction and an integer	
   function "*"(Frac : T_Fraction; N : Integer) return T_Fraction is
   begin
      return Reduce( T_Fraction'(N*Frac.num, Frac.den) );
   end "*";
   --------------------------------------------------------------------------------  
   -- # FUNCTION: "/"
   --   Division of two fraction 
   function "/"(Frac1,Frac2 : T_Fraction) return T_Fraction is
      F1: T_Fraction := Frac1;
      F2: T_Fraction := Frac2;
   begin
      if F1.num < 0 and then F2.num < 0 then
         F1.num := abs(F1.num);
         F2.num := abs(F2.num);
      end if;
      if F2.num < 0 then
         F1.num := F1.num*(-1);
         F2.num := abs(F2.num);
      end if;
      if F2.num = 0 then
         return T_Fraction'(0,1);
      end if;
      
      return Reduce( T_Fraction'(F1.num*F2.den, F1.den*F2.num) );
    
   end "/";
   
   --------------------------------------------------------------------------------  
   -- # FUNCTION: "/"
   --   Division of integer and fraction
   function "/"(N : Integer; Frac : T_Fraction) return T_Fraction is
   begin     
      if N = 0 then
         raise DIV_PAR_ZERO; 
      else         
         return Frac / T_Fraction'(N, 1);
      end if;  
   end "/"; 
   -------------------------------------------------------------------------------- 
   -- # FUNCTION: "/"
   --   Division of integer and fraction
   function "/"(Frac : T_Fraction; N : Integer) return T_Fraction is
   begin
      if N = 0 then
         raise DIV_PAR_ZERO; 
      else         
         return Frac / T_Fraction'(N, 1);
      end if;      
   end "/"; 
   -------------------------------------------------------------------------------- 
   -- # FUNCTION: "**"
   --   Power of a fraction
   function "**"(Frac : T_Fraction; N : Integer) return T_Fraction is
   begin   
      if Frac.num < 1 then
         raise DIV_PAR_ZERO;
      end if;
      
      if N < 0 then
         return T_Fraction'(1, Frac.num**(abs(N)));
      else
         return Reduce( T_Fraction'(Frac.num**N, Frac.den**N) );
      end if;
   end "**";
   --------------------------------------------------------------------------------  
   -- # FUNCTION: Reel
   --   Float value of a fraction
   function Reel(Frac : T_Fraction) return Float is
   begin
      return Float(Frac.num) / Float(Frac.den);
   end Reel;
   -------------------------------------------------------------------------------- 
   -- # FUNCTION: PGCD
   --   PGCD
   function PGCD(A, B: Positive) return Positive is
   begin
      if A rem B = 0 then
         return B;
      else
         return PGCD(B, A rem B);
      end if;
   end PGCD;
   -------------------------------------------------------------------------------- 
   -- # FUNCTION: "+"
   --   Addition of two fraction

   function "+" (Frac1, Frac2: T_Fraction) return T_Fraction is
      commonDen: Positive := Frac1.den;
      Num1: Integer := Frac1.num;
      Num2: Integer := Frac2.num;
   begin
      if Frac1.den /= Frac2.den then		
         commonDen := Frac1.den* Frac2.den;
         Num1 := commonDen / Frac1.den * Frac1.num;
         Num2 := commonDen / Frac2.den * Frac2.num;	
      end if;		
      return Reduce( T_Fraction'(Num1+Num2, commonDen) );	
   end "+";
   -------------------------------------------------------------------------------- 
   -- # FUNCTION: "-"
   --   Soustraction of two fraction
   function "-" (Frac1, Frac2: T_Fraction) return T_Fraction is
      commonDen: Positive := Frac1.den;
      Num1: Integer := Frac1.num;
      Num2: Integer := Frac2.num;
   begin
      if Frac1.Den /= Frac2.den then		
         commonDen := Frac1.den* Frac2.den;
         Num1 := commonDen / Frac1.den * Frac1.num;
         Num2 := commonDen / Frac2.den * Frac2.num;	
      end if;
		
      return Reduce( T_Fraction'(Num1-Num2, commonDen) );	
   end "-";
	
end gestion_fraction;
