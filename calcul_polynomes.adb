--AUTEUR:	Ramusi Michael
--SECTION:	ITI 1ère année
--DATE:		Decembre 2017
--COURS:	Labo prog
--PROJET:	TP7 - Fractions / Polynomes / Reed

with gestion_polynomes; use gestion_polynomes;
with gestion_fraction; use gestion_fraction;
with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; 

procedure calcul_polynomes is

   MISSING_OPERATOR: exception;         
   INVALID_OPERATOR: exception; 
   INVALID_ARGUMENT: exception;
   -----------------------------------------------------------------------------  
   function IsNumeric (chaine : in String) return Boolean is
      T : Float;
   begin
      T := Float'Value (chaine);
      return true;
   exception
      when others =>
         return false;
   end IsNumeric;

   Procedure ParseArguments(P1, P2: in out T_Polynome; Operator: out Character) is
      Tmp: T_Polynome;
      Pos, OperatorPos: Natural := 1;
      Degre : Natural := 0;
   begin
      loop
         if IsNumeric(Argument(Pos)) then
            Tmp := Alloc_Polyn(Degre);
            Tmp.Coeff(Tmp.Coeff'Last) := T_Fraction'(Integer'Value(Argument(Pos)), 
                                                     Integer'Value(Argument(Pos+1)));
            P1 := P1 + Tmp;
         else
            -- save the character as operator
            Operator := Argument(Pos)(1);
            OperatorPos := Pos;
            exit;
         end if; 
         Pos := Pos + 2;
         Degre := Degre + 1;
      end loop;
      -- first polynom not found in arguments
      if OperatorPos < 3  then
         raise INVALID_ARGUMENT;
      end if;
      
      Pos := OperatorPos + 1;
      Degre := 0;
      
      while Pos < Argument_Count loop
         if IsNumeric(Argument(Pos)) then
            Tmp := Alloc_Polyn(Degre);
            Tmp.Coeff(Tmp.Coeff'Last) := T_Fraction'(Integer'Value(Argument(Pos)), 
                                                     Integer'Value(Argument(Pos+1)));
            P2 := P2 + Tmp;
         else
            -- at this point, only numerical values are accepted
            raise INVALID_ARGUMENT;
         end if; 
         Pos := Pos + 2;
         Degre := Degre + 1;
      end loop;
      -- no 2nd polynom found in arguments
      if Degre = 0 then
         raise INVALID_ARGUMENT;
      end if;      
   end ParseArguments;
   -----------------------------------------------------------------------------   
   F : T_Fraction;
   NO_OPERATOR : constant Character := '_';
   Operator : Character := NO_OPERATOR;
   OperatorPos : Natural := 1; 
   Poly1, Poly2, PolyR : T_Polynome;
   
begin
   begin
   if Argument_Count >= 3 and then Argument_Count mod 2 = 1 then
      ParseArguments(Poly1, Poly2, Operator);
   else
      raise INVALID_ARGUMENT;
   end if;

   if Poly1.Coeff'Last > 0 then
      Poly1 := gestion_polynomes.Reduce(Poly1);
   end if;
   
   if Poly2.Coeff'Last > 0 then
      Poly2 := gestion_polynomes.Reduce(Poly2);
   end if;
   
   
   case Operator is
      when NO_OPERATOR => raise MISSING_OPERATOR;
      when '+' => 
         PolyR := Poly1 + Poly2;
         Put(PolyR);
      when '-' => 
         PolyR := Poly1 - Poly2;
         Put(PolyR);
      when 'e' => 
         if Poly2.Coeff'Last = 0 then
            F := Eval(Poly1, T_Fraction'(Poly2.Coeff(0).num, Poly2.Coeff(0).den));
            Put(F);
         else
            raise INVALID_ARGUMENT;
         end if;
      when '/' => 
         PolyR := Poly1 / Poly2;         
         Put(PolyR);
      when 'r' => 
         PolyR := Reste(Poly1, Poly2);    
         Put(PolyR);     
      when 'x' =>   
            PolyR := Poly1 * Poly2;            
            Put(PolyR);
      when others => raise INVALID_OPERATOR;
      end case;
   exception
         when INVALID_ARGUMENT => Put("Arguments invalid or missing");
   end;
end calcul_polynomes;
