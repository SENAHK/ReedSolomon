--AUTEUR:	Ramusi Michael
--SECTION:	ITI 1ère année
--DATE:		Decembre 2017
--COURS:	Labo prog
--PROJET:	TP7 - Fractions / Polynomes / Reed

with gestion_fraction; use gestion_fraction;
with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure calcul_fraction is
   
   function IsNumeric (chaine : in String) return Boolean is      
      T : Float;
   begin
      T := Float'Value (chaine);
      return true;
   exception
      when others =>
         return false;
   end IsNumeric;
   
   NO_ARGUMENT, MISSING_ARGUMENT, INVALID_ARGUMENT: exception;
   
   F1 : T_Fraction;
   F2 : T_Fraction;
   N: Integer;
   Operator: Character;
   
   OperatorPos: Natural := 0;
begin
   begin
      -- 0 ou 1 argument -> Erreur
      if Argument_Count = 0 then
         raise NO_ARGUMENT;
      elsif Argument_Count = 1 or Argument_Count = 2 then
         raise MISSING_ARGUMENT;
      else
         
         if Argument_count = 3 then
            -- Calcul du PGCD
            if Argument(3) = "PGCD" or Argument(3) = "pgcd" then
               Get(F1, Integer'value(Argument(1)), 1);
               Get(F2, Integer'value(Argument(2)), 1);
               if F1.num > 0 and then F2.num > 0 then
                  Put(PGCD(F1.num, F2.num));
               end if;
               
            end if;
            
         end if;
	
         if Argument_count = 4 then
            -- Récupération de l'opérateur
            for i in 1..Argument_Count loop               
               if not IsNumeric(Argument(i)) then
                  Operator := Argument(i)(1);
                  OperatorPos := i;
                  exit;
               end if;
            end loop;
            -- Gestion erreur avec position de l'opérateur
            if OperatorPos = 1 or OperatorPos = 4 or OperatorPos = 0 then
               raise INVALID_ARGUMENT;
            elsif not IsNumeric(Argument(OperatorPos + 1)) or 
              not IsNumeric(Argument(OperatorPos - 1)) then
               raise INVALID_ARGUMENT;	
            end if;
            --Création des Fractions
            if OperatorPos > 2 then
               N := Integer'Value(Argument(4));
               gestion_fraction.Get(F1, Integer'Value(Argument(1)), Integer'Value(Argument(2)));  
            else               
               N := Integer'Value(Argument(1));
               gestion_fraction.Get(F1, Integer'Value(Argument(3)), Integer'Value(Argument(4))); 
            end if;
            Case Operator is
               when '+' => Put( F1 + T_Fraction'(N, 1));
               when '-' => Put( F1 - T_Fraction'(N, 1));
               when 'x' => Put( F1 * T_Fraction'(N, 1));
               when 'p' => Put( F1**N );
               when '/' => Put( F1 / N );
               when others => raise INVALID_ARGUMENT;                  
            end case;
                               
         end if;
                               
         if Argument_Count > 4 then
            for i in 1..Argument_Count loop               
               if not IsNumeric(Argument(i)) then
                  Operator := Argument(i)(1);
                  OperatorPos := i;
                  exit;
               end if;
            end loop;
            
            if OperatorPos /= 3 then
               raise INVALID_ARGUMENT;
            end if;
            
            gestion_fraction.Get(F1,Integer'value(Argument(1)), Integer'value(Argument(2)) );          
            gestion_fraction.Get(F2,Integer'Value(Argument(4)), Integer'value(Argument(5)) );

            if Argument(3) in String then
               Operator := argument(3)(1);
            end if;
		
            Case Operator is
               when '+' => Put( F1 + F2 );
               when '-' => Put( F1 - F2 );
               when 'x' => Put( F1 * F2 );
               when '/' => Put( F1 / F2 );
               when others => null;
            end case;
            
         end if;
         
      end if;
      
   exception
      when NO_ARGUMENT => Put("/!\ Please execute with arguments");
      when MISSING_ARGUMENT => Put("/!\ Missing argument"); 
      when INVALID_ARGUMENT => Put("/!\ Invalid arguments"); 
      when DIV_PAR_ZERO => Put("/!\ Division by zero");
   end;   
end calcul_fraction;
