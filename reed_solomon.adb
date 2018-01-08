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
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Containers.Vectors;

procedure reed_solomon is

   -- K is the size of the list of bytes
   K: constant Integer := 5;
   -- N is the aditionnal byte to append to the list of bytes
   N: constant Integer := 2;
   
   -- Arbitrary range of error
   --(by definition, reed solomon can't correct more than N/2 errors)
   subtype T_Error is Natural range 1..N+1;
   package Random_Errors is new Ada.Numerics.Discrete_Random(T_Error);

   subtype T_Range is Natural range 0..K-1+N;
   package Random_Position is new Ada.Numerics.Discrete_Random(T_Range);

   subtype T_Byte is Natural range 0..255;
   package Random_Byte is new Ada.Numerics.Discrete_Random(T_Byte);

   ByteGen: Random_Byte.Generator;
   PosGen: Random_Position.Generator;
   NbErrors: Random_Errors.Generator;

   -- Used for the list of bytes
   type T_Coordinates is record
      X: Integer;
      Y: Integer;
   end record;
   type T_Bytes is array(Natural range <>) of T_Coordinates;

   -- Used for the list of polynomials 
   type T_R_Polynomial is record
      Occurence: Positive;
      Polynom: T_Polynome;
   end Record;
   type T_Polynomials is array (Natural range <>) of T_R_Polynomial;
   
   type T_Numbers is array (Natural range <>) of Natural;
   package P_Vectors is new Ada.Containers.Vectors(Positive, T_R_Polynomial); 
   use P_Vectors;
   -----------------------------------------------------------------------------
   -- # Procedure:   Put
   --
   -- # Description: print the list of bytes
   procedure Put(List: in T_Bytes) is
   begin
      for I in List'Range loop
         if i = list'first then
            Put("[" & Integer'Image(List(I).Y) & ",");
         elsif i = list'Last then
            Put(Integer'Image(List(I).Y) & " ]");
         else
            Put(Integer'Image(List(I).Y) & ",");
         end if;
      end loop;
      New_Line;
   end Put;
   
   -----------------------------------------------------------------------------
   -- # Procedure:    PrintPolynoms
   --
   -- # Description: print the list of polynomials and their occurences
   procedure PrintPolynoms(List: in Vector) is
   begin
      for i in List.First_Index..List.Last_Index loop
         New_Line;
         Put(List(I).Occurence);
         Put(" occurences: ");
         Put(List(i).Polynom);
      end loop;
   end PrintPolynoms;
   -----------------------------------------------------------------------------
   -- # Function:    Produit
   --
   -- # Description: Multiplication of degre 1 terms
   -- # Parameters:
   --  +List:        T_Bytes
   --  +I:           Natural
   -- # Returns:     T_Polynome
   function Produit(List: T_Bytes; I: Natural) return T_Polynome is
      P: T_Polynome(1);
   begin
      P.Coeff(1) := (1, 1);
      P.Coeff(0) := (-(List(I).X), 1);
      if I < List'Last then
         return P*Produit(List, I+1);
      else
         return P;
      end if;
   end Produit;
   -----------------------------------------------------------------------------
   -- # Function:    Produit
   --
   -- # Description: Multiplication of degre 0 terms
   -- # Parameters:
   --  +List:        T_Bytes
   --  +I:           Natural
   --  +CurrentX:    T_Fraction
   -- # Returns:     T_Polynome
   function Produit(List: T_Bytes; I: Natural; CurrentX: T_Fraction)
                    return T_Polynome is
      P: T_Polynome(0);
   begin
      P.Coeff(0) := CurrentX-T_Fraction'(List(I).X, 1);
      if I < List'Last then
         return P*Produit(List, I+1, CurrentX);
      else
         return P;
      end if;
   end Produit;
   -----------------------------------------------------------------------------
   -- # Function:    Interpolate
   -- # Description: Builds a polynom from a list of X and Y
   --                with Lagrange interpolation
   -- # Parameters:
   --  +List:        T_Bytes
   -- # Returns:     Polynom T_Polynome
   function Interpolate(List: T_Bytes) return T_Polynome is
      R, Top, Bottom: T_Polynome;
      X, Y: T_Fraction;
   begin
      for i in List'Range loop
         X := (List(I).X, 1);
         Y:= (List(I).Y, 1);
         if i = List'First then
            Top := Y* Produit(List(List'First+1..List'Last), 1);
            Bottom := Produit(List(List'First+1..List'Last), 1, X);
         else
            Top := Y*Produit(List(List'First..i-1) & List(i+1..List'Last), 0);
            Bottom := Produit(List(List'First..i-1) & List(i+1..List'Last), 0, X);
         end if;
         R := R + (Top/Bottom);
      end loop;
      return R;
   end Interpolate;

   -----------------------------------------------------------------------------   
   -- # Procedure:    AddToList
   -- # Description: Adds the values entered by the user in the list of bytes
   -- # Parameters:
   --  +List:        T_Bytes
   --  +Pos:         Natural
   --  +Val:         Natural
   procedure AddToList(List: in out T_Bytes; Pos: in Natural; Val: in Natural) is
   begin
      List(Pos).X := Pos;
      List(Pos).Y := Val;
   end AddToList;
   -----------------------------------------------------------------------------
   -- # Procedure:    Encode
   -- # Description:  Add additionnal bytes to the list to encode it
   -- # Parameters:
   --  +List:         T_Bytes
   --  +K, N:         Natural
   --  +P:            T_Polynome
   procedure Encode(List: in out T_Bytes; K, N: Natural; P: T_Polynome) is
      T: T_Polynome;
      F: T_Fraction;
      Max: Natural := K+N;
   begin
      -- Starts after k and add N coordinates to the list
      for I in K..Max-1 loop
         List(I).X := I;
         F := Eval(P, (I,1));
         List(I).Y := F.num;
      end loop;
   end Encode;
   -----------------------------------------------------------------------------
   -- # Procedure:    Noisify
   -- # Description:  Change some bytes of the list to simulate a noisy channel
   -- # Parameters:
   --  +List:         T_Bytes
   --  +K, N:         Natural
   --  +P:            T_Polynome   
   procedure Noisify(List: in out T_Bytes) is
      Gen: Generator;
      Val, Pos : Natural;
      Max: Natural;
   begin
      Random_Byte.Reset(ByteGen);
      Random_Position.Reset(PosGen);
      Random_Errors.Reset(NbErrors);
      Max := Random_Errors.Random(NbErrors);
      --Max := Integer(Float(List'Last/2)*Random(Gen)+1.0);
      for i in 1..Max loop
         Val := Random_Byte.Random(ByteGen);
         Pos := Random_Position.Random(PosGen);
         List(Pos).Y := Val;
         Random_Byte.Reset(ByteGen);
         Random_Position.Reset(PosGen);
      end loop;
   end Noisify;
   -----------------------------------------------------------------------------
   -- # Procedure:    AddToListPolynomials
   -- # Description:  Add the polynom to the array of record
   -- # Parameters:
   --  +List:         T_Polynomials
   --  +Pos:          Natural
   --  +P:            T_Polynome     
   procedure AddToListPolynomials(Vect: in out Vector; P: in T_Polynome) is
      Exists: Boolean := false;
      NewP: T_R_Polynomial;
   begin
      -- Seek the polynom in the vector of polynom
      for i in Vect.First_Index..Vect.Last_Index loop
         -- if polynom already exists
         if Element(Vect, I).Polynom = P then
            -- increment occurence
            Vect(I).Occurence := Element(Vect, I).Occurence + 1;
            Exists := true;
         end if;
      end loop;

      -- If polynom doesnt exist in the vector, add it with an occurence of 1
      if not Exists then
         NewP := (1, P);
         Vect.Append(NewP);
      end if;
   end AddToListPolynomials;

   -----------------------------------------------------------------------------
   -- # Procedure:    Decode
   --
   -- # Description:  Creates combination of index into temporary array (T_Numbers),
   --                 interpolate a polynom passing through it,
   --                 and insert into vector of polynoms
   -- # Parameters:
   --  + Combination: T_Numbers
   --  + ListB:       T_Bytes
   --  + ListP:       Vector
   --  + K:           Natural
   --  + N:           Natural
   procedure Decode(Combination: in out T_Numbers; ListB: in T_Bytes;
                    ListP: in out Vector; K, N: in Natural; 
                    index: in Natural; CPT: in out Natural) is
      First: Natural;
      ShortListB: T_Bytes(Combination'Range);
      P: T_Polynome;
   begin
      -- End of ieme combinaison
      if index >= K then
         New_Line;
         -- For each combi of index | ex: 0, 3, 4 out of  [0, 1, 2, 3, 4] |
         for i in Combination'range loop
            -- Creates a temporary List of Bytes: ShortList has combination
            -- of indexes from ListB
            ShortListB(i) := ListB(Combination(i));
         end loop;
         -- Interpolate the polynom passing through the "coordinates"
         -- of the combination | ex: (0;y1), (3;y2), (4;y3) |
         P := Interpolate(ShortListB);
         -- Push the Polynom to the vector of polynom
         AddToListPolynomials(ListP, P);
         Cpt := Cpt + 1;
         New_Line; 
         Put("Combi " & Integer'Image(CPT));
         Put(ShortListB);
         Put(P);
      else
         first := 0;
         if index > 0 then
            first := Combination(index-1)+1;
         end if;
         for i in first..N-1 loop
            Combination(index) := i;
            Decode(Combination, ListB, ListP, K, N, index+1, Cpt);
         end loop;
      end if;
   end Decode;
   -----------------------------------------------------------------------------
   -- # Procedure:    RetrieveInitialList
   -- # Description:  Recreate the initial list by evaluating the polynom
   -- # Parameters:
   --  +ListPoly:     T_Polynomials
   --  +N:            Natural
   --  +P:            T_Polynome  
   procedure RetrieveInitialList(ListPoly: in Vector; N: in Natural) is
      Max: Natural := 1;
      Pos : Integer := - 1;
      P: T_Polynome;
      List: T_Bytes(0..N);
   begin
      for i in ListPoly.First_Index..ListPoly.Last_Index loop
         if Element(ListPoly, I).Occurence > Max then
            Pos := I;
         end if;
      end loop;
      if Pos /= -1 then
         P := ListPoly(Pos).Polynom;
         for i in List'Range loop
            List(i).x := I;
            List(i).Y := Eval(P, (I, 1)).num;
         end loop;
         New_Line;
         Put("Initial list: ");
         Put(List);
         New_Line;
         Put("Associated polynom: ");
         Put(P);
      else
         Put("Initial list could not be retrieved");
      end if;
   end RetrieveInitialList;
   -----------------------------------------------------------------------------
   -- ListBytes' final length will be equals to K+N (-1 since starting at 0)
   ListBytes: T_Bytes(0..K+N-1);
   -- Number of combination = K
   Combinations: T_Numbers(0..K-1);

   
   P: T_Polynome;
   
   -- Not efficient, should use Vector but not allowed because Ada95
   --ListPoly : T_Polynomials(0..nCr(K+N, K)-1);
   ListPol: Vector;
   
   I, Pos: Natural := 0;
   Nb: Integer := 0;
   --_________________________________MAIN_____________________________________
begin
   begin
      -- INTERACTIVE MODE
      if Argument_Count = 0 then
         Put("K: " & Integer'Image(K));
         New_Line;
         Put("N: " & Integer'Image(N));
         New_Line;
         
         Put_line("List of bytes definition:");
         -- User's defined bytes
         while I < K loop
            Put_line("Byte " & Integer'Image(I+1) & " of " & Integer'Image(K));
            Put("Write a number from 0 to 255: ");
            Get(Nb);
            if Nb >= 0 and then Nb <= 255 then
               AddToList(ListBytes, I, Nb);
               I := I + 1;
            else
               Put_Line("Number is not in the range.");
            end if;
         end loop;

         Put("List K : ");
         Put(ListBytes(0..K-1));
         New_Line;

         -- Polynom interpolated with the bytes
         P := Interpolate(ListBytes(0..K-1));
         Put("P(k) : ");
         Put(P);
         New_Line;

         -- Encode by adding bytes matching the interpolated polynom
         Encode(ListBytes, K, N, P);
         Put("List K + N: ");
         Put(ListBytes);
         New_Line;

         -- Simulate the alteration of the list by adding error to it
         Noisify(ListBytes);
         Put("List K + N, after passing through noise: ");
         Put(ListBytes);
         New_Line;

         -- Decode the altered list by interpolating a polynom passing through
         -- k element out of the length of initial list
         -- => kCLength
         Put("Trying to retrieve initial list: ");
         Decode(Combinations, ListBytes, ListPol, Combinations'Length, ListBytes'Length, 0, Pos);

         -- Print the polynomials found, with their occurences
         Put("Polynomials that could match the K+N List");
         New_Line;
         PrintPolynoms(ListPol);

         -- Retrieve the initial list and the associated polynom
         RetrieveInitialList(ListPol, ListBytes'Last);
      else
         --------------------------------------------------------------------------
         -- WITH ARGUMENTS
         if Argument_Count mod 2 = 0 then
            I := 1;
            -- Retrieve arguments and add them to list of bytes
            while I < Argument_Count loop
               ListBytes(Pos).X := Integer'Value(Argument(i));
               ListBytes(Pos).Y := Integer'Value(Argument(i + 1));
               I := I + 2;
               Pos := Pos + 1;
            end loop;
            New_Line;
            Put(ListBytes(0..Pos-1));
            -- Interpolate the list of bytes
            P := Interpolate(ListBytes(0..Pos-1));
            Put(P);
         end if;

      end if;

   exception
      when Constraint_Error => Put("Overflow error: please use smaller numbers."); 
      when Data_Error => Put_Line("Wrong type given.");
   end;
end reed_solomon;
