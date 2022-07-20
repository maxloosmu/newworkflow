> module Property where

> import BPMN (TaskName)
> import Formula
> import GenParse

> patternparse :: String -> Property
> patternparse = topLevel property0

 data Spec = Nc Spec Spec | Np Spec Spec | Sq Spec Spec | Pf Atom Spec | End 
	      deriving (Eq, Ord, Show)
	      
 data Pattern = Nc Pattern Pattern | Np Pattern Pattern | Sq Pattern Pattern | Pf Atom Pattern | End 
	      deriving (Eq, Ord, Show)

> data Pattern = Nc Pattern Pattern | Np Pattern Pattern | Pf Atom Pattern | End 
>	      deriving (Eq, Ord, Show)

> data Atom = Tk TaskName | NotRefuse TaskName | Any deriving (Eq, Ord, Show)

 data Pattern = Tn TaskName | Avail TaskName | Pattern Spec deriving (Eq, Ord, Show)

> data Property = AndP Property Property | OrP Property Property | 
>		  Absence Pattern Scope | Exist Pattern Int Scope | BoundExist Pattern Bound Scope | Universal Pattern Scope |
>		  Precedes Pattern Pattern Int Scope | Response Pattern Pattern Int Scope 
>		  deriving (Eq, Ord, Show)

> data Bound = Exact Int | Atmost Int | Atleast Int 
>	       deriving (Eq, Ord, Show)

> data Scope = Global | Before Pattern Int | After Pattern | Between Pattern Pattern Int | Until Pattern Pattern Int
>	       deriving (Eq, Ord, Show)

 type Properties = [Property] 

Grammar

number ::= 1 | 2 | 3 | 4 ... 

property0 ::= property1 ("||" property1)*
property1 ::= property2 ("&&" property2)*
property2 ::= "Abs" "(" pattern "," scope ")" | "Un" "(" pattern "," scope ")" | "Ex" "(" pattern "," number "," scope ")"  | 
              "BEx" "(" pattern "," bound "," scope ")"  | "Pre" "(" pattern "," pattern "," scope ")" | 
	      "Res" "(" pattern "," pattern "," number "," scope ")" | "(" property0 ")"

bound ::= "==" number | ">=" number | "<=" number 

scope ::= "Always" | "Before" "(" pattern "," number ")" | "After" pattern | 
	  "Between" pattern "And" "(" pattern "," number ")" | "From" pattern "Until" pattern | "(" scope ")"

pattern ::= spec0

spec0 ::= spec1 ("|~|" spec1)*
spec1 ::= spec2 ("|+|" spec2)*
spec2 ::= spec3 (";" spec3)*
spec3 ::= atom "->" spec3 | End | "(" spec0 ")"

atom :: = a | "available" a | "any"

> property0 = 
>   property1 ^^^ many (word1ws "||" ^^> property1) >>> (\ (a, b) -> foldl OrP a b)
	
> property1 =
>   property2 ^^^ many (word1ws "&&" ^^> property2) >>> (\ (a, b) -> foldl AndP a b)

> property2 =
>	word1ws "Abs" ^^> word1ws "(" ^^> pattern0 ^^^ word1ws "," ^^> scope <^^ word1ws ")" >>> (\ (a, b) -> (Absence a b))
>	|||
>	word1ws "Un" ^^> word1ws "(" ^^> pattern0 ^^^ word1ws "," ^^> scope <^^ word1ws ")" >>> (\ (a, b) -> (Universal a b))
>	|||
>	word1ws "Ex" ^^> word1ws "(" ^^> pattern0 ^^^ word1ws "," ^^> number ^^^ word1ws "," ^^> scope <^^ word1ws ")" >>> (\ (a, (b, c)) -> (Exist a b c))
>	|||
>	word1ws "BEx" ^^> word1ws "(" ^^> pattern0 ^^^ word1ws "," ^^> bound ^^^ word1ws "," ^^> scope <^^ word1ws ")" >>> (\ (a, (b, c)) -> (BoundExist a b c))
>	|||

	word1ws "Pre" ^^> word1ws "(" ^^> pattern0 ^^^ word1ws "," ^^> pattern0 ^^^ word1ws "," ^^> number ^^^ word1ws "," ^^> scope <^^ word1ws ")" >>> (\ (a, (b, (c, d))) -> (Precedes a b c d))
	|||
	word1ws "Res" ^^> word1ws "(" ^^> pattern0 ^^^ word1ws "," ^^> pattern0 ^^^ word1ws "," ^^> number ^^^ word1ws "," ^^> scope <^^ word1ws ")" >>> (\ (a, (b, (c, d))) -> (Response a b c d))
	|||
	
>	word1ws "(" ^^> property0 <^^ word1ws ")"

 pattern0 = 
	word1ws "Behaviour" ^^> word1ws "(" ^^> spec0  <^^ word1ws ")" >>> Pattern
	|||
	word1ws "Available" ^^> wordws >>> Avail
	|||
	((check (flip notElem ["Behaviour","Available"])) . wordws) >>> Tn
	|||
	word1ws "(" ^^> pattern0 <^^ word1ws ")"
	
> pattern0 = spec0 

> spec0 = 
>	spec1 ^^^ many (word1ws "|~|" ^^> spec1) >>> (\ (a, b) -> foldl Nc a b)

> spec1 =
>	spec2 ^^^ many (word1ws "|+|" ^^> spec2) >>> (\ (a, b) -> foldl Np a b)

> spec2 = spec3

 spec2 =
	spec3 ^^^ many (word1ws ";" ^^> spec3) >>> (\ (a, b) -> foldl Np a b)

 spec3 =
	spec4 <^^ (word1ws "->") ^^^ spec5 >>> (\ (a, b) -> Pf a b)

 spec4 = 
	word1ws "Available" ^^> wordws >>> NotRefuse
	|||
	((check (flip notElem ["Behaviour","Available","End","Live"])) . wordws) >>> Tk
	|||
	word1ws "Live" ^^> succeed Any

> spec3 =
>	(((check (flip notElem ["available","any"])) . wordws) <^^ word1ws "->") ^^^ spec4 >>> (\ (a, b) -> Pf (Tk a) b)
>	|||
>	(word1ws "available" ^^> wordws <^^ word1ws "->") ^^^ spec4 >>> (\ (a, b) -> Pf (NotRefuse a) b)
>	|||
>	(word1ws "any" <^^ word1ws "->") ^^^ spec4 >>> (\ (a, b) -> Pf Any b)
>	|||
>	word1ws "End" ^^> succeed End

> spec4 = word1ws "End" ^^> succeed End
>	  |||
>	  word1ws "(" ^^> spec0 <^^ word1ws ")"
>	  |||
>	  spec0

	(((check (flip notElem ["Behaviour","Available","End","Live"])) . wordws) <^^ word1ws "->") ^^^ spec3 >>> (\ (a, b) -> Pf a b)

> bound = 
>	word1ws "==" ^^> number >>> Exact
>	|||
>	word1ws ">=" ^^> number >>> Atleast
>	|||
>	word1ws "<=" ^^> number >>> Atmost
>	|||
>	word1ws "(" ^^> bound <^^ word1ws ")"

> scope = 
>	word1ws "Always" ^^> succeed Global
>	|||
>	word1ws "Before" ^^> word1ws "(" ^^> pattern0 ^^^ word1ws "," ^^> number <^^ word1ws ")" >>> ( \ (a,b) -> Before a b )
>	|||
>	word1ws "After" ^^> pattern0 >>> After
>	|||
>	word1ws "Between" ^^> pattern0 ^^^ word1ws "And" ^^> word1ws "(" ^^> pattern0 ^^^ word1ws "," ^^> number <^^ word1ws ")" >>> (\ (a, (b, c)) -> Between a b c)
>	|||
>	word1ws "From" ^^> pattern0 ^^^ word1ws "Until" ^^> word1ws "(" ^^> pattern0 ^^^ word1ws "," ^^> number <^^ word1ws ")"  >>> (\ (a, (b, c)) -> Until a b c)
>	|||
>	word1ws "(" ^^> scope <^^ word1ws ")"

> specparse :: String -> Pattern
> specparse = topLevel spec0

