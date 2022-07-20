CSP syntax

> module CSP where

> import List (find,nub)

> type Comment = String
> type ProcVar = String
> type SetName = String
> type DataName = String
> type Event = String
> type Var = String
> type Declaration = [(Var,Events)]

> type Expression = String

> data DataType = DList DataName [String] | DSet DataName Events deriving (Show,Eq)
> data Data = DN DataName | DS Events deriving (Show,Eq)
> data Channel = NData [String] | TData [String] [Data] deriving (Show,Eq)
> data Type = Set | Seq deriving (Show,Eq)
> data Events = SName SetName | List Type [Event] | Comp Type Declaration Expression deriving (Show,Eq)  

> data Process = Prefix Event Process | Extern Process Process | Intern Process Process | Stop | Skip | Inter Process Process | ProcId ProcVar | Hide Process Events |
>		 Parcomp (Events,Process) (Events,Process) | Interrupt Process Process | Parinter Process Events Process | Sequential Process Process |
> 		 Indextern (Var,Events) Process | Indintern (Var,Events) Process | Indparcomp (Var,Events) Events Process | Indinter (Var,Events) Process | Indparinter (Var,Events) Events Process deriving (Show,Eq)

> data Local = LP (ProcVar,[Local],Process) | LS (SetName,Events) deriving (Show,Eq) 

> data Model = T | F | FD | R deriving (Show,Eq)
> data Specification = Deter Model Process | Refine Model Process Process | Deadlock Model Process | Livelock Process deriving (Show,Eq)

> data Script = Script [DataType] [Channel] [(ProcVar,[Local],Process)] [(SetName,Events)] [Specification] deriving (Show,Eq)

return data element

> datatype :: Script -> [DataType]
> datatype (Script ds cs ps es sp) = ds

> getDName :: DataType -> DataName
> getDName (DList d ds) = d
> getDName (DSet d es) = d

> getData :: DataType -> (Either [String] Events)
> getData (DList d ds) = Left ds
> getData (DSet d es) = Right es

show data type in CSPm

> showDataType :: Script -> String
> showDataType (Script ds cs ps es sp) = (concatMap showdata ds)
>	where showdata (DList dn ls) = "datatype " ++ dn ++ " = " ++ (sd ls) ++ "\n"
>	      showdata (DSet dn (List t es)) = if t /= Set then error "Incorrect comprehension\n"
>					       else "nametype " ++ dn ++ " = " ++ (CSP.showList Set es) ++ "\n"  
>	      sd (l:[]) = l
>	      sd (l:ls) = l ++ " | " ++ (sd ls)

return channels

> channels :: Script -> [Channel]
> channels (Script ds cs ps es sp) = cs

> getChName :: Channel -> [String]
> getChName (NData ss) = ss
> getChName (TData ss d) = ss

> isCompound :: Channel -> Bool
> isCompound (TData ss d) = True
> isCompound _ = False

> getCType :: Channel -> [Data]
> getCType (TData ss d) = d


show channel in CSPm

> showChannels :: Script -> String
> showChannels (Script ds cs ps es sp) = (concatMap showchannel cs) 
>	where showchannel (NData cs) = "channel " ++ (sc cs) ++ "\n"
>	      showchannel (TData cs ds) = "channel " ++ (sc cs) ++ " : " ++ (sd ds) ++ "\n"
>	      sc [] = ""
>	      sc (c:[]) = c
>	      sc (c:cs) = c ++ " , " ++ (sc cs)
>	      sd (d:[]) = (sd2 d)
>	      sd (d:ds) = (sd2 d) ++ "." ++ (sd ds)
>	      sd2 (DN n) = n
>	      sd2 (DS (List t es)) = (CSP.showList t es)

return name/set mapping

> events :: Script -> [(SetName,Events)]
> events (Script ds cs ps es sp) = es

show enumeration/comprehension in CSPm

> showEvents :: Script -> String
> showEvents (Script ds cs ps es sp) = (showevent es ((fst.unzip) es))
>	where showevent [] _ = ""
>	      showevent ((n,(List t es)):ns) sns = n ++ " = " ++ (CSP.showList t es) ++ "\n" ++ showevent ns sns
> 	      showevent ((n,(Comp t d e)):ns) sns = n ++ " = " ++ (showComp t d e) ++ "\n" ++ showevent ns sns
>	      showevent ((n,(SName sn)):ns) sns = n ++ " = " ++ sn ++ "\n" ++ showevent ns sns

						  case find (==sn) sns of
							Just sn -> n ++ " = " ++ sn ++ "\n" ++ showevent ns sns
						 	Nothing -> error "undefined set/sequence term\n"

> getSet :: Events -> [Event]
> getSet (List t es) = es

show set in a CSPm process

> showSet :: Events -> String
> showSet (List t es) = (CSP.showList t es)
> showSet (Comp t d e) = (showComp t d e)
> showSet (SName sn) = sn 

show enumeration

> showList :: Type -> [Event] -> String
> showList t es = if t == Set then "{ " ++ sg es ++ " }" else "< " ++ sg es ++ " >"
>	where sg [] = ""
>	      sg (e:[]) = e
>	      sg (e:es) = e ++ ", " ++ (sg es)

show comprehension

 showComp :: Type -> Declaration -> Expression -> [SetName] -> String
 showComp t d e sns = if t == Set then "{ " ++ showcomp d e sns ++ " }" else "< " ++ showcomp d e sns ++ " >"
	where showcomp d e sns = 
		let decl = showDecl1 d sns
		    exp = showExp e ((fst.unzip) d) sns 
		in exp ++ " | " ++ decl
		
> showComp :: Type -> Declaration -> Expression -> String
> showComp t d e = if t == Set then "{ " ++ showcomp d e ++ " }" else "< " ++ showcomp d e ++ " >"
>	where showcomp d e = 
>		let decl = showDecl1 d
>		    exp = showExp e 
>		in exp ++ " | " ++ decl

 
> showExp :: Expression -> String
> showExp _ = ""

 showDecl1 :: Declaration -> [SetName] -> String
 showDecl1 [] _ = ""
 showDecl1 ((v,(SName sn)):ds) sns = case find (==sn) sns of
					Just set -> show v ++ " <- " ++ show set ++ " , " ++ (showDecl1 ds ([v]++sns))
					Nothing -> error "undefined set/sequence term\n"

 showDecl2 :: (Var,Events) -> [SetName] -> String
 showDecl2 (v,(SName sn)) sns = case find (==sn) sns of
					Just set -> show v ++ " : " ++ show set 
					Nothing -> error "undefined set/sequence term\n"

> showDecl1 :: Declaration -> String
> showDecl1 [] = ""
> showDecl1 ((v,(SName set)):ds) = v ++ " <- " ++ set ++ " , " ++ (showDecl1 ds)

> showSpecs :: Script -> String
> showSpecs (Script ds cs ps es sp) = concatMap showSpec sp

> showSpec :: Specification -> String
> showSpec (Deter m p) = "assert "++(showScript2 p)++" :[ deterministic ["++(showModel m)++"] ]\n"
> showSpec (Refine m p q) = "assert "++(showScript2 p)++" ["++(showModel m)++"= "++(showScript2 q)++"\n"
> showSpec (Deadlock m p) = "assert "++(showScript2 p)++" :[ deadlock free ["++(showModel m)++"] ]\n"
> showSpec (Livelock p) = "assert "++(showScript2 p)++" :[ divergence free ]\n"

> showModel :: Model -> String
> showModel (T) = "T"
> showModel (F) = "F"
> showModel (FD) = "FD"
> showModel (R) = "R"

return process

> showCSP :: Script -> String 
> showCSP s = (showDataType s) ++ "\n" ++ (showChannels s) ++ "\n" ++ (showCompress) ++ "\n" ++ (showEvents s) ++ "\n" ++ (showProcess s) ++ "\n" ++ (showSpecs s) ++ "\n" 

> showProcess :: Script -> String
> showProcess (Script ds cs ps es sp) = showScript ps

> showScript :: [(ProcVar,[Local],Process)] -> String
> showScript [] =  ""
> showScript ((v,[],p):ps) = v ++ " = " ++ (showScript2 p) ++ "\n" ++ showScript ps
> showScript ((v,ls,p):ps) = "\n"++ v ++ " =\nlet\n" ++ (concatMap ((++"\n").((++) "\t")) ((lines.showLocal) ls)) ++ "within\n\t" ++ (showScript2 p) ++ "\n\n" ++ showScript ps

> showLocal :: [Local] -> String
> showLocal [] = ""
> showLocal ((LP p):ls) = (showScript [p]) ++ showLocal ls
> showLocal ((LS s):ls) = (showEvents (Script [] [] [] [s] [])) ++ showLocal ls

> showScript2 :: Process -> String
> showScript2 p = case p of
>	Stop -> "STOP"
> 	Skip -> "SKIP"
> 	(Prefix x y) -> x ++ " -> " ++ "( " ++ (showScript2 y) ++ " )"
>  	(Extern x y) -> "( " ++ (showScript2 x) ++ " )" ++ " [] " ++ "( " ++ (showScript2 y) ++ " )"
>  	(Intern x y) -> "( " ++ (showScript2 x) ++ " )" ++ " |~| " ++ "( " ++ (showScript2 y) ++ " )"
>  	(Hide x e) -> "( " ++ (showScript2 x) ++ " )" ++ " \\ " ++ "( " ++ (showSet e) ++ " )"
>  	(Inter x y) -> "( " ++ (showScript2 x) ++ " )" ++ " ||| " ++ "( " ++ (showScript2 y) ++ " )"
>  	(Parcomp (e1,x) (e2,y)) -> "( " ++ (showScript2 x) ++ " )" ++ "[ " ++ (showSet e1) ++ " || " ++ (showSet e2) ++" ]" ++ "( " ++ (showScript2 y) ++ " )"
>  	(Interrupt x y) -> "( " ++ (showScript2 x) ++ " )" ++ " /\\ " ++ "( " ++ (showScript2 y) ++ " )"
>  	(Parinter y xs z) -> "( " ++ (showScript2 y) ++ " )" ++ " [| " ++ (showSet xs) ++ " |] " ++ "( " ++ (showScript2 z) ++ " )"
>	(Sequential y z) -> "( " ++ (showScript2 y) ++ " )" ++ " ; " ++ "( " ++ (showScript2 z) ++ " )"
>	(Indextern (v,vs) z) -> "[] "++ v ++ ":" ++ (showSet vs) ++ " @ " ++ "( " ++ (showScript2 z) ++ " )"
>	(Indintern (v,vs) z) -> "|~| "++ v ++ ":" ++ (showSet vs) ++ " @ " ++ "( " ++ (showScript2 z) ++ " )"
>  	(Indparcomp (v,vs) e z) -> "|| "++ v ++ ":" ++ (showSet vs) ++ " @ " ++ "[ " ++ (showSet e) ++ " ] " ++ "( " ++ (showScript2 z) ++ " )"
>  	(Indinter (v,vs) z) -> "||| "++ v ++ ":" ++ (showSet vs) ++ " @ " ++ "( " ++ (showScript2 z) ++ " )"
>  	(Indparinter (v,vs) e z) -> "[| " ++ (showSet e) ++ " |] " ++ v ++ ":" ++ (showSet vs) ++ " @ " ++ "( " ++ (showScript2 z) ++ " )"
>   	(ProcId x) -> x

> processes :: Script -> [(ProcVar,[Local],Process)]
> processes (Script ds cs ps es sp) = ps

> showCSPFile :: Script -> FilePath -> IO()
> showCSPFile i j = do { writeFile j (showCSP i) }

> showCompress :: String

 showCompress = ""

> showCompress = "transparent diamond, sbisim\ncompress(P) = sbisim(diamond(P))\n"

> alpha :: Process -> [Event]
> alpha p = alpha1 [] p

> alpha1 :: [Event] -> Process -> [Event]
> alpha1 es p =  case p of
> 	(Prefix x y) -> alpha1 (nub (x:es)) y  
>  	(Extern x y) -> nub ((alpha1 es x) ++ (alpha1 es y))
>  	(Intern x y) -> nub ((alpha1 es x) ++ (alpha1 es y))
>  	(Hide x e) -> (alpha1 es x)
>  	(Inter x y) -> nub ((alpha1 es x) ++ (alpha1 es y))
>  	(Parcomp (e1,x) (e2,y)) -> nub ((alpha1 es x) ++ (alpha1 es y))
>  	(Interrupt x y) -> nub ((alpha1 es x) ++ (alpha1 es y))
>  	(Parinter y xs z) -> nub ((alpha1 es y) ++ (alpha1 es z))
>	(Sequential y z) -> nub ((alpha1 es y) ++ (alpha1 es z))
>	_ -> es

			  if null [ c | c <- cs, isCompound c && (elem (take i e) (getChName c)) ]
			  then if null [ c | c <- cs, isCompound c && (elem (drop (i+1) e) (getCType c)) ]
			       then mchans [ TData [(take i e)] [(drop (i+1) e)] ] cs
			       else let (TData dn dt) = head [ c | c <- cs, isCompound c && (elem (drop (i+1) e) (getCType c)) ]
				    in [TData ([(take i e)]++dn) dt
			  else if null [ c | c <- cs, isCompound c && (elem (take i e) (getChName c)) && (elem (drop (i+1) e) (getCType c)) ]

 removeProcess :: Script -> (ProcVar,Process) -> Script
 removeProcess (Script ps es) rp = (Script nps es) 
	where nps = [ (v,p) | (v,p) <- ps, (v,p) /= rp ] 

 isProcess :: ProcVar -> (ProcVar,Process) -> Bool
 isProcess p (v,r) = v == p


