> module Temporal where 

> import CSP
> import List (nub)
> import BPMNToCSP (extern,awk)
> import Formula
> import Parser
> import PropertyToTL
> import Property (specparse)

Build automaton corresponding to formula

> type StateMap = [(Int,Formulas)]
> type TransRel = [(Int,Int)]

Build up state map and transition relation.  Arguments:

- states :: StateMap : each (n,fs) pair indicates that state number n must
  satisfy formulas fs;

- delta :: TransRel : each (m,n) pair indicates that there should be
  transitions from state m to state n;

- next : Int : the number of the next state;

- unp : [(State,Formulas)] : each (n, x) pair represents that from state
  number n, the next states must satisfy x; this represents that transitions
  from n still have to be built.

> iter :: StateMap -> TransRel -> Int -> [(Int,Formulas)] -> (StateMap,TransRel)
> iter states delta next [] = (states,delta)
> iter states delta next ((n,x):unp) =
>  let satx = saturate x
>      new = -- new lists of formulas for which states have to be built
>        filter (`notElem` (map snd states)) satx
>      states' = states ++ zip [next..] new
>      unp' = unp ++ zip [next..] (map nexts new)
>      findStateNo q = head [m | (m,q') <- states', q==q']
>      delta' = delta ++ [(n,findStateNo q) | q <- satx]
>  in iter states' delta' (next+length new) unp'

>-- label :: [a] -> Int -> [(Int,a)]
>-- label xs n = zip [n..] xs

Extract "Next" formulas from list

> nexts :: Formulas -> Formulas
> nexts fs = [phi | Next phi <- fs]

Build the main automaton

> buildAuto :: Formula -> (Int, (StateMap,TransRel))
> buildAuto phi = 
>  let inits = saturate [phi]
>      states = zip [0..] inits
>      unp = zip [0..] (map nexts inits)
>      next = length states
>  in (next, iter states [] next unp)


----------------------------------------------------------------------
Make CSP specification corresponding to formula

> makeSpec0 :: Formula -> (ProcVar,[Local],Process)
> makeSpec0 phi =
>  let (n, (states, delta)) = buildAuto phi
>      events = List Set (alphas phi)
>  in if states==[] then ("SPEC",[],ProcId "div")  
>     else ("SPEC",(map (LP . (showState delta)) states), (makeIntchoice [ProcId ("SPEC_"++show m) | m <- [0..n-1]])) 

Show a single state (i.e. subsidiary process) of the spec

> showState delta (n,fs) =
>  let nexts = [ProcId ("SPEC_"++show m) | (n1,m) <- delta, n==n1]
>      nextsString = makeIntchoice nexts
>      (local,lp) = makeTrans fs nextsString
>  in ("SPEC_"++show n,local,lp)

Produce CSP process to make transitions, as required by fs, to process
nextsString.

> makeTrans fs nextsProc =
>  let doeses = [a | Does a <- fs]
>      doesnots = [a | Doesnot a <- fs]
>      avails = [a | Available a <- fs]
>  in case doeses of
>       [a] -> if null avails then ([],(Prefix a nextsProc))
>	       else ([],Extern (Prefix a nextsProc) (extern (map (\ a -> Prefix a (ProcId "div")) avails)))
>       [] -> 
>         if Deadlocked `elem` fs then ([],Stop)
>         else if doesnots==[]
>              then let any = Indintern ("x_",SName "Events") (Prefix "x_" nextsProc)
>			sp2 = (if notElem Live fs then Intern (Intern Skip Stop) any else any)
>		    in ([],if null avails then sp2 else Extern sp2 (extern (map makeAvail avails)))

              else let any = ProcId "if poss=={} then div else proceed"
		    in ([LS ("poss",SName ("diff(Events,{"++commaConcat doesnots++"})")),
		         LP ("proceed",[],Indintern ("x_",SName "poss") (Prefix "x_" nextsProc))],
			 
>              else let any = ProcId "Proceed"
>		    in ([LS ("poss",SName ("diff(Events,{"++commaConcat doesnots++"})")),
>		         LP ("Proceed",[],Indintern ("x_",SName "poss") (Prefix "x_" nextsProc))],
>		       if (Live `notElem` fs) 
>		       then if null avails then (Intern (Intern Skip Stop) any) 
>			    else Extern (Intern Stop any) (extern (map makeAvail avails))
>		       else if null avails then any
>			    else Extern any (extern (map makeAvail avails)))
>         where makeAvail a = if a `elem` doesnots then Prefix a (ProcId "div") 
>		  	      else Prefix a nextsProc 

> commaConcat = foldr1 (\ st1 st2 -> st1++","++st2)

-- spaces n = replicate n ' '
-- maybeString b st = if b then st else ""

> makeIntchoice ps = if ps==[] then (ProcId "div") else intern ps

> intern :: [Process] -> Process
> intern [] = error "intern"
> intern (p:[]) = p
> intern (p:ps) = Intern (intern [p]) (intern ps)

Alphabets of a BTL formula

> alphas :: Formula -> [Event]
> alphas = nub . alpha0

> alpha0 :: Formula -> [Event]
> alpha0 (BoundEventually f i) = alpha0 f
> alpha0 (BoundUntil f (g,i)) = (alpha0 f) ++ (alpha0 g)
> alpha0 (Implies f g) = (alpha0 f) ++ (alpha0 g)
> alpha0 (Next f) = (alpha0 f)
> alpha0 (And f g) = (alpha0 f) ++ (alpha0 g)
> alpha0 (Or f g) = (alpha0 f) ++ (alpha0 g)
> alpha0 (Release f g) = (alpha0 f) ++ (alpha0 g)
> alpha0 (Available a) = [a]
> alpha0 (Doesnot a) = [a]
> alpha0 (Does a) = [a]
> alpha0 _ = []

The main function

> makeSpec :: String -> [(ProcVar,[Local],Process)]
> makeSpec s = [("div",[],ProcId "div"),p]
>	where p = (makeSpec0 . derive . makeTL) s

> makeSpecf :: String -> String
> makeSpecf = (showScript1 . makeSpec0 . derive . makeTL)
>	where showScript1 p = showScript [("div",[],ProcId "div"),p]

> makeSpec1 :: String -> IO()
> makeSpec1 =  (putStr . makeSpecf)

 makeSpec1 :: String -> IO()
 makeSpec1 =  (putStr . showScript1 . makeSpec0 . derive . parse)
	where showScript1 p = showScript [p] 

 makeSpec2 :: FilePath -> String -> IO()
 makeSpec2 f =  ((writeFile f) . showScript1 . makeSpec0 . derive . parse)
	where showScript1 p = showScript [p] 

 makeSpec3 :: String -> IO()
 makeSpec3 =  (putStr . showScript1 . makeSpec0 . pattern . specparse)
	where showScript1 p = showScript [p]

 makeSpec4 :: String -> IO()
 makeSpec4 =  (putStr . showScript1 . makeSpec0 . negatef . pattern . specparse)
	where showScript1 p = showScript [p]

 makeSpec5 :: String -> IO()
 makeSpec5 =  (putStr . show . traces . pattern1 . specparse)
	where showScript1 p = showScript [p]


makeSpec = putStr . makeSpec0 . parse
