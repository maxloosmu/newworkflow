> module PropertyToTL where

> import BPMNToCSP (awk,mchans)
> import BPMN (TaskName)
> import Property
> import Formula
> import List (findIndex,nub)
> import Char (isDigit)

Take our extended grammar back to Lowe's before saturation

> derive :: Formula -> Formula
> derive (BoundEventually f i) = derive (derive1 Truef f i)
> derive (BoundUntil f (g,i)) = derive (derive1 f g i)
> derive (Implies f g) = derive (Or (negatef f) (And f g))
> derive (Next f) = Next (derive f)
> derive (And f g) = And (derive f) (derive g)
> derive (Or f g) = Or (derive f) (derive g)
> derive (Release f g) = Release (derive f) (derive g)
> derive atom = atom

> derive1 :: Formula -> Formula -> Int -> Formula
> derive1 _ f 0 = error "zero boundary"
> derive1 _ f 1 = f
> derive1 g f i = Or f (And g (Next (derive1 g f (i-1))))

> makeTL :: String -> Formula
> makeTL = makeTL0 . patternparse 

> makeTL0 :: Property -> Formula
> makeTL0 (AndP p q) = And (makeTL0 p) (makeTL0 q)
> makeTL0 (OrP p q) = Or (makeTL0 p) (makeTL0 q)
> makeTL0 (Absence p s) = absence p s
> makeTL0 (Exist p n s) = exist p n s
> makeTL0 (BoundExist p b s) = boundexist p b s
> makeTL0 (Universal p s) = universal p s
> makeTL0 (Precedes p q n s) = precede p q n s 
> makeTL0 (Response p q n s) = response p q n s

Absence pattern

> absence :: Pattern -> Scope -> Formula
> absence p Global = Release Falsef ((negatef . pattern) p)
> absence p (Before q n) = 
>	if n <= (states . pattern) p then error "bound too low"
>	else (Or (Release Falsef (negatef (pattern q))) (BoundUntil ((negatef . pattern) p) ((pattern q),n)))
> absence p (After q) = Release Falsef (Or ((negatef . pattern) q) (And (pattern q) (next (pattern q) (Release Falsef ((negatef . pattern) p)))))
> absence p (Between q r n) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>		r1 = pattern r
>	in
>		Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (BoundUntil (negatef p1) (r1,n)))))

		if n <= states q1 || n <= states p1 then error "bound too low"
		else Release Falsef (Or (negatef ant) (And ant (next q1 (BoundUntil (negatef p1) (r1,n)))))

> absence p (Until q r n) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>		r1 = pattern r
>	in
>		if n <= states q1 || n <= states p1 then error "bound too low"
>		else Release Falsef (Implies q1 (next q1 (Or (Release Falsef (negatef p1)) (BoundUntil (negatef p1) (r1,n)))))  

Universal pattern

> universal :: Pattern -> Scope -> Formula
> universal p Global = let p1 = pattern p in if single p1 then Release Falsef p1 else p1 
> universal p (Before q n) = 
>	let 
>		p1 = pattern p 
>		q1 = pattern q
>	in 
>		if single p1 then Or (Release Falsef (negatef q1)) (BoundUntil p1 (q1,n))
>		else if n > states p1 then Or (Release Falsef (negatef q1)) (And p1 ((next1 n) q1))
>		     else Or (Release Falsef (negatef q1)) (And p1 ((next p1) q1))
> universal p (After q) =
>	let 
>		p1 = pattern p 
>		q1 = pattern q
>	in 
>		if single p1 then Release Falsef (Implies q1 (next q1 (Release Falsef p1)))
>		else Release Falsef (Implies q1 (next q1 p1))
> universal p (Between q r n) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>		r1 = pattern r
>	in
>		if single p1 then Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (BoundUntil p1 (r1,n)))))
>		else if n > states p1 then Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (And p1 (next1 n r1)))))
>		     else Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 ((states p1)+1)) (And p1 (next p1 r1)))))

		if single p1 then Release Falsef (Or (negatef ant) (And ant (next q1 (BoundUntil p1 (r1,n)))))
		else if n > states p1 then Release Falsef (Or (negatef ant) (And ant (next q1 (And p1 (next1 n r1)))))
		     else Release Falsef (Or (negatef ant) (And ant (next q1 (And p1 (next p1 r1)))))

> universal p (Until q r n) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>		r1 = pattern r
>	in
>		if single p1 then Release Falsef (Or (negatef q1) (And q1 (next q1 (Or (Release Falsef p1) (BoundUntil p1 (r1,n))))))
>		else if n > states p1 then Release Falsef (Or (negatef q1) (And q1 (next q1 (Or p1 (next1 n r1)))))
>		     else Release Falsef (Or (negatef q1) (And q1 (next q1 (Or p1 (next p1 r1)))))

Existence pattern

> exist :: Pattern -> Int -> Scope -> Formula
> exist p m Global = BoundEventually (pattern p) m
> exist p m (Before q n) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>	in
>		if n < m+states(p1) then Implies (BoundEventually q1 (m+states(p1))) (BoundUntil (negatef q1) (p1,m))
>		else Implies (BoundEventually q1 n) (BoundUntil (negatef q1) (p1,m))
> exist p m (After q) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>	in
>		Release Falsef (Implies q1 (next q1 (BoundEventually p1 m)))
> exist p m (Between q r n) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>		r1 = pattern r
>	in
>		if n < m+states(p1) 
>	     	then Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 (m+states(p1))) (And (BoundUntil (negatef r1) (p1,m)) (Release r1 (negatef q1))))))
>		else Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (And (BoundUntil (negatef r1) (p1,m)) (Release r1 (negatef q1))))))

		if n < m+states(p1) 
		then Release Falsef (Implies q1 (next q1 (Implies (And (BoundEventually r1 (m+states(p1))) (BoundEventually p1 m)) (Release p1 (negatef r1)))))
		else Release Falsef (Implies q1 (next q1 (Implies (And (BoundEventually r1 n) (BoundEventually p1 m)) (Release p1 (negatef r1)))))

		if n < m+states(p1) then Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 (m+states(p1))) (BoundUntil (negatef r1) (p1,m)))))
		else Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (BoundUntil (negatef r1) (p1,m)) )))

		if single p1
		then if n < m+states(p1) 
		     then Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 (m+states(p1))) (And (BoundUntil (negatef r1) (p1,m)) (Release r1 (negatef q1))))))
		     else Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (And (BoundUntil (negatef r1) (p1,m)) (Release r1 (negatef q1))))))
		else if n < m+states(p1) 
		     then Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 (m+states(p1))) (And (BoundUntil (negatef r1) (p1,m)) (Release r1 (negatef q1))))))
		     else Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (And (BoundUntil (negatef r1) (p1,m)) (Release r1 (negatef q1))))))

> exist p m (Until q r n) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>		r1 = pattern r
>	in

		Release Falsef (Implies q1 (next q1 (And (BoundEventually p1 m) (BoundUntil (Or p1 (negatef p1)) (r1,n)))))  

>		Release Falsef (Implies q1 (next q1 (BoundUntil (negatef r1)(p1,m))))

Bounded Existence

> boundexist :: Pattern -> Bound -> Scope -> Formula
> boundexist p b Global = (disjunct . fst . unzip) (beglobal p Falsef b)
> boundexist p b (Before q n) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>		bp = (beglobal p q1 b)
>	in
>		disjunct [ Implies (BoundEventually q1 n) (BoundUntil (negatef q1) (f,n-m)) | (f,m) <- bp ]
> boundexist p b (After q) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>		bp = (beglobal p q1 b)
>	in
>		disjunct [ Release Falsef (Implies q1 (next q1 f)) | (f,m) <- bp ]

		Release Falsef (Implies q1 (next q1 (beglobal p1 q1 b)))             
		
		Release Falsef (Implies q1 (next q1 (And (beglobal p1 Falsef b) (Release Falsef (negatef q1)))))
		
> boundexist p b (Between q r n) = 
>	let
>		p1 = pattern p
>		q1 = pattern q
>		r1 = pattern r
>		bp = (beglobal p r1 b)
>	in
>		if n <= getb(b)*states(p1) 
>		then disjunct [ (Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 (m+1)) (And (And f (Release f (negatef r1))) (Release r1 (negatef q1))))))) | (f,m) <- bp ] 
>		else disjunct [ (Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (And (And f (Release f (negatef r1))) (Release r1 (negatef q1))))))) | (f,m) <- bp ] 

		then (Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 (getb(b)*states(p1)+1)) (And (And (beglobal p1 r1 b) (Release (beglobal p1 r1 b) (negatef r1))) (Release r1 (negatef q1)))))))
		else (Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (And (And (beglobal p1 r1 b) (Release (beglobal p1 r1 b) (negatef r1))) (Release r1 (negatef q1)))))))

		if n <= getb(b)*states(p1) then Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 (getb(b)*states(p1)+1)) (BoundUntil (negatef r1) ((beglobal p1 b),getb(b)*states(p1))))))
		else Release Falsef (Implies q1 (next q1 (Implies (BoundEventually r1 n) (BoundUntil (negatef r1) ((beglobal p1 b),getb(b)*states(p1))))))
		
> boundexist p b (Until q r n) =
> 	let
>		p1 = pattern p
>		q1 = pattern q
>		r1 = pattern r
>		bp = (beglobal p (Or q1 r1) b)
>	in
>		disjunct [ Release Falsef (Implies q1 (next q1 (BoundUntil (negatef r1) (f,1)))) | (f,m) <- bp ]

		Release Falsef (Implies q1 (next q1 (BoundUntil (negatef r1) ((beglobal p1 (Or q1 r1) b),1))))

		then Release Falsef (Implies q1 (next q1 (And (Implies (BoundEventually r1 n) (Release r1 (negatef q1))) 
							      (And (And (beglobal p1 r1 b) (Release (beglobal p1 r1 b) (negatef r1))) (Release r1 (negatef q1))))))
		else Release Falsef (Implies q1 (next q1 (And (Implies (BoundEventually r1 n) (Release r1 (negatef q1))) 
							      (And (And (beglobal p1 r1 b) (Release (beglobal p1 r1 b) (negatef r1))) (Release r1 (negatef q1))))))


> getb :: Bound -> Int
> getb (Atmost n) = n
> getb (Atleast n) = n
> getb (Exact n) = n

Bounded Existence with global scope

 beglobal :: Formula -> Formula -> Bound -> Formula
 beglobal f g (Exact 1) = And f (next f (Release g (negatef f))) 
 beglobal f g (Exact n) = And f (next f (beglobal f g (Exact (n-1))))
 beglobal f g (Atleast 1) = f 
 beglobal f g (Atleast n) = And f (next f (beglobal f g (Atleast (n-1))))
 beglobal f g (Atmost 1) = And (Or (negatef f) f) (next f (Release g (negatef f)))
 beglobal f g (Atmost n) = And (Or (negatef f) f) (next f (beglobal f g (Atmost (n-1))))
 
> beglobal :: Pattern -> Formula -> Bound -> [(Formula,Int)]
> beglobal p g b = 
>	let fs = combine p (getb b)
>	    ss = nub (map states fs)
>	    sl = [ ((filter ((== n) . states) fs),n)  | n <- ss ] 
>	in case b of
>		(Exact n) -> [ (And (disjunct gs) (next1 n (Release g (negatef (pattern p)))),n)  | (gs,n) <- sl ]
>		(Atleast n) -> [ (disjunct gs,n) | (gs,n) <- sl ]
>		(Atmost n) -> [ ((next1 n (Release g (negatef (pattern p)))),n) | (gs,n) <- sl ]


> combine :: Pattern -> Int -> [Formula]
> combine p 1 = pattern2 p
> combine p n = [ joins g | f <- (pattern2 p), g <- combine0 p [f] (n-1) ]

> combine0 :: Pattern -> [Formula] -> Int -> [[Formula]]
> combine0 p fs 1 = [ fs++[g] | g <- (pattern2 p) ]
> combine0 p fs n = [ q | g <- (pattern2 p), q <- combine0 p (fs++[g]) (n-1) ] 


 !P W (P W (!P W (P W []!P )))

 weak :: Formula -> (Formula,Int) -> Formula
 weak f (g,n) = Or f (BoundUntil f (g,n))

 beglobal f (Atmost n) = foldl And (Or (negatef f) f) (mkterms (n-1) (Or (negatef f) f))
	where mkterms 0 g = []
	      mkterms n g = (mkterms (n-1) g)++[mkterm n g]
	      mkterm 1 g = Next g
	      mkterm n g = Next (mkterm (n-1) g) 

p1 precedes p2

> precede :: Pattern -> Pattern -> Int -> Scope -> Formula
> precede _ _ _ _ = Live

p1 responds p2

> response :: Pattern -> Pattern -> Int -> Scope -> Formula
> response _ _ _ _ = Live

Negation only takes formulae constructed using the temporal operators O and binary operators && and ||

> negatef :: Formula -> Formula
> negatef (Implies f g) = negatef (Or (negatef f) (And f g))  
> negatef (Or f g) = And (negatef f) (negatef g)
> negatef (And f g) = Or (negatef f) (negatef g)
> negatef (BoundEventually f n) = (negatef . derive) (BoundEventually f n)
> negatef (BoundUntil f (g,n)) = (negatef . derive) (BoundUntil f (g,n))
> negatef (Next f) = Next (negatef f) -- !(O p) = (O !p)
> negatef (Available e) = Doesnot e 
> negatef (Does e) = Doesnot e
> negatef (Doesnot e) = Does e
> negatef Truef = Falsef
> negatef Falsef = Truef
> negatef Live = Deadlocked
> negatef Deadlocked = Live
> negatef f = error ("Cannot negate "++ show f)

next f g returns g composed with n Next where n is the length of f

> next :: Formula -> (Formula -> Formula)
> next = next1 . states

> states :: Formula -> Int
> states (Or f g) = max (states f) (states g)
> states (And f g) = max (states f) (states g)
> states (Next f) = 1 + (states f)
> states _ = 1

> next1 :: Int -> (Formula -> Formula)
> next1 1 = Next
> next1 n = Next . next1 (n-1)

> single :: Formula -> Bool
> single = (== 1) . states 

 single = error . show . singles 

 singles :: Formula -> [Int]

singles (Or f g) = if singles f == singles g then singles f else -1
 
 singles (And f g) = singles f ++ singles g
 singles (Next f) = [2 ++ singles f]
 singles Live = 0
 singles _ = 1
 
> join :: Formula -> Formula -> Formula
> join Deadlocked g = g
> join (Does a) g = And (Does a) (Next g)
> join (And t (Next f)) g = And t (Next (join f g))

> joins :: [Formula] -> Formula
> joins = foldl1 join  

> pattern2 :: Pattern -> [Formula]
> pattern2 = traces . pattern1

> pattern :: Pattern -> Formula
> pattern = norm . pattern1

Because it is in the form (a && O (B)) || (b && O (C))
-- LHS of && is always atomic

> traces :: Formula -> [Formula]
> traces fs = (map (norm.sequential) (traces0 [] fs))

> traces0 :: [Formula] -> Formula -> [[Formula]]
> traces0 fs (Or g h) = (traces0 fs g)++(traces0 fs h)
> traces0 fs (And g (Next h)) = traces0 (fs++[g]) h 
> traces0 fs Deadlocked = [fs++[Deadlocked]]
> traces0 _ f = (error.show) f

> sequential :: [Formula] -> Formula
> sequential [Deadlocked] = Deadlocked
> sequential (f:fs)  = And f (Next (sequential fs))

> disjunct :: [Formula] -> Formula
> disjunct [f] = f
> disjunct (f:fs) = Or f (disjunct fs)

pattern = error . show . pattern1 

> norm :: Formula -> Formula
> norm (Or Deadlocked f) = norm f
> norm (Or f Deadlocked) = norm f
> norm (Or f g) = Or (norm f) (norm g)
> norm (And Deadlocked (Next f)) = norm f
> norm (And f (Next Deadlocked)) = norm f
> norm (And f (Next g)) = And (norm f) (Next (norm g))
> norm f = f

> pattern1 :: Pattern -> Formula
> pattern1 (Nc x y) = Or (pattern1 x) (pattern1 y)
> pattern1 (Pf x y) = And (atom x) (Next (pattern1 y))
> pattern1 (Np x y) = (pattern1 . npar) (x,y)
> pattern1 End = Deadlocked

 pattern1 (Sq x y) = seqcomp(pattern1 x,pattern1 y)

 pattern :: Pattern -> Formula
 pattern (Nc x y) = if x == End then (pattern y) 
		      else if y == End then (pattern x) 
			   else Or (pattern x) (pattern y)
 pattern (Sq x y) = if x == End then (pattern y) 
		      else if y == End then (pattern x)
			   else And (pattern x) (Next (pattern y))
 pattern (Pf x y) = if y == End then (atom x)
		      else And (atom x) (Next (pattern y))
 pattern (Np x y) = (pattern . npar) (x,y)
 pattern End = Deadlocked

> atom :: Atom -> Formula
> atom (Tk t) = (Does . awk) t
> atom (NotRefuse t) = (Available . awk) t
> atom Any = Live

> npar :: (Pattern,Pattern) -> Pattern
> npar (End,End) = End
> npar (End,t) = t
> npar (s,End) = s
> npar (s,t) = Nc (ncs h1 t) (ncs h2 s)
>	where h1 = hp s 
>	      h2 = hp t

> ncs :: [(Atom,Pattern)] -> Pattern -> Pattern
> ncs [(a,s)] p = Pf a (npar (s,p))
> ncs ((a,s):ts) p = Nc (Pf a (npar (s,p))) (ncs ts p)

> hp :: Pattern -> [(Atom,Pattern)]
> hp (Nc x y) = pnc (x,y)
> hp (Pf x y) = [(x,y)]
> hp End = []
> hp (Np x y) = case npar (x,y) of
>			(Nc x1 y1) -> pnc (x1,y1)
>			x -> hp x

 hp (Sq x y) = hp x

> pnc :: (Pattern,Pattern) -> [(Atom,Pattern)]
> pnc (x,y) = (hp x)++(hp y)

 behavef :: SPEC -> FilePath -> IO()
 behavef s f = writeFile f (showCSP sp)
	where pr = behave s
	      (chn,ds) = (mkchans . alpha) pr
	      sp = (Script [(DList "Node" (nub ds))] chn [("SPEC",[],pr)] [] [])

 test = (Np (Nc (Tk "a" (Tk "e" End)) (Tk "b" End)) (Nc (Tk "c" End) (Tk "d" End)))
 test2 = (Np (Tk "a" (Tk "b" End)) (Tk "c" (Tk "d" End)))
 test3 = (Np (Tk "a" End) (Tk "b" End))


