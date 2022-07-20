> module BPMNToCSP where

> import List (find,union,intersect,delete,nub,isPrefixOf,isSuffixOf,span)
> import System.IO
> import CSP
> import BPMN
> import XMLToBPMN (xTob,countEnds,countAborts,countEerrors)
> import Text.XML.HaXml.Types
> import Text.XML.HaXml.Parse (xmlParse)

> import System.Cmd (system)

DataTypes Channels Processes Sets

> dir :: String
> dir = "examples//time//"

> dir2 :: String
> dir2 = "examples//case//"

> dir3 :: String
> dir3 = "..//oworkflow//example//"


Test Cases

> test :: IO()
> test = do {
>		putStr "translate timed_example_single.ibp\n";
> 		(semanticsf "timed_example_single.ibp"  "example0.csp");
>		putStr "translate timed_example_sequential.ibp\n";
> 		(semanticsf "timed_example_sequential.ibp"  "example0a.csp");
>		putStr "translate timed_example.ibp\n";
> 		(semanticsf "timed_example.ibp"  "example1.csp");
>		putStr "translate timed_example2.ibp\n";
> 		(semanticsf "timed_example2.ibp"  "example2.csp");
>		putStr "translate timed_example_and.ibp\n";
> 		(semanticsf "timed_example_and.ibp"  "example3.csp");
>		putStr "translate timed_example_and_untimed.ibp\n";
> 		(semanticsf "timed_example_and_untimed.ibp"  "example3a.csp");
>		putStr "translate timed_example_and_except.ibp\n";
> 		(semanticsf "timed_example_and_except.ibp"  "example4.csp");
>		putStr "translate timed_example_sub.ibp\n";
> 		(semanticsf "timed_example_sub.ibp"  "example5.csp");
>		putStr "translate timed_example_miseq.ibp\n";
> 		(semanticsf "timed_example_miseq.ibp"  "example6.csp");
>		putStr "translate timed_example_sub_except.ibp\n";
> 		(semanticsf "timed_example_sub_except.ibp"  "example7.csp");
>		putStr "translate timed_example_and_sub.ibp\n";
> 		(semanticsf "timed_example_and_sub.ibp"  "example8.csp");
>		putStr "translate timed_example_and_sub_except.ibp\n";
> 		(semanticsf "timed_example_and_sub_except.ibp"  "example9.csp");
>		putStr "translate timed_example_miseqs.ibp\n";
> 		(semanticsf "timed_example_miseqs.ibp"  "example10.csp");
>		putStr "translate timed_example_miseqs_ntstart.ibp\n";
> 		(semanticsf "timed_example_miseqs_ntstart.ibp"  "example10a.csp");
>		putStr "translate timed_example_miseq_except.ibp\n";
> 		(semanticsf "timed_example_miseq_except.ibp"  "example11.csp");
>		putStr "translate timed_example_miseqs_except.ibp\n";
> 		(semanticsf "timed_example_miseqs_except.ibp"  "example12.csp");
>		putStr "translate timed_example_collab_simple.ibp\n";
> 		(semanticsf "timed_example_collab_simple.ibp"  "example13.csp");
>		putStr "translate timed_example_collab_simple.ibp\n";
> 		(semanticsf2 "timed_example_collab_simple.ibp" "timed_example_spec.ibp" "example14.csp" );
>		putStr "translate timed_example_sub_nest.ibp\n";
> 		(semanticsf "timed_example_sub_nest.ibp" "example15.csp");
>		putStr "translate timed_example_sub_nest_and.ibp\n";
> 		(semanticsf "timed_example_sub_nest_and.ibp" "example16.csp");
>		putStr "translate timed_example_sub_nest_and1.ibp\n";
> 		(semanticsf "timed_example_sub_nest_and1.ibp" "example17.csp");
>		putStr "translate timed_example_sub_nest_and2.ibp\n";
> 		(semanticsf "timed_example_sub_nest_and2.ibp" "example18.csp");
>		putStr "translate timed_example_sub_and2.ibp\n";
> 		(semanticsf "timed_example_sub_and2.ibp" "example18a.csp");
>	}

> example0 = semanticsf "timed_example_single.ibp"  "example0.csp"
> example0a = semanticsf "timed_example_sequential.ibp"  "example0a.csp"
> example1 = semanticsf "timed_example.ibp"  "example1.csp"
> example2 = semanticsf "timed_example2.ibp"  "example2.csp"
> example3 = semanticsf "timed_example_and.ibp"  "example3.csp"
> example3a = semanticsf "timed_example_and_untimed.ibp"  "example3a.csp"
> example4 = semanticsf "timed_example_and_except.ibp"  "example4.csp"
> example5 = semanticsf "timed_example_sub.ibp"  "example5.csp"
> example6 = semanticsf "timed_example_miseq.ibp"  "example6.csp"
> example7 = semanticsf "timed_example_sub_except.ibp"  "example7.csp"
> example8 = semanticsf "timed_example_and_sub.ibp"  "example8.csp"
> example9 = semanticsf "timed_example_and_sub_except.ibp"  "example9.csp"
> example10 = semanticsf "timed_example_miseqs.ibp"  "example10.csp"
> example10a = semanticsf "timed_example_miseqs_ntstart.ibp"  "example10a.csp"
> example11 = semanticsf "timed_example_miseq_except.ibp"  "example11.csp"
> example12 = semanticsf "timed_example_miseqs_except.ibp"  "example12.csp"
> example13 = semanticsf "timed_example_collab_simple.ibp"  "example13.csp"
> example14 = semanticsf2 "timed_example_collab_simple.ibp" "timed_example_spec.ibp" "example14.csp"

> example15 = semanticsf "timed_example_sub_nest.ibp" "example15.csp"
> example16 = semanticsf "timed_example_sub_nest_and.ibp" "example16.csp"
> example17 = semanticsf "timed_example_sub_nest_and1.ibp" "example17.csp"
> example18 = semanticsf "timed_example_sub_nest_and2.ibp" "example18.csp"
> example18a = semanticsf "timed_example_sub_and2.ibp" "example18a.csp"

> testcase :: IO() 
> testcase = do {
>		putStr "translate neotango_eligibility.ibp\n";
> 		(semanticsf3 "neotango_eligibility.ibp" "neotango_eligibility.csp");

		putStr "translate neotango_simple.ibp\n";
 		(semanticsf3 "neotango_simple.ibp" "neotango_simple.csp");

>		putStr "translate neotango_randomisation.ibp\n";
> 		(semanticsf3 "neotango_randomisation.ibp" "neotango_randomisation.csp");

		putStr "translate neotango_randomisation_simple.ibp\n";
 		(semanticsf3 "neotango_randomisation_simple.ibp" "neotango_randomisation_simple.csp")
		
>		putStr "translate neotango_followup.ibp\n";
> 		(semanticsf3 "neotango_followup.ibp" "neotango_followup.csp");		
		
>	}

> eligibility = semanticsf3 "neotango_eligibility.ibp" "neotango_eligibility.csp"
> random = semanticsf3 "neotango_randomisation.ibp" "neotango_randomisation.csp"

> a1work = semanticsf3 "neotango_treatment_a1_abstract_obv_auto.ibp" "neotango_treatment_a1_abstract_obv_auto.csp"
> a11work = semanticsf3 "neotango_treatment_a1_1_abstract_obv.ibp" "neotango_treatment_a1_1_abstract_obv.csp"

> a1noobv = semanticsf3 "neotango_treatment_a1_no_obv_auto.ibp" "neotango_treatment_a1_no_obv_auto.csp"
> a1noobvut = semanticsf4 "neotango_treatment_a1_no_obv_auto.ibp" "neotango_treatment_a1_no_obv_auto.csp"

> a11 = semanticsf3 "neotango_treatment_a1_1.ibp" "neotango_treatment_a1_1.csp"


 simple = semanticsf3 "neotango_simple.ibp" "neotango_simple.csp"
 simple2 = semanticsf3 "neotango_simple2.ibp" "neotango_simple2.csp"
 random = semanticsf3 "neotango_randomisation.ibp" "neotango_randomisation.csp"
 random2 = semanticsf3 "neotango_randomisation_simple.ibp" "neotango_randomisation_simple.csp"
 random3 = semanticsf3 "neotango_randomisation_simple2.ibp" "neotango_randomisation_simple2.csp"


 example6a = bpmn "timed_example_miseq.ibp"  "temp"

 eg6 = [("Pool",[Atomic [(State (Stime (MkTime Pve 0 0 0 0 15 0)) [] [(True,"Flow11")] [] (ZERO,ZERO) [] [] [] [] [] 1),
                 (State (End 0) [(True,"Flow12")] [] [] (ZERO,ZERO) [] [] [] [] [] 1),
                 (State (Miseq "A" 1 StandardT NoCond) [(True,"Flow11")] [(True,"Flow12")] [] (MkTime Pve 0 0 0 0 0 0,MkTime Pve 0 0 0 0 5 0) [] [] [] [] [] 1)]])]

For generating BPMN data structure

> bpmn :: FilePath -> FilePath -> IO()
> bpmn i j = do { xml <- readFile (dir++i);
>	          if null j then putStr ((show . xTob) (xmlParse "" xml))
>		  else writeFile (dir++j) ((show . xTob) (xmlParse "" xml)) }

> bpmn1 :: FilePath -> FilePath -> IO()
> bpmn1 i j = do { xml <- readFile i;
>	          if null j then putStr ((show . xTob) (xmlParse "" xml))
>		  else writeFile j ((show . xTob) (xmlParse "" xml)) }

> nsemanticsf :: FilePath -> FilePath -> IO()
> nsemanticsf r w = do { xml <- readFile (dir++r);
>			h <- (openFile (dir++w) WriteMode);
>			(hPutStr h ((showCSP .  bToc . xTob) (xmlParse r xml))) `catch` (force h);
>			hClose h }

> force :: Handle -> IOError -> IO()
> force h e = hClose h 

> semanticsf :: FilePath -> FilePath -> IO()
> semanticsf r w = do { xml <- readFile (r);
>			case w of
>				"" -> putStr ((showCSP .  bTou . xTob) (xmlParse r xml))
>				s -> writeFile (dir++w) ((showCSP .  bTou . xTob) (xmlParse r xml))  }
			
> semanticsf2 :: FilePath -> FilePath -> FilePath -> IO()
> semanticsf2 m s o = do { xml1 <- readFile (dir++m);
>			   xml2 <- readFile (dir++s);
>			   case o of
>				"" -> putStr (showCSP (refine ((bToc . xTob . (xmlParse s)) xml2) ((bToc . xTob) (xmlParse m xml1))))
>				x -> writeFile (dir++o) (showCSP (refine ((bToc . xTob . (xmlParse s)) xml2) ((bToc . xTob) (xmlParse m xml1))))  }

> semanticsf3 :: FilePath -> FilePath -> IO()
> semanticsf3 r w = do { xml <- readFile (dir2++r);
>		         case w of
>				"" -> putStr ((showCSP .  bToc . xTob) (xmlParse r xml))
>				s -> writeFile (dir2++w) ((showCSP .  bToc . xTob) (xmlParse r xml))  }

> semanticsf4 :: FilePath -> FilePath -> IO()
> semanticsf4 r w = do { xml <- readFile (r);
>		         case w of
>				"" -> putStr ((showCSP .  btoc1 . xTob) (xmlParse r xml))
>				s -> writeFile (dir2++w) ((showCSP .  btoc1 . xTob) (xmlParse r xml))  }

purely untimed semantics

> bTou :: BPMN -> Script
> bTou b = btoc1 b

> btoc1 :: BPMN -> Script
> btoc1 = uglobal

> uglobal :: BPMN -> Script
> uglobal b | length b == 1 = s
>	    | otherwise = (merge wd (ucollab b))
>	where s = ((foldl merge (Script [] [] [] [] [])) . (map enactment)) b
>	      wd = makeCSpec b s

	      wd = makeSpec ("",(concatMap snd b)) s

> ucollab :: BPMN -> Script
> ucollab b = (Script [dns] [] [ucollab] cas spec)
>	where dns = (DList "Proc" (map (gname.fst) b))
>	      ucps = [ ((ugpname n),[],ProcId ((compress.uname) n)) | (n,ns) <- b ]
>	      cas = [ ((ganame n),(List Set (aprocess ns))) | (n,ns) <- b ]
>	      ms = [ e | (n,ns) <- b, e <- (getMsgEvents.aprocess) ns] 
>	      ucollab = ("UC",(map LP ucps),Hide (Indparcomp ("i",(List Set (map (gname.fst) b))) (SName ("GA(i)")) (ProcId ("UGP(i)"))) (List Set ms))
>	      spec = [(Refine F (ProcId "DF") (ProcId "UC"))]

> urefine :: Script -> Script -> Script
> urefine s i = merge (merge s i) (Script [] [] [] [] rfs)
>	where rfs = [Refine F (Hide (ProcId (utoplevel s)) eas) (Hide (ProcId (utoplevel i)) ses)]
>	      ses = let xs = List Set [ x | (i,j) <- (events s), isPrefixOf ("A"++((tail.utoplevel) s)) i, x <- getSet j, isPrefixOf "starts." x]
>		    in SName ("diff(Events,"++(showSet xs)++")")
>	      eas = SName ("{| fin,aborts,error |}")


> utoplevel :: Script -> ProcVar
> utoplevel s = 
>	let ns = map fst3 (processes s)
>	in case find (=="UC") ns of
>		Just x -> "UC"
>		Nothing -> last ns

> bToc :: BPMN -> Script
> bToc = global 

> global :: BPMN -> Script
> global b | length b == 1 = s
>	   | otherwise = (merge wd (collab b))
>	where s = ((foldl merge (Script [] [] [] [] [])) . (map local)) b
>	      wd = makeCSpec b s

	      wd = makeSpec ("",(concatMap snd b)) s

> refine :: Script -> Script -> Script
> refine s i = merge (merge s i) (Script [] [] [] [] rfs)
>	where rfs = [Refine F (ProcId (toplevel s)) (Hide (ProcId (toplevel i)) ses),Refine F (ProcId (toplevel s)) (Hide (ProcId ("U"++((tail.toplevel) i))) ses) ]
>	      ses = let xs = List Set [ x | (i,j) <- (events s),  isPrefixOf ("A"++((tail.toplevel) s)) i, x <- getSet j]
>		    in SName ("diff(Events,"++(showSet xs)++")")

> toplevel :: Script -> ProcVar
> toplevel s = 
>	let ns = map fst3 (processes s)
>	in case find (=="TC") ns of
>		Just x -> "TC"
>		Nothing -> last ns

> proprefine :: String -> [(ProcVar,[Local],Process)] -> Script -> Script
> proprefine f sp m = Script dt c (ps++sp) sets (specs++rfs)
>	where tl = if f == "u" then (utoplevel m) else ("U"++((tail.toplevel) m))
>	      rfs = [Refine R (ProcId "SPEC") (ProcId tl)]
>	      (Script dt c ps sets specs) = m

> merge :: Script -> Script -> Script
> merge (Script sd sc sp se ss) (Script td tc tp te ts) = (Script (mdts sd td) (mchans sc tc) (mprocs sp tp) (msets se te) (mspecs ss ts))

> collab :: BPMN -> Script
> collab b = (Script [dns] [] [ucollab,tcollab] cas spec)
>	where dns = (DList "Proc" (map (gname.fst) b))
>	      ucps = [ ((ugpname n),[],ProcId ((compress.uname) n)) | (n,ns) <- b ]
>	      tcps = [ ((tgpname n),[],ProcId ((compress.tname) n)) | (n,ns) <- b ]
>	      cas = [ ((ganame n),(List Set (aprocess ns))) | (n,ns) <- b ]
>	      ms = [ e | (n,ns) <- b, e <- (getMsgEvents.aprocess) ns] 
>	      ucollab = ("UC",(map LP ucps),Hide (Indparcomp ("i",(List Set (map (gname.fst) b))) (SName ("GA(i)")) (ProcId ("UGP(i)"))) (List Set ms))
>	      tcollab = ("TC",(map LP tcps),Hide (Indparcomp ("i",(List Set (map (gname.fst) b))) (SName ("GA(i)")) (ProcId ("TGP(i)"))) (List Set ms))
>	      spec = [(Refine F (ProcId "DF") (ProcId "UC")),(Refine F (ProcId "DF") (ProcId "TC"))]

> mdts :: [DataType] -> [DataType] -> [DataType]
> mdts xd yd = foldl mdt xd yd

> mdt :: [DataType] -> DataType -> [DataType]
> mdt xd (DList d ds) = 
>	case find ((== d).getDName) xd of
>		Just (DList e es) ->  [DList e (union es ds)]++[x | x <- xd, (getDName x) /= e]  
>		Just (DSet _ _) -> error "Incompatible datatype"
>		Nothing -> ([DList d ds]++xd)

merge channel

> mchans :: [CSP.Channel] -> [CSP.Channel] -> [CSP.Channel]
> mchans xc yc = foldl mchan xc yc

> mchan :: [CSP.Channel] -> CSP.Channel -> [CSP.Channel]
> mchan xc (NData cs) = [NData (union (concat [ getChName x | x <- xc, (not.isCompound) x ]) cs)] ++ [ x | x <- xc, isCompound x ]
> mchan xc (TData cs es) = (mchan1 cs es [ x | x <- xc, isCompound x ]) ++ [ x | x <- xc, (not.isCompound) x ]

> mchan1 :: [String] -> [Data] -> [CSP.Channel] -> [CSP.Channel]
> mchan1 [] es xc = xc
> mchan1 cs es [] = [(TData cs es)]
> mchan1 cs es ((TData ss ds):xc) =
>	let dup = [ s | s <- ss, elem s cs]
>	in if null dup then [(TData ss ds)]++(mchan1 cs es xc)
>	   else [(TData ss (mdatas (zip ds es)))] ++ (mchan1 [ c | c <- cs, notElem c ss] es xc)

partial definition of merging data types for channels

> mdatas :: [(Data,Data)] -> [Data]
> mdatas [] = []
> mdatas (((DS (List Set xs)),(DS (List Set ys))):ds) = nub ([(DS (List Set ((nub.(union xs)) ys)))]++(mdatas ds))
> mdatas (((DN m),(DN n)):ds) = nub ([(DN n)]++(mdatas ds))
> mdatas ds = error ((show ds) ++ "\n\n mdatas")

> mprocs :: [(ProcVar,[Local],Process)] -> [(ProcVar,[Local],Process)] -> [(ProcVar,[Local],Process)]
> mprocs ps qs = foldl mproc ps qs

> mproc :: [(ProcVar,[Local],Process)] -> (ProcVar,[Local],Process) -> [(ProcVar,[Local],Process)]
> mproc ps (v,l,p) = 
>	case find ((== v).fst3) ps of
>		Just (w,wl,q) -> [p | p <- ps, ((fst3 p) /= v)]++[(v,l,p)]
>		Nothing -> ps ++ [(v,l,p)]

> msets :: [(SetName,Events)] -> [(SetName,Events)] -> [(SetName,Events)]
> msets ss tt = foldl mset ss tt

> mset :: [(SetName,Events)] -> (SetName,Events) -> [(SetName,Events)]
> mset ss (n,(List Set ns)) = 
>	case find ((== n).fst) ss of
>		Just (x,(List Set y)) -> nub ([s | s <- ss, ((fst s) /= n)]++[(x,(List Set (union ns y)))])
>		Nothing -> nub (ss ++ [(n,(List Set ns))])

> mspecs :: [Specification] -> [Specification] -> [Specification]
> mspecs ss tt = ss++tt

> local :: (PName,[StateSet]) -> Script
> local bp = merge (merge ent coord) (tcompose bp)
>	where ent = enactment bp
>	      coord = coordination bp

relative timed semantics (partial interleaving of enactment and coordination) 

> tcompose :: (PName,[StateSet]) -> Script
> tcompose (n,ss) = (Script [] [] [tp] [] [rf])
>	where le = ((map (invEnd.getType)) . (filter (isEnd.getType))) (level ss)
>	      se = [ "fin."++((show.invEnd.getType) s) | s <- allstates ss, notElem s (level ss), ((isEnd.getType) s) ]
>	      ae = [ s | s <- (aprocess ss), notElem s se && notElem s ((getMsgEvents.aprocess) ss) ]

	      tp2 = (((tname n)++"2"), Parinter (ProcId (lname n)) (List Set ae) (ProcId (cname n)))
	      tp1 = (((tname n)++"1"),ProcId ("compress("++(tname n)++"2)") )
	      
>      	      tp1 = (((tname n)++"1"),[],Parinter (ProcId (lname n)) (List Set ae) (ProcId (cname n)))
>	      ts = nub [ e | s <- (allstates ss), e <- (map atrans ((getIn s)++(getOut s)++((snd.unzip.getError) s))) ]
>	      tp = ((tname n),[(LP tp1)],Hide (ProcId ((tname n)++"1")) (List Set ts))
>	      me = [ e | e <- aprocess ss, (isSuffixOf ".init") e || (isSuffixOf ".done") e ]
>	      rf = (if null me then (Refine F (ProcId (dfname n)) (ProcId (tname n)))
>		    else (Refine F (ProcId (dfname n)) (Hide (ProcId (tname n)) (List Set me) )))

	      tp1 = (((tname n)++"1"), Parinter (ProcId (lname n)) (List Set (aprocess ss)) (ProcId (cname n)))

	      se = [ "fin."++((show.invEnd.getType) s) | s <- allstates ss, notElem s (level ss), ((isEnd.getType) s) ]
	      ae = [ s | s <- (aprocess ss), notElem s se ]

> aprocess :: [StateSet] -> [Event]
> aprocess ss = (nub.concat) [ (astate ss e) | e <- level ss ]    

> enactment :: (PName,[StateSet]) -> Script
> enactment (n,ss) = merge ck (Script [] [] [hus] [] [nr])
> 	where ck = makeSpec (n,ss) (enact (n,ss))
>	      me = [ e | e <- aprocess ss, (isSuffixOf ".init") e || (isSuffixOf ".done") e ]
>	      nr = (if null me then (Refine F (ProcId (dfname n)) (ProcId (uname n)))
>		    else (Refine F (ProcId (dfname n)) (Hide (ProcId (uname n)) (List Set me) )))
>	      ts = nub [ e | s <- (allstates ss), e <- (map atrans ((getIn s)++(getOut s)++((snd.unzip.getError) s)))++(getExceptTrans s) ]
>	      hus = ((uname n),[],Hide (ProcId (lname n)) (List Set ts))

	      nr = (Refine F (ProcId "DF") (ProcId (uname n)))

> getExceptTrans :: State -> [Event]
> getExceptTrans s = 
>	let t = getType s 
>	in if isIerror t && invIerror t /= NoException then ["except."++((show . invException . invIerror) t)]
>	   else if isEerror t && (snd . invEerror) t /= NoException then ["except."++((show . invException . snd .invEerror) t)]
>		else [ "except."++((show . invException . invIerror) t)  | t <- (fst . unzip . getError) s, isIerror t && invIerror t /= NoException ] 
	      
> enact :: (PName,[StateSet]) -> Script
> enact (n,ss) = merge sp (compose (n,ss) ((nub.concat.snd.unzip) astates))
>	where atom = (getAtomic ss)
>	      subs = delete (Atomic atom) ss 
>	      fn = ((map (invEnd.getType)) . (filter (isEnd.getType))) atom
>	      an = ((map (invAbort.getType)) . (filter (isAbort.getType))) atom
>	      en = ((map (fst.invEerror.getType)) . (filter (isEerror.getType))) atom
>	      adde s = if isStarts s then (addendaborterror 's' fn an en) else (addendaborterror 'e' fn an en)
>	      states = [ ((adde s).(rstate ss n)) s | s <- level ss, (not.isEerror.getType) s && (not.isEnd.getType) s && (not.isAbort.getType) s  && (not.isMults) s]
>		       ++ [ rend n fn an en (getType s) (getIn s) | s <- level ss, (isEnd.getType) s ]
>		       ++ [ rabort n fn an en (getType s) (getIn s) | s <- level ss, (isAbort.getType) s ]
>		       ++ [ rerror n fn an en (getType s) (getIn s) | s <- level ss, (isEerror.getType) s ]
>		       ++ [ addendaborterror 'e' fn an en (rmi ss n s) | s <- level ss, isMults s ]
>	      allsts = if null subs then (Script [] [] states [] []) 
>		       else merge (Script [] [] states [] []) (foldl1 merge (map (rsub.getSP) subs))
>	      astates = [ ((aname n k),(union ["aborts."++(show a) | a <- an ] (union [ "error."++(show e) | e <- en ] (union ["fin."++(show f) | f <- fn ] (astate ss k))))) | k <- level ss ]
>	      ae = nub ((map show) (union (countEnds ss) (union (countAborts ss) (countEerrors ss))))
>	      chs = (getChannels.nub.concat.snd.unzip) astates
>	      dt = [ sname(getType k,getOut k) | k <- (atom++(map (fst.getSP) subs)) ]
>	      sp = merge (Script [(DList "Node" dt),(DList "Msg" ["init","done"])] chs [] [(k,(List Set kk)) | (k,kk) <- astates ] []) allsts

> rmi :: [StateSet] -> PName -> State -> (ProcVar,[Local],Process)
> rmi ss p s | (isMipar.getType) s || (isMipars.getType) s = rmipar ss p s
>	     | (isMiseq.getType) s || (isMiseqs.getType) s = rmiseq ss p s

return process corresponding end state

> rend :: PName -> [Int] -> [Int] -> [Int] -> BPMN.Type -> [Transition] -> (ProcVar,[Local],Process)
> rend p fs a es y ts = addendaborterror 'e' (delete (invEnd y) fs) a es (pn,[],(Sequential (xgate ts) (Prefix ("fin."++(show.invEnd) y) Skip)))
>	where pn = ((name p)++"("++sname(y,ts)++")")

return process corresponding end state

> rerror :: PName -> [Int] -> [Int] -> [Int] -> BPMN.Type -> [Transition] -> (ProcVar,[Local],Process)
> rerror p fs a es y ts = 
>	if (snd.invEerror) y == NoException then addendaborterror 'e' fs a (delete ((fst.invEerror) y) es) (pn,[],(Sequential (xgate ts) (Prefix ("error."++(show.fst.invEerror) y) Skip)))
>	else addendaborterror 'e' fs a (delete ((fst.invEerror) y) es) (pn,[],(Sequential (xgate ts) (Prefix ("except."++(show.invException.snd.invEerror) y) (Prefix ("error."++(show.fst.invEerror) y) Skip))))
>	where pn = ((name p)++"("++sname(y,ts)++")")

	      fs1 = (map ((endindex p).show) (delete (invEnd y) fs))
	      a1 = (map ((endindex p).show) a))

return process corresponding terminate state

> rabort :: PName -> [Int] -> [Int] -> [Int] -> BPMN.Type -> [Transition] -> (ProcVar,[Local],Process)
> rabort p fs a es y ts = addendaborterror 'e' fs (delete (invAbort y) a) es (pn,[],(Sequential (xgate ts) (Prefix ("aborts."++(show.invAbort) y) Stop)))
>	where pn = ((name p)++"("++sname(y,ts)++")")

 rabort p fs a y ts = addendabort 'e' fs1 a1 (pn,[],(Sequential (xgate ts) (Prefix ("aborts."++(endindex p (invAbort y))) Stop)))
	where pn = ((name p)++"("++sname(y,ts)++")")
	      fs1 = (map ((endindex p).show) fs)
	      a1 = (map ((endindex p).show) (delete (invAbort y) a))


return external choice over given process and ending process  
e.g. P(a) = (init.a -> starts.a -> init.b -> P(a)) [] fin.1 -> Skip

> addendaborterror :: Char -> [Int] -> [Int] -> [Int] -> (ProcVar,[Local],Process) -> (ProcVar,[Local],Process)
> addendaborterror _ [] [] [] x = x
> addendaborterror t s a e (v,l,p) =

>	let ep = Indextern ("i",List Set (map show s)) (Prefix "fin.i" Skip)
>	    ap = Indextern ("i",List Set (map show a)) (Prefix "aborts.i" Stop)
>	    rp = Indextern ("i",List Set (map show e)) (Prefix "error.i" Skip)

	let ep = Indextern ("i",List Set s) (Prefix "fin.i" Skip)
	    ap = Indextern ("i",List Set a) (Prefix "aborts.i" Stop)
	    
>	    er = if null e then ep else (Extern ep rp)
>	in case p of
>		(Sequential x y) -> if t == 's' then (if null a then (v,l,Sequential p er) else (v,l,Interrupt (Sequential p er) ap))
>				    else if null a then (v,l,Extern p er) else (v,l,(Extern (Sequential (Interrupt x ap) y) er))
>		_ -> if null a then (v,l,Sequential p er) else (v,l,Interrupt (Sequential p er) ap)

> rsub :: (State,[StateSet]) -> Script
> rsub (s,ss) | (isBpmn.getType) s = enact ((fst.invBpmn.getType) s,ss)
>	      | (isMiseqs.getType) s = enact ((fst4.invMiseqs.getType) s,ss)
>	      | (isMipars.getType) s = enact ((fst4.invMipars.getType) s,ss)

returning the parallel composition of processes per local diagram 

> compress :: String -> String
> compress p = "compress("++p++")"

> compose :: (PName,[StateSet]) -> [Event] -> Script
> compose (n,ss) es = Script [] [] [us] [] []
>	where ees =  [ "fin."++((show.invEnd.getType) s) | s <- level ss, (isEnd.getType) s ]
>	      us = ((lname n),[],(Indparcomp ((index n),(List Set (indexSet (Right ss)))) (SName (((BPMNToCSP.alpha) n)++"("++(index n)++")")) (ProcId ((name n)++"("++(index n)++")"))))

	      ix = (indexSetName n,List Set (indexSet (Right ss)))
	      us = ((lname n),[(LS ix)],(Indparcomp ((index n),(SName (indexSetName n))) (SName (((BPMNToCSP.alpha) n)++"("++(index n)++")")) (ProcId ((name n)++"("++(index n)++")"))))

	      pc = ((wname n),[],(Indparcomp ((index n),(SName (indexSetName n))) (SName (((BPMNToCSP.alpha) n)++"("++(index n)++")")) (ProcId ((name n)++"("++(index n)++")"))))
	      cnt = ((cpname n),[],Extern eds (Indextern ("i",List Set [ e | e <- es, (notElem e ees), (not . (isPrefixOf "aborts.")) e ]) (Prefix "i" (ProcId (cpname n)))))
	      us = ((lname n),[(LS ix),(LP cnt),(LP pc)],(Parinter (ProcId (wname n)) (List Set es) (ProcId (cpname n))))
	      ab = [ e | e <- es, (isPrefixOf "aborts.") e ]
	      eds = if null ab then (Indextern ("i",(List Set ees)) (Prefix "i" Skip)) 
		    else (Extern (Indextern ("i",(List Set ees)) (Prefix "i" Skip)) (Indextern ("i",(List Set ab)) (Prefix "i" Stop)))

	      cnt = ((cpname n),[],eds)
	      us1 = ((lname n)++"1",(Parinter (ProcId (wname n)) (List Set es) (ProcId (cpname n))))
	      us = ((lname n),ProcId (compress ((lname n)++"1")))


return CSP channels and data based on all specified CSP events

> getChannels :: [Event] -> [CSP.Channel]
> getChannels es = [nd,(TData (nub ch) [(DN "Node")]),(TData ["fin","aborts","error","except" ] [DS (List Set (map tail o))])]
>		   ++ (if null mg then [] else [(TData (nub mg) [(DN "Msg")])]) 
>	where nd = NData (getTranEvents es)
>	      (ch,n) = (unzip . (map (span (/='.')))) [ e | e <- es, elem '.' e, (not ((isPrefixOf "fin.") e || (isPrefixOf "aborts.") e || (isPrefixOf "error.") e || (isPrefixOf "except.") e || (isSuffixOf "init") e || (isSuffixOf "done") e)) ]
>	      (eh,o) = (unzip . (map (span (/='.')))) [ e | e <- es, ((isPrefixOf "fin.") e || (isPrefixOf "aborts.") e || (isPrefixOf "error.") e || (isPrefixOf "except.") e) ]
>	      (mg,m) = (unzip . (map (span (/='.')))) [ e | e <- es, ((isSuffixOf "init") e || (isSuffixOf "done") e) ]

return all trigger (transition) events

> getTranEvents :: [Event] -> [Event]
> getTranEvents es = filter (notElem '.') es

return all messageflows events

> getMsgEvents :: [Event] -> [Event]
> getMsgEvents es = filter test es
>	where test e = (isSuffixOf "init") e || (isSuffixOf "done") e

return message flows from events

> getMessage :: (BPMN.Type) -> Maybe Messageflow
> getMessage (Imessage m) = m
> getMessage (Smessage m) = m
> getMessage (Emessage m) = m

return a set of event corresponding the state's alphabet

> astate :: [StateSet] -> State -> [Event]
> astate ss (State t is os es rg im om rm am bm i) = trans++mges++wk++abo++end++sub++mp++xm++em++eerror++ierror
>	where trans = (map atrans (is++os++((snd.unzip)es)))++[ "except."++((show . invException . invIerror) e) | (e,s) <- es, isIerror e, invIerror e /= NoException ] 
>	      wk = if isWork t then [(awork t)] else []
>	      abo = if isAbort t then [ "aborts."++((show . invAbort) t) ] else []
>	      end = if isEnd t then [ "fin."++((show . invEnd) t) ] else []
>	      ierror = if isIerror t && invIerror t /= NoException then ["except."++((show . invException . invIerror) t)] else []
>	      eerror = if isEerror t 
>		      then (if (snd . invEerror) t /= NoException then  ["except."++((show . invException . snd . invEerror) t)] else []) 
>			   ++ [ "error."++((show . fst . invEerror) t) ] 
>		      else []
>	      sub = if (not.issubs) t then [] else (if (not.null) sp then concatMap (astate (head sp)) (level (head sp)) else error "undefined subprocess") 
>	      sp = [ ((snd.getSP) s) | s <- (delete (Atomic (getAtomic ss)) ss), (is == ((getIn.fst.getSP) s) && os == ((getOut.fst.getSP) s)) ]
>	      mp = mevents (State t is os es rg im om rm am bm i)
>	      mges = if (not.issubs) t && (not.isWork) t && (not.isImessage) t && (not.isSmessage) t then [] 
>		     else if (not.null) [ s | s <- (finds findP is ss), (getType s == Exgate)] then map amsg (om++rm++am++bm) 
>			  else if (isImessage) t && (isSmessage) t 
>			       then case getMessage t of 
>					Just x -> [amsg x]
>					Nothing -> []
>			       else map amsg (im++om++rm++am++bm)
>	      xm = if t /= Exgate then []
>		   else map amsg (concatMap getM (finds findS os ss))
>	      em = if (not.null) es then concat [ case invImessage e of 
>							Just x -> [amsg x] 
>							Nothing -> [] | (e,f) <- es, isImessage e ]

>		   else []
>	      getM s = if (not.isImessage.getType) s then getRec s 
>		       else case (getMessage.getType) s of
>					Just m -> [m] 
>					Nothing -> []

> mevents :: State -> [Event]
> mevents (State t is os es rg im om rm am bm i) =
>	if isMiseq t then [(atrans.fst.mstrans.fst4.invMiseq) t,(atrans.snd.mstrans.fst4.invMiseq) t]
>	else if isMiseqs t then [(atrans.fst.mstrans.fst4.invMiseqs) t,(atrans.snd.mstrans.fst4.invMiseqs) t]
>	     else if isMipar t then map atrans (concat [ [i,j] |  (i,j) <- mptrans (((fst4.invMipar) t),((getLoops.snd4.invMipar) t)) ])
>		  else if isMipars t then map atrans (concat [ [i,j] |  (i,j) <- mptrans (((fst4.invMipars) t),((getLoops.snd4.invMipars) t))])
>		       else []

return a CSP event corresponding to a transition

> atrans :: Transition -> Event
> atrans (g,l) = l

return a CSP event corresponding to a message flow

> amsg :: Messageflow -> Event
> amsg (m,c) = c++"."++m

> isWork :: BPMN.Type -> Bool
> isWork t = or [isTask t,isMiseq t,isMipar t]

 isWork t = or [isTask t,isBpmn t,isMiseq t,isMipar t,isMiseqs t,isMipars t]

return a CSP event corresponding to a state's "activity"

> awork :: BPMN.Type -> Event
> awork (Task a _) = ("starts."++a)
> awork (Miseq a _ _ _) = ("starts."++a)
> awork (Mipar a _ _ _) = ("starts."++a)
> awork (Bpmn a _) = ("starts."++a)
> awork (Miseqs a _ _ _) = ("starts."++a)
> awork (Mipars a _ _ _) = ("starts."++a)
> awork _ = error "awork\n"

> awk :: TaskName -> Event
> awk t = "starts."++t

return process corresponding the state

> rstate :: [StateSet] -> PName -> State -> (ProcVar,[Local],Process)
> rstate ss p s  
>	| (t == Start || isStime t) = (pn,[],(rtrans t os))
>	| isSmessage t = (pn,[],(Sequential rmp (rtrans t os)))
>	| isItime t  = (pn,[],(Sequential (Sequential (xgate is) (xgate os)) (ProcId pn)))
>	| isImessage t  = 
>		if (not.null) [ x | x <- (finds findP is ss), (getType x == Exgate)] 
>		then (pn,[],(Sequential (Sequential (xgate is) (xgate os)) (ProcId pn)))
>		else (pn,[],(Sequential (Sequential (xgate is) (Sequential rmp (xgate os)))) (ProcId pn))
>	| (t == Agate || t == Xgate) = (pn,[],(Sequential (rs ss s) (ProcId pn)))
>	| isTask t = (pn,[],(Sequential (rs ss s) (ProcId pn)))
>	| isBpmn t = (pn,[],Sequential (rs ss s) (ProcId pn))
>	| t == Exgate = 
>		let sucs = [mkpr x | x <- (finds findS os ss), (isImessage.getType) x || (not.null.getRec) x]
>		    mkpr w = if (isImessage.getType) w 
>			     then case (invImessage.getType) w of 
>					Just m -> Sequential (xgatem [m]) ((xgate.getIn) w)
>					Nothing -> ((xgate.getIn) w)
>			     else Sequential ((xgatem.getRec) w) ((xgate.getIn) w)
>		in if null sucs then (pn,[],(Sequential (rs ss s) (ProcId pn)))
>		   else (pn,[],(Sequential (Sequential (xgate is) (extern sucs)) (ProcId pn)))
>	where is = getIn s
>	      os = getOut s
>	      t = getType s
>	      pn = ((name p)++"("++sname(t,os)++")")
>	      smp = (seqcomp [(agatem.getSd) s,(agatem.getRep) s,(agatem.getAcc) s])
>	      rmp = if (isImessage.getType) s || (isSmessage.getType) s 
>		    then case (getMessage.getType) s of 
>			 	Just m -> xgatem [m]
>				Nothing -> Skip
>		    else if (null.getRec) s then Skip else (xgatem.getRec) s


	| isAbort t = (pn,[],(Sequential (rtrans t is) (Prefix ("aborts."++((show.invAbort) t)) Stop)))

> rs :: [StateSet] -> State -> Process
> rs ss (State t is os es rg im om rm am bm i) 
>	| (t == Agate || t == Xgate || t == Exgate) = (Sequential (rtrans t is) (rtrans t os))
>	| isTask t && null es =
>		if (not.null) [ s | s <- (finds findP is ss), (getType s == Exgate)] 
>		then (Sequential (rtrans t is) (Sequential (work t) (Sequential smp (rtrans t os))))
>		else (Sequential (rtrans t is) (Sequential rmp (Sequential (work t) (Sequential smp (rtrans t os)))))
>	| isTask t && (not.null) es =
>		if (not.null) [ s | s <- (finds findP is ss), (getType s == Exgate)] 
>		then (Sequential (rtrans t is) (exception t os es (Sequential (work t) (Sequential smp (rtrans t os)))))
>		else (Sequential (rtrans t is) (Sequential rmp (exception t os es (Sequential (work t) (Sequential smp (rtrans t os))))))
>	| isBpmn t && null es = 
>		if (not.null) [ s | s <- (finds findP is ss), (getType s == Exgate)] 
>		then (Sequential (rtrans t is) (Sequential (ProcId subn) (Sequential smp (rtrans t os))))
>		else (Sequential (rtrans t is) (Sequential rmp (Sequential (ProcId subn) (Sequential smp (rtrans t os)))))
>	| isBpmn t && (not.null) es =
>		if (not.null) [ s | s <- (finds findP is ss), (getType s == Exgate)] 
>		then (Sequential (rtrans t is) (exception t os es (Sequential (ProcId subn) (Sequential smp (rtrans t os)))))
>		else (Sequential (rtrans t is) (Sequential rmp (exception t os es (Sequential (ProcId subn) (Sequential smp (rtrans t os))))))
>	where subn = (lname.fst.invBpmn) t
>	      smp = (seqcomp [(agatem om),(agatem rm),(agatem am)])
>	      rmp = if null im then Skip else (xgatem im)


	if True then (error.show) (Sequential (rtrans t is) (Sequential (ProcId subn) (Sequential smp (rtrans t os)))) else

	| isBpmn t && null es = Sequential (rtrans t is) (Sequential (work t) (Sequential (ProcId subn) (rtrans t os)))
	| isBpmn t && (not.null) es = Sequential (rtrans t is) (exception t os es (Sequential (work t) (Sequential (ProcId subn) (rtrans t os))))

> exception :: BPMN.Type -> [Transition] -> [(BPMN.Type,Transition)] -> Process -> Process
> exception t os es p =
>	(Parinter (Interrupt p (except 1)) 
>		  (List Set ((map atrans os)++((CSP.alpha) (except 2))))
>		  (Extern (except 2) (rtrans t os)))
>	where except i = extern [ if isImessage e1 
>				  then case invImessage e1 of 
>					Just m -> Prefix (amsg m) (Prefix (atrans e2) Skip) 
>			  		Nothing -> (Prefix (atrans e2) Skip) 
>				  else if i == 2 && isIerror e1 && invIerror e1 /= NoException 
>				       then Prefix ("except."++((show . invException . invIerror) e1)) (Prefix (atrans e2) Skip)
>				       else Prefix (atrans e2) Skip | (e1,e2) <- es ]

return process corresponding multiple instance state

> rmipar :: [StateSet] -> PName -> State -> (ProcVar,[Local],Process)
> rmipar ss p (State t is os es rg im om rm am bm i) = 
>	if null es then (pn,[],(Sequential (Hide (Sequential (xgate is) (Parinter mtasks miset con)) hide) (ProcId pn)))
>	else (pn,[],(Hide (Sequential (xgate is) (Sequential (exception t os es (Parinter mtasks miset con)) (ProcId pn))) hide))
>	where pn = ((name p)++"("++sname(t,os)++")")
>	      (nm,ni) = (if isMipar t then ((fst4.invMipar) t,(getLoops.snd4.invMipar) t) else ((fst4.invMipars) t,(getLoops.snd4.invMipars) t))
>	      it = if isMipar t then (Task nm StandardT) else (Bpmn nm SequenceB)
>	      lp = if isMipar t then (snd4.invMipar) t else (snd4.invMipars) t
>	      mt = mptrans (nm,ni)
>	      ms = map atrans (union os ((snd.unzip) es))
>	      miset = List Set (ms++(map atrans ((fst.unzip) mt ++ (snd.unzip) mt)))
>	      con = if isFix lp then (cn mt) else Extern (xgate os) (cn mt) 
>	      mtasks = parinter (List Set ms) [ Sequential (rs ss (State it [i] [j] [] rg [] [] [] [] [] 1)) (xgate os)  | (i,j) <- mt ]
>	      cn ts = if length ts > 1 then (ic ts ((not.isFix) lp)) else (ec ts)
>	      ic ts b = extern [ Prefix (atrans i) (Inter (Prefix (atrans j) Skip) 
>			          	           (if b then Extern (xgate os) (cn (delete (i,j) ts)) 
>						    else (cn (delete (i,j) ts)))) | (i,j) <- ts ]
>	      ec ts = extern [ Prefix (atrans i) (Prefix (atrans j) (xgate os)) | (i,j) <- ts ]
>	      hide = List Set (map atrans ((fst.unzip) mt ++ (snd.unzip) mt))

	      cn ts = Extern (if length ts > 1 then (ic ts True) else (ec ts))
			     (Extern (xgate os) (Extern (ec ts) (ic ts False)))
	      ic ts b = extern [ Prefix (atrans i) (Inter (Prefix (atrans j) Skip) 
				 		          (if b then (xgate os) else (cn (delete (i,j) ts)))) | (i,j) <- ts ]
	      ec ts = if null ts then error "es" else extern [ Prefix (atrans i) (Prefix (atrans j) (xgate os)) | (i,j) <- ts ]
	      hide = List Set (map atrans ((fst.unzip) mt ++ (snd.unzip) mt))

> mptrans :: (String,Int) -> [(Transition,Transition)]
> mptrans (_,0) = []
> mptrans (i,n) = [((True,(i++"i"++(show n))),(True,(i++"o"++(show n))))]++(mptrans (i,n-1))

> rmiseq :: [StateSet] -> PName -> State -> (ProcVar,[Local],Process)
> rmiseq ss p (State t is os es rg im om rm am bm i) =

	if null (im ++ om ++ rm ++ am ++ bm) 
	then let sq1 x = Extern (if isFix lp then Stop else (xgate os))
		    	    	(if x == 1 then (Sequential (Hide (rs ss (State it [t1] [t2] [] rg [] [] [] [] [] 1)) hide) (xgate os))
		     	     	 else (Sequential (Hide (rs ss (State it [t1] [t2] [] rg [] [] [] [] [] 1)) hide) (sq1 (x-1))))
	     in if null es then (pn,[],(Sequential (Sequential (xgate is) (sq1 n)) (ProcId pn)))
		else (pn,[],(Sequential (Sequential (xgate is) (exception t os es (sq1 n))) (ProcId pn)))
	else
	
> 	     if null es then (pn,[],(Sequential (Sequential (xgate is) (Hide (Parinter (cq n) (List Set sy) (sq n)) hide)) (ProcId pn)))
>	     else (pn,[],(Sequential (Hide (Sequential (xgate is) (exception t os es (Parinter (cq n) (List Set sy) (sq n)))) hide) (ProcId pn)))
>	where pn = ((name p)++"("++sname(t,os)++")")
>	      it = if isMiseq t then (Task ((fst4.invMiseq) t) StandardT) else (Bpmn ((fst4.invMiseqs) t) SequenceB)
>	      lp = if isMiseq t then (snd4.invMiseq) t else (snd4.invMiseqs) t
>	      ((t1,t2),n) = if isMiseq t then ((mstrans.fst4.invMiseq) t,getLoops lp) else ((mstrans.fst4.invMiseqs) t,getLoops lp)
>	      sy = if (not.null) [ s | s <- (finds findP is ss), (getType s == Exgate)] 
>		   then (map amsg (om++rm++am++bm))++(((map atrans).nub) (os ++((snd.unzip) es) ++ [t1,t2]))
>		   else (map amsg (im++om++rm++am++bm))++(((map atrans).nub) (os ++((snd.unzip) es) ++ [t1,t2]))
>	      cq x = (Sequential (if x == n then Skip else (Prefix (atrans t2) Skip))
>				 (Extern (if (isFix lp) then Stop else (xgate os))
>					 (if x > 1 then (Prefix (atrans t1) (Sequential smp (cq (x-1)))) 
>			      		  else (Prefix (atrans t1) (Sequential smp (Prefix (atrans t2) (xgate os)))))))

					 (if (x == n || isFix lp) then Stop else (xgate os))

>	      sq x = Extern (if isFix lp then Stop else (xgate os))
>		      	    (if x == 1 then (Sequential (rs ss (State it [t1] [t2] [] rg [] om rm am [] 1)) (xgate os))
>		     	     else (Sequential (rs ss (State it [t1] [t2] [] rg [] om rm am [] 1)) (sq (x-1))))
>	      hide = List Set (map atrans [t1,t2])
>	      smp = (seqcomp [(agatem om),(agatem rm),(agatem am)])

> mstrans :: String -> (Transition,Transition)
> mstrans i = ((True,(i++"i")),(True,(i++"o")))

return a process representing either a parallel split/join or a exclusive or splie/join

> rtrans :: BPMN.Type -> [Transition] -> Process  
> rtrans Agate ts = agate ts 
> rtrans _ ts = xgate ts

return a partial interleaving of processes ps with the shared interface es

> parinter :: Events -> [Process] -> Process
> parinter _ (p:[]) = p
> parinter es (p:ps) = Parinter (inter [p]) es (inter ps)

return a sequential composition of processes ps

> seqcomp :: [Process] -> Process
> seqcomp (p:[]) = p
> seqcomp (p:ps) = Sequential p (seqcomp ps)

return the interleaving of processes ps

> inter :: [Process] -> Process
> inter [] = Skip
> inter (p:[]) = p
> inter (p:ps) = Inter (inter [p]) (inter ps)

> agatem :: [Messageflow] -> Process
> agatem = inter . (map tp) where tp m = (Prefix (amsg m) Skip)

> agate :: [Transition] -> Process
> agate = inter . (map tp) where tp t = (Prefix (atrans t) Skip)

return the extern choice over processes ps

> extern :: [Process] -> Process
> extern [] = error "extern"
> extern (p:[]) = p
> extern (p:ps) = Extern (extern [p]) (extern ps)

> xgatem :: [Messageflow] -> Process
> xgatem ms = if null ms then error "xgatem" 
>	      else (extern . (map tp)) ms where tp m = (Prefix (amsg m) Skip)

> xgate :: [Transition] -> Process
> xgate ts = if null ts then error "xgate" 
>	     else (extern . (map tp)) ts where tp t = (Prefix (atrans t) Skip)

> xgate1 :: [Transition] -> Process
> xgate1 = extern . (map tp) where tp t = (Prefix (atrans t) Stop)

> work :: BPMN.Type -> Process
> work t = if (isWork t) then (Prefix (awork t) Skip) else Skip

 coordination :: (PName,[StateSet]) -> Script
 coordination (x,y) = merge cc (Script [] [(NData ["cycle"])] [cp] [] [])
	where cc = coordSignals (x,y)
	      cp1 = ((cname x)++"1",[],(coordProcess (x,y)))
	      run = ("run",[],Extern (Indextern ("i",SName "Internal") (Prefix "i" (ProcId "run"))) ips) 
	      cp = ((cname x),[(LP run),(LP cp1)], Hide (ProcId ((cname x)++"1")) (SName "Internal")) 
	      end = nub ((map (show.invEnd.getType) . filter (isEnd.getType)) (getAtomic y)) 
	      abt = nub ((map (show.invAbort.getType) . filter (isAbort.getType)) (getAtomic y))
	      ips = if null abt then (Indextern ("i",(List Set end)) (Prefix "fin.i" Skip))
		    else (Extern (Indextern ("i",(List Set end)) (Prefix "fin.i" Skip)) (Indextern ("i",(List Set abt)) (Prefix "aborts.i" Stop)))

> coordination :: (PName,[StateSet]) -> Script
> coordination (x,y) = (Script [] [TData ["finish","cancel","delayed","interrupted"] [DN "Signal"]] [cp] [("Signal",(List Set ee))] [])
>	where cp1 = ((cname x)++"1",[],(coordProcess (x,y)))
>	      run = ("run(S)",[],Extern (Indextern ("i",SName "S") (Prefix "i" (ProcId "run(S)"))) ips)

	      run = ("run",[],Extern (Indextern ("i",SName "Internal") (Prefix "i" (ProcId "run"))) ips)
	      
>	      se = [ (atrans.head.getOut) s | s <- allstates y, not ((isEnd.getType) s || (isAbort.getType) s) ]
>	      me = (concatMap mevents (filter isMults (allstates y)))
>	      e = [ "fin."++((show.invEnd.getType) s) | s <- allstates y, (isEnd.getType) s ]
>	      a = [ "aborts."++((show.invAbort.getType) s) | s <- allstates y, (isAbort.getType) s ]	      
>	      ee = [ (atrans e) | s <- allstates y, (t,e) <- getError s] ++ se ++ me ++ e ++ a
>	      ai = nub (concatMap internal [ s | s <- allstates y, isTimed s ])
>	      end = nub ((map (show.invEnd.getType) . filter (isEnd.getType)) (getAtomic y)) 
>	      abt = nub ((map (show.invAbort.getType) . filter (isAbort.getType)) (getAtomic y))

	      locals = [(LS ("Internal",(List Set ai))),(LS ("in1",SName ("union(Internal,"++(showSet (List Set ((map ((++) "fin.") end)++(map ((++) "aborts.") abt))))++")"))),(LP run),(LP cp1)]
	      
>      	      locals = [(LS ("Internal",(List Set ai))),(LS ("ses",(List Set ((map ((++) "fin.") end)++(map ((++) "aborts.") abt))))),(LP run),(LP cp1)]     
>	      cp = ((cname x),locals, Hide (ProcId ((cname x)++"1")) (SName "Internal"))
>	      ips = if null abt then (Indextern ("i",(List Set end)) (Prefix "fin.i" Skip))
>		    else (Extern (Indextern ("i",(List Set end)) (Prefix "fin.i" Skip)) (Indextern ("i",(List Set abt)) (Prefix "aborts.i" Stop)))


 	      cp1 = ((cname x)++"1",[],(Interrupt (coordProcess (x,y)) ips))
	      run = ("run(S)",[],Indextern ("i",SName "S") (Prefix "i" (ProcId "run(S)")))
	      cpcp = ((cname x)++"CP", Hide (ProcId ((cname x)++"1")) (List Set (acoord y)))
	      cpcp = ((cname x)++"CP",[], Hide (ProcId ((cname x)++"1")) (SName "Internal"))
	      cp = ((cname x),ProcId (compress ((cname x)++"CP")))
	      cp1 = ((cname x)++"1", (coordProcess (x,y)))
	      cp1 = ((cname x)++"1", (Interrupt (coordProcess (x,y)) ips))

 acoord :: [StateSet] -> [Event]
 acoord ss = (nub.concat) [ internal s | s <- allstates ss, isTimed s ]

> coordProcess :: (PName,[StateSet]) -> Process
> coordProcess (n,ss) = stable 0 (timer ss [] []) ss [] bg [] 
>	where bg = [s | s <- (getAtomic ss), isStarts s]

 coordProcess (n,ss) = (extern . (map bp)) bg
	where bg = [s | s <- (getAtomic ss), (isStime.getType) s || getType s == Start]
	      bp b = Sequential ((xgate.getOut) b) (stable 0 (timer ss) ss [ k | k <- (xt b), (not.isTimed) k || isSubs k] [ k | k <- (xt b), isTimed k && (not.isSubs) k])
	      xt b = (finds findS (getOut b) ss)

 coordSignals :: (PName,[StateSet]) -> Script
 coordSignals (n,ss) = Script [] [TData ["finish","cancel","delayed","interrupted"] [DN "Signal"]] [] [("Signal",(List Set ee)),("Internal",(List Set ai))] []
	where se = [ (atrans.head.getOut) s | s <- allstates ss, not ((isEnd.getType) s || (isAbort.getType) s) ]
	      e = [ "fin."++((show.invEnd.getType) s) | s <- allstates ss, (isEnd.getType) s ]
	      a = [ "aborts."++((show.invAbort.getType) s) | s <- allstates ss, (isAbort.getType) s ]
	      me = (concatMap mevents (filter isMults (allstates ss)))
	      ee = [ (atrans e) | s <- allstates ss, (t,e) <- getError s] ++ se ++ me ++ e ++ a
	      ai = nub (concatMap internal [ s | s <- allstates ss, isTimed s ])

DList "Signal" ["placeholder"]

> wait :: State -> Event
> wait s = "delayed."++((atrans.head.getOut) s)

> fin :: State -> Event
> fin s = "finish."++((atrans.head.getOut) s)

> can :: State -> Event
> can s = if (isEnd.getType) s  
>	  then "cancel.fin."++((show.invEnd.getType) s)
>	  else if (isAbort.getType) s  
>	       then "cancel.aborts."++((show.invAbort.getType) s)
>	       else "cancel." ++((atrans.head.getOut) s)

> intt :: State -> [(Transition,Event)]
> intt s = [ (e,"interrupted."++(atrans e)) | (t,e) <- getError s ]

> internal :: State -> [Event]
> internal s =
>	let t = getType s
> 	in (if isTasks s then [wait s] else []) ++ (map snd (intt s)) ++ [fin s,can s]
>	    ++ (if isMiseq t then [ ((("finish."++).atrans.snd.mstrans.fst4.invMiseq) t),((("delayed."++).atrans.snd.mstrans.fst4.invMiseq) t) ]
>		else (if isMiseqs t then [ ((("finish."++).atrans.snd.mstrans.fst4.invMiseqs) t),((("delayed."++).atrans.snd.mstrans.fst4.invMiseqs) t) ]
>		      else (if isMipar t 
>		      	    then (map (("finish."++).atrans) ((snd.unzip.mptrans) ((fst4.invMipar) t,(getLoops.snd4.invMipar) t)))
>			    	 ++(map (("delayed."++).atrans) ((snd.unzip.mptrans) ((fst4.invMipar) t,(getLoops.snd4.invMipar) t)))
>		      	    else (if isMipars t 
>			    	  then (map (("finish."++).atrans) ((snd.unzip.mptrans) ((fst4.invMipars) t,(getLoops.snd4.invMipars) t)))
>			          	++(map (("delayed."++).atrans) ((snd.unzip.mptrans) ((fst4.invMipars) t,(getLoops.snd4.invMipars) t)))
>			    	  else []))))

> isInternal :: Event -> Bool
> isInternal e =  (isPrefixOf "finish." e || isPrefixOf "cancel." e || isPrefixOf "delayed." e || isPrefixOf "interrupted." e)

> finds :: ([State] -> Transition -> [State]) -> [Transition] -> [StateSet] -> [State]
> finds f ts [] = []
> finds f ts ((Atomic s):ss) = (concatMap (f s) ts)++(finds f ts ss)
> finds f ts ((SubProcess s ss):sss) = (concatMap (f [s]) ts)++(finds f ts ss)++(finds f ts sss)

return immediate succeding states

> findS :: [State] -> Transition -> [State]
> findS ss t = filter ((elem t).getIn) ss

find immediate preceding states

> findP :: [State] -> Transition -> [State]
> findP ss t = [ x | x <- ss, (elem t (union (getOut x) ((snd.unzip.getError) x) ))]

minimise concurrency on untimed enactment

> minimise :: [State] -> [Transition]
> minimise ss = 
>	let one = [ (head.getOut) s | s <- ss, getType s /= Xgate && getType s /= Exgate && (not.null.getOut) s ]
>	in [ i | s <- ss, (getType s == Xgate || getType s == Exgate), i <- getOut s] 
>	   ++ (if null one then [] else [head one]) 

branch out all untimed states until diagram is time stable
(argument mi memorizing multiple instance subprocesses (sequential)) 

> stable :: Int -> (Int -> [(String,Int)] -> [State] -> Process) -> [StateSet] -> [(String,Int)] -> [State] -> [State] -> Process
> stable cn f ss ms st rn =

	if (and [ (isEnd.getType) s && (null (parentstate ss s)) | s <- st ] && null rn) then Skip
	else if (and [ (isAbort.getType) s && (null (parentstate ss s)) | s <- st ] && null rn) then Stop
	
	else if ((not.null) st && or [ (isAbort.getType) s && (null (parentstate ss s)) | s <- st ]) then Stop
	
>	if ((not.null) st && and [ ((isEnd.getType) s || (isAbort.getType) s) && (null (parentstate ss s)) | s <- st ]) 
>	then extern ([ (Prefix ((((++) "fin.").show.invEnd.getType) s) Skip) | s <- st, (isEnd.getType) s ]
>		     ++[ (Prefix ((((++) "aborts.").show.invAbort.getType) s) Stop) | s <- st, (isAbort.getType) s ])
>	else if ((not.null) st && or [ (isAbort.getType) s && (null (parentstate ss s)) | s <- st ]) 
>	     then extern [ (Prefix ((((++) "aborts.").show.invAbort.getType) s) Stop) | s <- st, (isAbort.getType) s ]
>            else if (nrun == st) then (f cn ms rn) else (if null ts then error "stable" else extern sp)  
>	where ts = (map atrans ((minimise [ s | s <- st, notElem s nrun])++ends))
> 	      sp = (map mp ts)

	      ts = (map atrans ([ t | s <- st, notElem s nrun, t <- (union (getOut s) ((snd.unzip.getError) s))]++ends))
	      
>	      ends = [ t | s <- st, notElem s nrun, (isEnd.getType) s && ((not.null) (parentstate ss s)), 
>			   t <- ((if (not.isMiseqs.getType.fst.getSP.head.(parentstate ss)) s then []
>			   	  else if head [n | (m,n) <- ms, m == (fst4.invMiseqs.getType.fst.getSP.head.(parentstate ss)) s] <= 0 then [] 
>				       else ((concatMap getOut).(filter isStarts).getAtomic.snd.getSP.head.(parentstate ss)) s)
>			   	  ++(if (isMiseqs.getType.fst.getSP.head.(parentstate ss)) s 
>				     then (if (isFix.snd4.invMiseqs.getType.fst.getSP.head.(parentstate ss)) s && head [n | (m,n) <- ms, m == (fst4.invMiseqs.getType.fst.getSP.head.(parentstate ss)) s] > 0
>				     	  then [] else ((getOut.fst.getSP.head.(parentstate ss)) s))
>				     else ((getOut.fst.getSP.head.(parentstate ss)) s))) ]
>	      nan = [ s | s <- st, (getType s == Agate),(length.getOut.head.sim) s < (length.getIn.head.sim) s, (length.getIn.head.sim) s > 1]
>	      nrun = union [ s | s <- nan, or (map (flip (isPre ss) ((head.sim) s)) srn)] nrun1
>	      nrun1 = nub [ s | s <- nan, t <- [ (head.sim) x | x <- (st++rn), not (elem x nan)], y <- (intersect (findPs ss s) (findPs ss t)), getType y /= Agate]  
>	      srn = st++rn++[ (fst.getSP) p | s <- (st++rn), p <- enclosestate ss s ]
>	      sim x = if (isItime.getType) x && (null.getIn) x 
>		      then [ s | s <- allstates ss, or [ t == (head.getOut) x  | t <- (snd.unzip.getError) s ] ]
>		      else [ s | s <- allstates ss, subset (getIn x) (getIn s) && (sameType x s) ]
>	      rsim y = [ s | s <- st, subset (getIn s) (getIn y) && (if isStarts y then isStarts s && (getOut s == getOut y) else (not.isStarts) s) ]
>	      isEbMiseqs s =  (not.null.(parentstate ss)) s && (isMiseqs.getType.fst.getSP.head.(parentstate ss)) s
>	      mp e = let suc = [ s | s <- allstates ss, notElem s nrun, elem e (map atrans (getIn s))]
>			 pre = let pp = [ s | s <- allstates ss, notElem s nrun, elem e (map atrans (union (getOut s) ((snd.unzip.getError) s)))]
>				   ip = [ s | s <- st, ((not.null) (parentstate ss s)) && ((fst.getSP.head) (parentstate ss s)) == (head pp)]
>			       in if (null pp) || (not.isSubs.head) pp then pp 
>				  else if null ip then pp else ip
>			 pae = if null pre || (not.isEnd.getType.head) pre || (null.(parentstate ss).head) pre then [] 
>			       else let (p,c) = (getSP.head.(parentstate ss).head) pre
>				    in [ s | s <- (st++rn), or [ s `approx` be | b <- ([p]++(allstates c)), be <- sep b] ] 
>			 o = [ t | t <- (union ((getOut.head) pre) ((snd.unzip.getError.head) pre)), atrans t == e ]
>			 ac = ((not.null) pre && (getType.head) pre == Agate && (length.getOut.head.rsim.head) pre  > 1)
>			 nm = if (isMiseqs.getType.head) pre then rmm ms ((fst4.invMiseqs.getType.head) pre)
>			      else if not ((isStarts.head) pre && (isEbMiseqs.head) pre) then ms
>			      	   else subm ms ((fst.(getPN ss).head) pre)
>			 me = if not ((isStarts.head) pre && (not.null.(parentstate ss).head) pre) then []
>			      else [ s | s <- st, (isEnd.getType) s && ((not.null) (parentstate ss s)) && (parentstate ss s) == (parentstate ss (head pre))]

			 rd = or [ isStarts r &&  isEbMiseqs r && ((inm ms).fst.(getPN ss)) r 
				   && or [ y == (snd.(getPN ss)) r | (x,y) <- ms, x == (fst.(getPN ss)) r ] | r <- rn ]
				   
>		     in (if (not.null) suc && (isSubs.head) suc 
>			 then let bs = [ i | i <- getAtomic (getSubs (head suc) ss), isStarts i ]
>				  bp s = if (isStime.getType) s
>				 	 then let nnms = if (not.isMiseqs.getType.head) suc then nms 
>						         else subm nms ((fst4.invMiseqs.getType.head) suc)
>					      in if ac then (stable cn f ss nnms [ s | s <- (union [ s | s <- st, (notElem s ((rsim.head) pre))] [(rmtrans ((head.rsim.head) pre) (head o))]), not (elem s pae)]  [ s | s <- (union rn [s]), not (elem s pae)] )
>					         else (stable cn f ss nnms [ s | s <- [ s | s <- st, (notElem s ((rsim.head) pre))], not (elem s pae)] [ s | s <- (union rn [s]), not (elem s pae)] )
>					 else if ac then (stable cn f ss nms [ s | s <- (union (union [ s | s <- st, (notElem s ((rsim.head) pre))] [(rmtrans ((head.rsim.head) pre) (head o))]) [s]), not (elem s pae)] [ s | s <- rn, not (elem s pae)])
>					      else (stable cn f ss nms [ s | s <- (union [ s | s <- st, (notElem s ((rsim.head) pre))] [s]), not (elem s pae)] [ s | s <- rn, not (elem s pae)])
>				  nms = if (not.isMiseqs.getType.head) suc then ms
>				        else ms++[((fst4.invMiseqs.getType.head) suc,(getLoops.snd4.invMiseqs.getType.head) suc)]
>			      in Prefix e (extern (map bp bs))

				 if (not.isMiseqs.getType.head) suc then Prefix e (extern (map bp bs)) 
				 else (Prefix e (Extern (extern (map bp bs)) 
					(if ac then (stable cn f ss ms [ s | s <- (union (union [ s | s <- st, (notElem s ((rsim.head) pre))] [(rmtrans ((head.rsim.head) pre) (head o))]) suc), not (elem s pae)] [ s | s <- rn, not (elem s pae)])
					 else (stable cn f ss ms [ s | s <- (union [ s | s <- st, (notElem s ((rsim.head) pre))] suc), not (elem s pae)] [ s | s <- rn, not (elem s pae)]))))
					 
>			 else if ((not.null) pre && (isStarts.head) pre && (isEbMiseqs.head) pre && ((inm ms).fst.(getPN ss).head) pre)   
>			      then if (isStime.getType.head) pre then (stable cn f ss nm [ s | s <- st, (notElem s me)] (union rn pre))
>			 	   else if (not.null) suc && (isTimed.head) suc
>				        then Prefix e (stable cn f ss nm [ s | s <- st, (notElem s (me++pre))] (union rn suc))
>				        else Prefix e (stable cn f ss nm (union [ s | s <- st, (notElem s (me++pre))] suc) rn)
>			      else if (not.null) suc && (isTimed.head) suc 
>				   then if ac then Prefix e (stable cn f ss ms [ s | s <- (union [ s | s <- st, (notElem s ((rsim.head) pre))] [(rmtrans ((head.rsim.head) pre) (head o))]), not (elem s pae)] [ s | s <- (union rn suc), not (elem s pae)])
>				        else Prefix e (stable cn f ss ms [ s | s <- [ s | s <- st, (notElem s ((rsim.head) pre))], not (elem s pae)] [ s | s <- (union rn suc), not (elem s pae)])
>		        	   else if ac then Prefix e (stable cn f ss ms [ s | s <- (union (union [ s | s <- st, notElem s ((rsim.head) pre)] [(rmtrans ((head.rsim.head) pre) (head o))]) suc), not (elem s pae)] [ s | s <- rn, not (elem s pae)])
>				        else Prefix e (stable cn f ss ms [ s | s <- (union [ s | s <- st, notElem s ((rsim.head) pre)] suc), not (elem s pae)] [ s | s <- rn, not (elem s pae)]))

> sameType :: State -> State -> Bool
> sameType x y =
>	let s = getType x
>	    t = getType y
>	in (isStime s && isStime t) || (isItime s && isItime t) || (isTask s && isTask t) || (isBpmn s && isBpmn t) ||
>	   (isMiseq s && isMiseq t) || (isMiseqs s && isMiseqs t) || (isMipar s && isMipar t) || (isMipars s && isMipars t) || 
>	   (s == Agate && t == Agate) || (s == Exgate && t == Exgate) || (s == Xgate && t == Xgate) || (s == Start && t == Start) ||  
>	   (isSmessage s && isSmessage t) || (isImessage s && isImessage t) || (isEnd s && isEnd t) || (isAbort s && isAbort t)

> inm :: [(String,Int)] -> String -> Bool
> inm [] _ = False
> inm ((n,i):xs) m = if m == n then True else (inm xs m)
		
> subm :: [(String,Int)] -> String -> [(String,Int)]
> subm [] _ = []
> subm ((n,i):xs) m = if m == n then ((n,i-1):xs)
>		      else [(n,i)]++(subm xs m)

> getm :: [(String,Int)] -> String -> (String,Int)
> getm [] m = error ("getM No "++m)
> getm ((n,i):xs) m = if m == n then (n,i) else getm xs m

> rmm :: [(String,Int)] -> String -> [(String,Int)]
> rmm [] _ = []
> rmm ((n,i):xs) m = if m == n then xs
>		      else [(n,i)]++(rmm xs m)

get parent state's name and iteration (default 0)

> getPN :: [StateSet] -> State -> (String,Int)
> getPN ss s = let (a,b,c,d) = (invMiseqs.getType.fst.getSP.head.(parentstate ss)) s in (a,getLoops b)


> scomp :: Event -> Process
> scomp e = Prefix e Skip

> subset :: (Eq a) => [a] -> [a] -> Bool
> subset xs ys = all (`elem` ys) xs

> rmtrans :: State -> Transition -> State
> rmtrans (State t i o e r m n rm am bm q) rt = (State t i (delete rt o) e r m n rm am bm q)

Checks if s is a timed

> isTimed :: State -> Bool
> isTimed s = let t = getType s 
>	      in not (or [isEnd t, isAbort t, t == Start, t == Agate, t == Xgate, t == Exgate, isImessage t , isSmessage t])

Checks if s is a multiple instances state (mipar,miseq,miseqs,mipars)

> isMults :: State -> Bool
> isMults s = let t = getType s 
>	      in (or [isMiseq t, isMiseqs t, isMipar t, isMipars t])


returns state s's enclosed states

> getSubs :: State -> [StateSet] -> [StateSet]
> getSubs s [] = []
> getSubs s ss = 
>	let test (SubProcess t tt) = t == s
>	    test _ = False
>	in case (find test ss) of
>		Just (SubProcess t tt) -> tt
>		Nothing -> concatMap (getSubs s) ((map (snd.getSP).tail) ss)

Checks if p precedes u

> isPre :: [StateSet] -> State -> State -> Bool
> isPre ss p u = or [p `approx` r | r <- pre] || or (map (isPre ss p) pre)
>	where pre = finds findP (getIn u) ss

find all states that precede u

> findPs :: [StateSet] -> State -> [State]
> findPs ss u = 
>	let ps = finds findP (getIn u) ss
>	in if (null.getIn) u then [] else nub (ps ++ (concatMap (findPs ss) ps))  

> isStarts :: State -> Bool
> isStarts s = getType s == Start || (isStime.getType) s || (isSmessage.getType) s

returns u's enclosing state to the top level

> enclosestate :: [StateSet] -> State -> [StateSet]
> enclosestate ss s = 
>	let ps = parentstate ss s
>	    pps = parentstate ss ((fst.getSP.head) ps)
>	in if null ps then [] else ps ++ pps

returns u's enclosing state

> parentstate :: [StateSet] -> State -> [StateSet]
> parentstate ss s = case parentState ss s of
>			Just k -> [k]
>			Nothing -> []

> parentState :: [StateSet] -> State -> (Maybe StateSet)
> parentState [] _ = Nothing
> parentState ss u =
>	let subs = if (not.null.getAtomic) ss then (delete (Atomic (getAtomic ss)) ss) else ss
>	in if or [ s `approx` u | s <- (getAtomic ss) ] && (not.null.getAtomic) ss then Nothing 
>	   else if null subs then Nothing 
>		else case (parentState1 u) (head subs) of
>			Just a -> Just a
>			Nothing -> parentState (tail subs) u

> parentState1 :: State -> StateSet -> (Maybe StateSet)
> parentState1 u (SubProcess s ss) = 
>	if isIn u (SubProcess s ss) 
>	then if or [ x `approx` u | x <- (level ss) ] then Just (SubProcess s ss)
>	     else parentState (delete (Atomic (getAtomic ss)) ss) u
>	else Nothing


 parentState :: [StateSet] -> State -> (Maybe StateSet)
 parentState [] _ = Nothing
 parentState ((Atomic s):ss) u = if elem u s then Nothing else parentState ss u
 parentState ((SubProcess s ss):sss) u = 
		if s /= u && (isIn u (SubProcess s ss)) then Just (SubProcess s ss)
		else parentState sss u
		
> isIn :: State -> StateSet -> Bool
> isIn u (Atomic ss) = or [ s `approx` u | s <- ss ]
> isIn u (SubProcess s ss) = (s == u) || (or . (map (isIn u))) ss  

-- Ordering timed sequences

-- all ws's minrange have to be zero

 timer :: [StateSet] -> [State] -> [State] -> Int -> [(String,Int)] -> [State] -> Process
 timer ss bs ws cn mis su = 
	let ns = [ s | s <- su, (not.or) [ s `approx` a | a <- (union bs ws) ] ]
	    ub = [ b | b <- bs, not ((isMiseq.getType) b && or [ (fst.invTask.getType) w == (fst4.invMiseq.getType) b | w <- ws, (isTask.getType) w ]) ]
	    nws1 = if null ws then bs else update1 ws ub
	    nws = union ns (union nws1 [ b | b <- bs, not (elem b ub) ])
	in if null bs && null ws then timer1 ss cn mis su 
	   else timer1 ss cn mis nws

 update1 :: [State] -> [State] -> [State]
 update1 ws bs = let sb = if (getTime.head.order) bs >  
	 	  in [(update ((fst.getRange) b - (fst.getRange) sb) ((snd.getRange) b - (fst.getRange) sb) b)  | b <- bs]
		     ++ [ (update ZERO ((snd.getRange) w - (fst.getRange) sb) w) | w <- ws]

 timer1 :: [StateSet] -> Int -> [(String,Int)] -> [State] -> Process
 timer1 ss cn ms st =
	let ss1 = (order.nub.concat) (union (map sep st) (map (subexc ss) st))
	    ts = map time ss1
	    num = length (filter (== head ts) ts)
	    cur = take num ss1
	    ss2 = [ s | s <- st, ((notElem s cur) && (((not.isMiseq.getType) s) || 
				 (null [ c | c <- allstates ss, (((isMiseq.getType) s) && (c `approx` s) &&  
								((snd4.invMiseq.getType) s) < ((snd4.invMiseq.getType) c)) ] ))) ]
	    ns = map (subt (head ts)) ss2 
	in trun cn ss ms ns (map (subt (head ts)) cur)

	   cur = map (subt (head ts)) (take num ss1)
	   
> timer :: [StateSet] -> [State] -> [State] -> Int -> [(String,Int)] -> [State] -> Process
> timer ss bs ws cn ms su =
>	let cbs = [ s | s <- bs, or [ s `approx` a | a <- su ] ]
>	    cws = [ s | s <- ws, or [ s `approx` a | a <- su ] ]
>	    new = [ s | s <- su, (not.or) [ s `approx` a | a <- (union cbs cws) ] ]
>	    olds =  order [ s | s <- (nmiseq (cws++cbs) cbs)]
>	    delays = order2 cws
>	    fs = if (not.null) delays && (not.null) olds
>		 then if (time2.head) delays < (time.head) olds then (time2.head) delays
>		      else (time.head) olds
>		 else if (not.null) olds then (time.head) olds
>		      else (time2.head) delays
>	    nws = (order.nub) (union (union (map (subt fs) olds) [ update ZERO ZERO s | s <- cws, (snd.getRange) s <= fs ])
>			      [ update ZERO (((snd.getRange) s) - fs) s | s <- cws, (snd.getRange) s > fs ])
>	    ss2 = if null cbs && null cws 
>		  then concat (union (map sep su) (map (subexc ss) su)) 
>		  else union (union (concat (union (map sep new) (map (subexc ss) new))) nws) [ s | s <- cbs, notElem s olds ]
>	    (qs,is) = unzip ([ (splitseq ss c) | c <- (nmiseq ss2 ss2), (isMiseq.getType) c] ++ hmiseq ss2 ss2)
>	    ss1 = ((order.nub) ([ s | s <- ss2, notIn s (qs++is)]++is))
>	    ts = map time ss1
>	    num = if null ts then error "timer" else length (filter (== head ts) ts)
>	    cur = take num ss1
>	    nns = (map (subt (head ts)) [ s | s <- ss1, notElem s cur ])++qs 
>	in trun cn ss ms nns (map (subt (head ts)) cur)

	    st = if null bs && null ws then su else nws
	    ss2 = [ s | s <- st, ((notElem s cur) && (((not.isMiseq.getType) s) || 
				 (null [ c | c <- allstates ss, (((isMiseq.getType) s) && (c `approx` s) &&  
								((snd4.invMiseq.getType) s) < ((snd4.invMiseq.getType) c)) ] ))) ]

	in if null bs && null ws then timer1 ss cn mis su 
	   else if True then (error.show) nws else  else timer1 ss cn mis nws

> notIn :: State -> [State] -> Bool
> notIn s ss = (not.or)[ s `approx` x | x <- ss]
	   
remove miseq states from xs of which there exists instances in ws

> nmiseq :: [State] -> [State] -> [State]
> nmiseq ws xs = [ x | x <- xs, ((not.isMiseq.getType) x || (not.or) [ (fst4.invMiseq.getType) x == (fst.invTask.getType) n | n <- ws, (isTask.getType) n])]

> approx :: State -> State -> Bool
> approx x y = (getIn x == getIn y) && (getOut x == getOut y)

-- Ordering according to minimum range

> time :: State -> Time
> time s | (isStime.getType) s = (invStime.getType) s
>	 | (isItime.getType) s = (invItime.getType) s
>	 | otherwise = (fst.getRange) s 

> mint :: State -> State -> State
> mint x y = if (time x) <= (time y) then x else y

> order :: [State] -> [State]
> order [] = []
> order ss = let min = foldl1 mint ss 
>	     in [min]++ (order (filter (/= min) ss))

-- Ordering according to maximum range

> time2 :: State -> Time
> time2 s | (isStime.getType) s = (invStime.getType) s
>	  | (isItime.getType) s = (invItime.getType) s
>	  | otherwise = (snd.getRange) s 

> mint2 :: State -> State -> State
> mint2 x y = if (time2 x) <= (time2 y) then x else y

> order2 :: [State] -> [State]
> order2 [] = []
> order2 ss = let min = foldl1 mint2 ss 
>	      in [min]++ (order2 (filter (/= min) ss))


> sep :: State -> [State]
> sep s = if ((not (isTasks s || isSubs s)) || null [ t | (y,t) <- getError s, isItime y ]) then [s]
>	  else let (iy,it) = ((head.(filter (isItime.fst)).getError) s)
>	       in [s,(State iy [] [it] [] (ZERO,ZERO) [] [] [] [] [] 0)]  

> subexc :: [StateSet] -> State -> [State]
> subexc ss s = 
>	let su = [ u | (t,u) <- (tpsub ss), t == s ]  
>	    e = if null su then error "empty su\n" else ((filter (isItime.fst)).getError.head) su
>	in if (and [ notElem (s,u) (tpsub ss) | u <- allstates ss ]) then []
>	   else if null e then error "empty e\n" else [ (makexc.head) e ]++(finexc ss s (head su))

> finexc :: [StateSet] -> State -> State -> [State]
> finexc ss s v = if notElem (s,v) (insub ss v) then []
>		  else [ makexc e | e <- (getError v), (isItime.fst) e] ++ ((nub.concat.(map (finexc ss s))) st) 
>	where st = [ (fst.getSP) s | s <- ((tail.(getSubs v)) ss) ]

> makexc :: (BPMN.Type,Transition) -> State
> makexc (t,ts) = (State t [] [ts] [] (ZERO,ZERO) [] [] [] [] [] 0)

> tpsub :: [StateSet] -> [(State,State)]
> tpsub ss = union ((nub.concat) [ [ (t,s) | t <- (allsub ss s)] | s <- allstates ss, (isSubs s && or [ isItime x | (x,y) <- (getError s) ]) ])
>	     	   ((nub.concat) [ (insub ss s) | s <- allstates ss, (isSubs s && and [ (not.isItime) x | (x,y) <- (getError s) ]) ])

> insub :: [StateSet] -> State -> [(State,State)]
> insub ss su = if or [ isItime t | (t,y) <- (getError su) ] then [ (t,su) | t <- (allsub ss su) ]
>		else (nub.concat) (map (insub ss) st)
>		where st = [ (fst.getSP) s | s <- ((tail.(getSubs su)) ss) ]

Uses getSubs

> allsub :: [StateSet] -> State -> [State]
> allsub ss s = (allstates.(getSubs s)) ss

> gn :: State -> PName
> gn s 	| isMiseqs t = (fst4.invMiseqs) t
>      	| isMipars t = (fst4.invMipars) t 
>      	| isBpmn t = (fst.invBpmn) t
>	where t = getType s


subtracts delay ti from state 

> subt :: Time -> State -> State
> subt ti (State t is os es (t1,t2) im om rm am bm l)
>	| isTask t = (State t is os es ((if (t1 < ti) then ZERO else t1 - ti),(if t2 < ti then ZERO else t2 - ti)) im om rm am bm l) 
>	| isItime t = (State (Itime (if invItime t < ti then ZERO else invItime t - ti)) is os es (t1,t2) im om rm am bm l)
>	| isStime t = (State (Stime (if invStime t < ti then ZERO else invStime t - ti)) is os es (t1,t2) im om rm am bm l)
>	| otherwise = (State t is os es (t1,t2) im om rm am bm l)


coordinating timed states

 cn1 = State (Task "C" StandardT) [(True,"Flow25")] [(True,"Flow27")] [] (ZERO,ZERO) [] [] 1

 trun :: [StateSet] -> [State] -> [State] -> Process
 trun ss su cu = 
	let (ms,is) = unzip ([ splitseq c | c <- cu, (isMiseq.getType) c])
	    ps = (nub.concat) [ splitpar c | c <- cu, (isMipar.getType) c ] 
	    cu1 = (nub.concat) [[ c | c <- cu, (not.isMults) c],ps,is]
	    sy = nub (concatMap internal (union cu cu1))
	in (Parinter (Sequential (trun1 ss cu1) (ProcId ("run("++showSet(List Set sy)++")"))) (List Set sy) (record ss (union su ms) cu1 []))
	
 Hide (Parinter (Sequential (trun1 ss cu1) (ProcId ("run("++showSet(List Set sy)++")"))) (List Set sy) (record ss (union su ms) cu1 [])) (List Set sy)

-- cycle special event

> trun :: Int -> [StateSet] -> [(String,Int)] -> [State] -> [State] -> Process
> trun cn ss mis su cu = 
>	let (ms,is) = unzip ([ (splitseq ss c) | c <- (nmiseq cu cu), (isMiseq.getType) c] ++ hmiseq cu cu)
>	    ps = (nub.concat) [ splitpar c | c <- cu, (isMipar.getType) c ]
>	    cu1 = (nub.concat) [[ c | c <- cu, (not.isMults) c],ps,is]
>	    tp = (trun1 ss (union su ms) cu1)
>	    tpes = nub (filter isInternal ((CSP.alpha) tp)) 
>	in (Parinter (Sequential tp (ProcId ("run("++showSet(List Set tpes)++")")))
>		     (SName ("union(ses,"++(showSet (List Set tpes))++")"))
>		     (record cn ss mis tpes (union su ms) cu1 []))
		     
            record1 cn ss mis tpes tp (union su ms) cu1 []

	    ea = [ (((++) "fin.").show.invEnd.getType) s | s <- getAtomic ss, (isEnd.getType) s ]
		 ++ [ (((++) "aborts.").show.invAbort.getType) s | s <- getAtomic ss, (isAbort.getType) s ]

	in (Parinter (Sequential (trun1 ss (union su ms) cu1) (ProcId ("run(Internal)")))
		     (SName "union(Internal,") 
		     (record cn ss mis (union su ms) cu1 []))

    	    sy = nub (concatMap internal [ s | s <- allstates ss, isTimed s ])
	    nu2 = nub (concatMap internal ([ s | s <- allstates ss, (isTimed s && ((not.or) [ s `approx` c | c <- (union cu (msq su cu)) ]))]))
	    nu1 = [n | n <- nu2, notElem n [ "interrupted."++(atrans o) | c <- cu, (isItime.getType) c && (null.getIn) c, o <- getOut c]]
	 in (Parinter (Sequential (Hide (Parinter (Sequential (trun1 ss (union su ms) cu1) (Prefix "cycle" Skip))  
						 (List Set ["cycle"]) 
					   	 (Interrupt (ProcId ("run("++showSet(List Set nu1)++")")) (Prefix "cycle" Skip)))
				       (List Set ["cycle"])) 
				 (ProcId ("run(Internal)")))
		     (SName "Internal") 
		     (record cn ss mis (union su ms) cu1 []))

> hmiseq :: [State] -> [State] -> [(State,State)]
> hmiseq ws xs = map fter [ x | x <- xs, not (elem x (nmiseq ws xs)) ]   
>	where fter k = if null (gtask k) then error "hmiseq" else (k,(head.gtask) k)
>	      gtask k = [ x | x <- xs, (isTask.getType) x && (fst.invTask.getType) x == (fst4.invMiseq.getType) k ] 

> splitseq :: [StateSet] -> State -> (State,State)
> splitseq ss (State t is os es r im om rm am bm l) = 
>	let lp = if (isFix.snd4.invMiseq) t then Fix (((getLoops.snd4.invMiseq) t) - 1) else Ndet (((getLoops.snd4.invMiseq) t) - 1)
>	    nt = (Miseq ((fst4.invMiseq) t) lp ((trd4.invMiseq) t) ((fth4.invMiseq) t))
>	    nr = if null ss then r else head [ getRange s | s <- (allstates ss), (isMiseq.getType) s, (fst4.invMiseq.getType) s == (fst4.invMiseq) nt ] 
>	in ((State nt is os es nr im om rm am bm l),(State (Task ((fst4.invMiseq) t) StandardT) [] [(True,(((fst4.invMiseq) t)++"o"))] [] r [] [] [] [] [] 1))

> splitpar :: State -> [State]
> splitpar (State t is os es r im om rm am bm l) =
>	if (getLoops.snd4.invMipar) t == 0 then []
>	else let lp = if (isFix.snd4.invMipar) t then Fix (((getLoops.snd4.invMipar) t) - 1) else Ndet (((getLoops.snd4.invMipar) t) - 1)
>	    	 nt = (Mipar ((fst4.invMipar) t) lp ((trd4.invMipar) t) ((fth4.invMipar) t))
>	     in [(State (Task ((fst4.invMipar) t) StandardT) [] [] [] r [] [] [] [] [] 1)]++(splitpar (State nt is os es r im om rm am bm l))  

Execution process

 trun2 :: [StateSet] -> [State] -> [State] -> [Process]
 trun2 ss ms cu
	| null par1 && null cu1 = if and [ (isItime.getType) s | s <- cu ] then (map run3 cu) else error "trun1"     
	| null par1 = (map run2 cu1)
	| null cu1 = (map run1 par1)
	| otherwise = ((map run1 par1)++(map run2 cu1))

> trun1 :: [StateSet] -> [State] -> [State] -> Process
> trun1 ss ms cu
>	| null par1 && null cu1 = if and [ (isItime.getType) s | s <- cu ] then inter (map run3 cu) else error "trun1"     
>	| null par1 = inter (map run2 cu1)
>	| null cu1 = inter (map run1 par1)
>	| otherwise = Inter (inter (map run1 par1)) (inter (map run2 cu1))

>	where par = [ c | c <- cu, (isTask.getType) c && or [ ((isMipar.getType) t) && (((fst4.invMipar.getType) t) == ((fst.invTask.getType) c)) | t <- allstates ss ] ]
>	      par1 = (pars ss par (map (getmult ss gmpar) par))
>	      cu1 = [c | c <- cu, notElem c par && ((not.isItime.getType) c || (not.null.getIn) c)]
>	      run1 (k,ps) = parinter (List Set ([fin k, can k] ++ ((snd.unzip.intt) k))) (map (run12 k) ps)
>	      run12 k s = Interrupt (tmrn gmpar ss ms [ c | c <- cu, c /= s] s) 
>				    (Extern (extern [(Prefix y Skip) | (x,y) <- (intt k)]) (Extern (Prefix (fin k) Skip) (Prefix (can k) Skip)))  
>	      run2 s =  if (isTask.getType) s && (or [(isMiseq.getType) t && ((fst.invTask.getType) s == (fst4.invMiseq.getType) t) | t <- allstates ss ])
>			then let q = (getmult ss gmseq s) 
>				 iq = if (null.intt) q then Stop else (extern [(Prefix y Skip) | (x,y) <- (intt q)])

			     in Interrupt (tmrn gmseq ss ms [ c | c <- cu, c /= s] s) (Extern iq (Prefix (can q) Skip))

>			     in Interrupt (tmrn gmseq ss ms [ c | c <- cu, c /= s] s) (Extern Stop (Prefix (can q) Skip))
>			else (if (isStime.getType) s || ((isItime.getType) s && (not.null.getIn) s)
>			      then Sequential ((xgate . getOut) s) (Prefix (fin s) Skip)
>			      else Interrupt (ttrn ss [ c | c <- cu, c /= s] s) (Prefix (can s) Skip))
>	      run3 s =  if not ((isItime.getType) s && (null.getIn) s) then error "trun1"
>			else (Sequential ((xgate . getOut) s) (Prefix ("interrupted."++((atrans.strans) s)) Skip))

				   if (isItime.getType) s && (null.getIn) s 
				   then (Prefix ("interrupted."++((atrans.strans) s)) ((xgate . getOut) s)) 
				   else 

			    	          (Extern iq (Extern (Prefix (fin q) Skip) (Prefix (can q) Skip)))

> pars :: [StateSet] -> [State] -> [State] -> [(State,[State])]
> pars ss su ps = [ (p,[ s | s <- su, (getmult ss gmpar s) == p]) | p <- ps ] 

> gmpar, gmseq :: BPMN.Type
> gmpar = (Mipar "" (Ndet 0) StandardT NoCond)
> gmseq = (Miseq "" (Ndet 0) StandardT NoCond)

> getmult :: [StateSet] -> BPMN.Type -> State -> State
> getmult ss f s = case getMult ss f s of
>			[a] -> a
> 		  	[] -> (error "getmult")

> getMult :: [StateSet] -> BPMN.Type -> State -> [State]
> getMult ss f s = if (isMipar f) then [ t | t <- allstates ss, (isMipar.getType) t && ((fst4.invMipar.getType) t == (fst.invTask.getType) s) ]
> 		   else [ t | t <- allstates ss, (isMiseq.getType) t && ((fst4.invMiseq.getType) t == (fst.invTask.getType) s) ]

execution process for task

> ttrn :: [StateSet] -> [State] -> State -> Process
> ttrn ss cu s = 
>	let os = head [ x | x <- allstates ss, x `approx` s] 
>	    cs = nub [ c | c <- cu, or [c `approx` k | k <- (subexc ss os)] ]
>	    tx = [ k | k <- cu, (t,e) <- (getError s), strans k == e]
>	in if (not.null) cs 
>	   then let run1 se = 
>			let et = head [ j | u <- allstates ss, (t,e) <- getError u, (i,j) <- intt u, (e == strans se && i == e) ]
>			in Sequential ((xgate.getOut) se) (Prefix et Skip)
>		in if null cs then error "ttrn\n" else extern (map run1 cs)
> 	   else if ( length tx == 1 ) 
>		then let et = [ j | (i,j) <- intt s, i == (strans.head) tx ]
>		     in Sequential ((xgate.getOut.head) tx) (Prefix (head et) Skip)
>		else let es = [ snd e | e <- getError s, (not.isItime.fst) e ]
>		     	 tk = (if null es then Sequential (Prefix ((awork.getType) s) ((xgate.getOut) s)) (Prefix (fin s) Skip)
>			       else (Parinter (Interrupt (Prefix ((awork.getType) s) Skip) (xgate es))
>				   	      (List Set (map atrans es)) (Extern (except s) (Sequential ((xgate.getOut) s) (Prefix (fin s) Skip)))))
>	             in if (fst.getRange) s == (snd.getRange) s then tk else (Intern tk (Prefix (wait s) Skip)) 

> strans :: State -> Transition
> strans = (head . getOut)

> except :: State -> Process
> except s = extern (map exp [ e | (t,e) <- getError s, (not.isItime) t ])
>	where exp i = let et = head [ e | (t,e) <- intt s, i == t ]
>		      in Prefix (atrans i) (Prefix et Skip)

execution process for multiple instances

> tmrn :: BPMN.Type -> [StateSet] -> [State] -> [State] -> State -> Process
> tmrn f ss su cu s = 
>	let ms = getmult ss f s
>	    ke = [ c | c <- cu, or [ c `approx` k | k <- subexc ss ms ] ]
>	in if (not.null) ke then extern (map mke ke) 
>	   else (if or [ (strans k) == j | k <- cu, (i,j) <- getError ms ]
>		 then let tx = [ k | k <- cu, or [ (strans k) == j | (i,j) <- getError ms ] ]
>			  et = [ snd i | i <- intt ms, fst i == (strans.head) tx ]
>		      in if null tx || null et then error "tmrn"
>			 else Sequential ((xgate.getOut.head) tx) (Prefix (head et) Skip)
>		 else (if isMiseq f then tsrn ss su cu s else tprn ss cu s))  
>	where mke se =
>		let et = [ snd i | s <- allstates ss, e <- getError s, i <- intt s, (snd e == strans se) && (fst i == snd e) ]
>		in if null et then error "tmrn" 
>		   else Sequential ((xgate.getOut) se) (Prefix (head et) (Prefix (wait s) Skip))

	if length cu == 1 && or [ (isItime.getType) s | s <- cu ] && (isTask.getType) s 
	then (error.show) [ (strans k) == j | k <- cu, (i,j) <- getError ms ] else 			

execution process for multiple instances (sequential)

> tsrn :: [StateSet] -> [State] -> [State] -> State -> Process
> tsrn ss su cu s = 
>	let om = [ k | k <- allstates ss, (isMiseq.getType) k && ((fst4.invMiseq.getType) k == (fst.invTask.getType) s) ]
>	    mq = [ x | x <- su, (isMiseq.getType) x && ((fst4.invMiseq.getType) x == (fst.invTask.getType) s) ]
>	    es = [ snd e | e <- (getError.head) mq, (not.isItime.fst) e ]
>	    wn = (Sequential ((xgate.getOut.head) mq) (Prefix ((fin.head) mq) Skip))
>	    ep = if (isFix.snd4.invMiseq.getType.head) mq && (getLoops.snd4.invMiseq.getType.head) mq > 0 then (Prefix (fin s) Skip)
>		 else (if (getLoops.snd4.invMiseq.getType.head) mq > 0 then Extern (Prefix (fin s) Skip) wn else wn)
>	    sk = if null es then Sequential ((work.getType) s) ep
>		 else Parinter (Interrupt ((work.getType) s) (xgate es)) (List Set (map atrans es)) ep
>	in if null mq then error "tsrn" 
>	   else (if (fst.getRange) s == (snd.getRange) s then sk else (Intern sk (Prefix (wait s) Skip)))

	   else (if (snd4.invMiseq.getType.head) om - 1 == (snd4.invMiseq.getType.head) mq 
		 then Extern wn (if (fst.getRange) s == (snd.getRange) s then sk else (Intern sk (Prefix (wait s) Skip)))
		 else (if (fst.getRange) s == (snd.getRange) s then sk else (Intern sk (Prefix (wait s) Skip))))

incomplete implementation for multiple instances (parallel)

> tprn :: [StateSet] -> [State] -> State -> Process
> tprn ss cu s = (Prefix "tprn" Skip)

New recording process

 record1 :: Int -> [StateSet] -> [(String,Int)] -> [Event] -> [Process] -> [State] -> [State] -> [State] -> Process
 record1 cn ss mis te tp su cu wt = 
	case tp of
	  (Sequential x y) -> 


Recording process

> record :: Int -> [StateSet] -> [(String,Int)] -> [Event] -> [State] -> [State] -> [State] -> Process
> record cn ss mis te su cu wt =
>	if null cu 
>	then let sq = [ s | s <- su, (isMiseq.getType) s && or [((fst4.invMiseq.getType) s) == ((fst.invTask.getType) w) | w <- wt] ]
>	     in if (su == sq || null su) 
>	     	then let m = [ (snd.getRange) w | w <- wt, and [ (snd.getRange) w <= (snd.getRange) x | x <- wt ] ] 
>		     in if null m then error "record" else trun cn ss mis sq (map (subt (head m)) wt)
>	     	else let osq s = or [ s `approx` x && (getLoops.snd4.invMiseq.getType) s < (getLoops.snd4.invMiseq.getType) x | x <- allstates ss] 
>			 (mq,si) = unzip [ splitseq ss s | s <- su, (isMiseq.getType) s && osq s && (not.or) [((fst4.invMiseq.getType) s) == ((fst.invTask.getType) w) | w <- (su++wt), (isTask.getType) w] ]
>			 pos = [ s | s <- su, isTimed s && notIn s mq ] ++ mq ++ si
>		     in stable cn (timer ss pos wt) ss mis [ s | s <- su, notIn s pos] (union pos wt)
>	else let ws = [ s | s <- cu, (isTask.getType) s && (getRange s) /= (ZERO,ZERO), elem (wait s) te ]
>		 fs = [ s | s <- (msq su [ s | s <- cu, ((not.isItime.getType) s || (not.null.getIn) s) ]), elem (fin s) te ]
>		 is = [ q | q <- [ s | s <- cu, ((isItime.getType) s && (null.getIn) s)], 
>			    or [ ((isTimed v) && (y == strans q)) | v <- allstates ss, (x,y) <- getError v] ]
>	     in if (null ws) 
>		then if (null fs) 
>		     then if (null is) then error "record\n" 
>			  else (extern (map (rintt ss mis te su cu wt) is))
>		     else if (null is) then (extern (map (rfin cn ss mis te su cu wt) fs))
>			  else (Extern (extern (map (rfin cn ss mis te su cu wt) fs)) (extern (map (rintt ss mis te su cu wt) is)))
>		else if (null fs) 
>		     then if (null is) then (extern (map (rwait cn ss mis te su cu wt) ws)) 
>			  else (Extern (extern (map (rwait cn ss mis te su cu wt) ws)) (extern (map (rintt ss mis te su cu wt) is)))
>		     else if (null is) then (Extern (extern (map (rwait cn ss mis te su cu wt) ws)) (extern (map (rfin cn ss mis te su cu wt) fs)))
>			  else Extern (extern (map (rwait cn ss mis te su cu wt) ws)) (Extern (extern (map (rfin cn ss mis te su cu wt) fs)) (extern (map (rintt ss mis te su cu wt) is)))

	     in Extern (extern (map (rwait ss su cu wt) ws)) (Extern (extern (map (rfin ss su cu wt) fs)) (extern (map (rintt ss su cu wt) is))) 

		 is = [ e | q <- [ s | s <- cu, ((isItime.getType) s && (null.getIn) s)],
			    w <- [ v | v <- allstates ss, (x,y) <- getError v, ((isTimed v) && (y == strans q))], (t,e) <- intt w, t == (strans q)]


make sure multiple instance (sequential) states are included if one of its instances
is in the current state.

> msq :: [State] -> [State] -> [State]
> msq su cu = 
>	[ s | s <- cu, (not.isTask.getType) s || 
>	      and [ ((fst4.invMiseq.getType) x) /= ((fst.invTask.getType) s) | x <- su, (isMiseq.getType) x && (getLoops.snd4.invMiseq.getType) x == 0 && (isTask.getType) s ] ]
>	++ [ s | s <- su, ((isMiseq.getType) s && or [ ((fst4.invMiseq.getType) s) == ((fst.invTask.getType) k) | k <- cu, (isTask.getType) k ]) ] 
			    
> rwait :: Int -> [StateSet] -> [(String,Int)] -> [Event] -> [State] -> [State] -> [State] -> State -> Process
> rwait cn ss mis te su cu wt s = Prefix (wait s) (record cn ss mis te su [ c | c <- cu, c /= s] (wt++[s]))

> rfin :: Int -> [StateSet] -> [(String,Int)] -> [Event] -> [State] -> [State] -> [State] -> State -> Process
> rfin cn ss mis te su cu wt s =

	if show fp == show Stop then error "rfin" else 
	
>	Prefix (fin s) fp
>	where es = [ k | k <- (su++cu++wt), or [ strans k == y | (x,y) <- (getError s)] ]
>	      fp = (if (isTask.getType) s 
>		    then (record cn ss mis te (union [ s | s <- su, notElem s es ] (finds findS (getOut s) ss)) 
>					    [c | c <- cu, c /= s, notElem c es] [w | w <- wt, notElem w es])
>	   	    else if (isMiseq.getType) s 
>		    	 then let it = [ t | t <- cu, (isTask.getType) t && ((fst.invTask.getType) t == (fst4.invMiseq.getType) s) ]
>		     	      in (record cn ss mis te (union [ x | x <- su, notElem x (es++[s])] (finds findS (getOut s) ss)) 
>							  [c | c <- cu, notElem c it, notElem c es] [w | w <- wt, notElem w es])
>		      	 else if (isMipar.getType) s
>			      then let ip = [ i | i <- (union su cu), ((isTask.getType) i && s == (getmult ss gmpar i)) ]
>				   in (record cn ss mis te (union [ s | s <- su, notElem s (union ip es)] (finds findS (getOut s) ss)) [c | c <- cu, notElem c ip] wt)
>			      else (record cn ss mis te (union su (finds findS (getOut s) ss)) [c | c <- cu, c /= s] wt))

				  ms = [ t | t <- su, t `approx` (getmult ss gmseq s)]

> rintt :: [StateSet] -> [(String,Int)] -> [Event] -> [State] -> [State] -> [State] -> State -> Process
> rintt ss mis te su cu wt s =
>	Prefix ("interrupted."++((atrans.strans) s)) rnt
>	where k = [ a | a <- (su++cu++wt), ((elem (strans s) [ y | (x,y) <- (getError a)])) ]
>	      rnt = if (not.null) k
>	   	    then if or [ c `approx` (head k) && (isTask.getType) c | c <- (su++cu++wt) ] then (inttask ss mis te su cu wt s) 
>			 else if or [ (isTask.getType) c && (head k) `approx` (getmult ss gmseq c) | c <- (su++cu++wt) ]
>		     	      then intmiseq ss mis te su cu wt s else intmipar ss mis te su cu wt s
>	   	    else let all = allexc ss (nub (su++cu++wt)) s
>		    	     all1 = [ w | w <- all, or [ ((c `approx` w) || (elem w (getMult ss gmpar c)) || (elem w (getMult ss gmseq c))) | c <- (cu++wt++su) ] && not((isItime.getType) w && (null.getIn) w) ]
>			 in Sequential (inter [ (Prefix (can a) Skip) | a <- all1]) 
>			 	       (record 0 ss mis te (delete s (union (rmsub su all) (finds findS (getOut s) ss))) 
>							   (delete s (rmsub cu all)) 
>							   (delete s (rmsub wt all)))

 rintt :: [StateSet] -> [(String,Int)] -> [State] -> [State] -> [State] -> State -> Process
 rintt ss mis su cu wt s =
	Prefix ("interrupted."++((atrans.strans) s)) rnt
	where k = [ a | a <- (su++cu++wt), ((elem (strans s) [ y | (x,y) <- (getError a)])) ]
	      rnt = if (not.null) k
	   	    then if or [ c `approx` (head k) && (isTask.getType) c | c <- (su++cu++wt) ] then (inttask ss mis su cu wt s) 
			 else if or [ (head k) == (getmult ss gmseq c) | c <- (su++cu++wt) ]
		     	      then intmiseq ss mis su cu wt s else intmipar ss mis su cu wt s
	   	    else let all = allexc ss (nub (su++cu++wt)) s
		    	     all1 = [ w | w <- all, or [ ((c `approx` w) || (elem w (getMult ss gmpar c)) || (elem w (getMult ss gmseq c))) | c <- (cu++wt++su) ] && not((isItime.getType) w && (null.getIn) w) ]
			 in Sequential (inter [ (Prefix (can a) Skip) | a <- all1]) 
			 	       (record 0 ss mis (delete s (union (rmsub su all) (finds findS (getOut s) ss))) 
							(delete s (rmsub cu all)) 
							(delete s (rmsub wt all)))

> inttask :: [StateSet] -> [(String,Int)] -> [Event] -> [State] -> [State] -> [State] -> State -> Process
> inttask ss mis te su cu wt s = 
>	let k = [ c | c <- (su++cu++wt), or [ (elem (strans s) [ y | (x,y) <- (getError a)]) && (c `approx` a) | a <- allstates ss] ]
>	in if null k then error "inttask\n" 
>	   else (record 0 ss mis te (union [ s | s <- su, (not.or) [ (strans s) == y | (x,y) <- ((getError.head) k) ]] (finds findS (getOut s) ss))
>		      		    [ c | c <- cu, c /= s && c /= (head k) && ((not.or) [ (strans c) == y | (x,y) <- ((getError.head) k) ]) ] 
>				    [ w | w <- wt, w /= (head k) && ((not.or) [ (strans w) == y | (x,y) <- ((getError.head) k) ]) ])

> intmiseq :: [StateSet] -> [(String,Int)] -> [Event] -> [State] -> [State] -> [State] -> State -> Process
> intmiseq ss mis te su cu wt s =
>	let k = [ c | c <- (su++cu++wt), or [ (elem (strans s) [ y | (x,y) <- (getError a)]) 
>					       && (isTask.getType) c && (a == (getmult ss gmseq c)) | a <- allstates ss] ]
>	    l = if null k then error "intmiseq\n" else 
>		[ s | s <- su, or [ (a == (getmult ss gmseq (head k))) && (s `approx` a) | a <- allstates ss] ]
>	in if null l then error "intmiseq2\n" else
>	   (record 0 ss mis te (union [ s | s <- su, s /= (head l) && s /= (head k) && (not.or) [ (strans s) == y | (x,y) <- ((getError.head) l) ]] (finds findS (getOut s) ss))
>		      	       [ c | c <- cu, c /= (head k) && ((not.or) [ (strans c) == y | (x,y) <- ((getError.head) l) ]) ] 
>			       [ w | w <- wt, w /= (head k) && ((not.or) [ (strans w) == y | (x,y) <- ((getError.head) l) ]) ])		       

> intmipar :: [StateSet] -> [(String,Int)] -> [Event] -> [State] -> [State] -> [State] -> State -> Process
> intmipar ss mis te su cu wt s =
>	let ks = [ c | c <- (cu++wt), or [ (elem (strans s) [ y | (x,y) <- (getError a)]) && (a == (getmult ss gmpar c)) | a <- allstates ss] ]
>	    l = [ a | a <- allstates ss, (elem (strans s) [ y | (x,y) <- (getError a)])]
>	in if null l then error "intmipar\n" else
>	   (record 0 ss mis te (union [ s | s <- su, (not.or) [ (strans s) == y | (x,y) <- ((getError.head) l) ]] (finds findS (getOut s) ss))
>		      		[ c | c <- cu, (notElem c ks) && ((not.or) [ (strans c) == y | (x,y) <- ((getError.head) l) ]) ] [ w | w <- wt, (notElem w ks)] )

> allexc :: [StateSet] -> [State] -> State -> [State]
> allexc ss su s = 
>	let k = [ (union [ w | w <- su, or [ (strans w) == (snd y) | x <- (allsub ss u), y <- (getError x) ] ] (allsub ss u)) 
>						| u <- allstates ss, (isSubs u) && or [ (snd e) == (strans s) | e <- getError u ] ]
>	in if null k then error "allexc\n" else head k

> rmsub :: [State] -> [State] -> [State]
> rmsub ss tt =
>	let x = [ s | s <- ss, or [ (((not ((isMipar.getType) t && (isTask.getType) s)) || (fst.invTask.getType) s == (fst4.invMipar.getType) t) 
>				     && ((not ((isMiseq.getType) t && (isTask.getType) s)) || (getType s) == (getType.fst.(splitseq [])) t)
>				     && ((not ((((isMiseq.getType) t && (isMiseq.getType) s)) || (not.isMults) t)) || s `approx` t)) | t <- tt ] ]
>	in [ s | s <- ss, notElem s x]

 err :: (Show a) => [a] -> c
 err s = error (concatMap sh s)
	where sh x = (show x) ++ "\n\n" 

> update :: Time -> Time -> State -> State
> update t1 t2 (State t is os es tr im om rm am bm l) = (State t is os es (t1,t2) im om rm am bm l)

return a parameterised process name

> pname :: PName -> State -> String
> pname p (State t is os es rg im om rm am bm i) = ((name p)++"("++sname(t,os)++")")

return a parameterised set name

> aname :: PName -> State -> String
> aname p (State t is os es rg im om rm am bm i) = (((BPMNToCSP.alpha) p)++"("++sname(t,os)++")")

 sgname :: String -> String
 sgname c = "Sig

> ugpname :: PName -> String
> ugpname p = "UGP("++(gname p)++")"

> tgpname :: PName -> String
> tgpname p = "TGP("++(gname p)++")"

> ganame :: PName -> String
> ganame p = "GA("++(gname p)++")"

> name :: String -> String
> name c = "P"++c

> gname :: String -> String
> gname c = "G"++c

> dfname :: String -> String
> dfname c = "DF"++c

> cpname :: String -> String
> cpname c = "PC"++c

> subname :: String -> String
> subname c = "S"++c

> wname :: String -> String
> wname c = "W"++c

> lname :: String -> String
> lname c = "L"++c

> cname :: String -> String
> cname c = "C"++c

> tname :: String -> String
> tname c = "T"++c

> uname :: String -> String
> uname c = "U"++c

> alpha :: String -> String
> alpha c = "A"++c

> index :: String -> String
> index c = "i"++c

> indexSetName :: String -> String
> indexSetName c = "I"++c

> indexSet :: (Either BPMN [StateSet]) -> [Event]
> indexSet (Left b) = map (name.fst) b
> indexSet (Right ((Atomic ss):sss)) = (map (sname.gtst) ss)++(map (sname.gtst.fst.getSP) sss)
>	where gtst s = (getType s,getOut s)

return index name for each state

> sname :: (BPMN.Type,[Transition]) -> Event
> sname	(Exgate,((g,l):ts)) = "exgate"++l
> sname	(Xgate,((g,l):ts)) = "xgate"++l
> sname	(Agate,((g,l):ts)) = "agate"++l
> sname (Start,((g,l):ts)) = "start"++l
> sname ((Smessage _),((g,l):ts)) = "smsg"++l
> sname	((Imessage _),((g,l):ts)) = "imsg"++l
> sname	((Emessage _),((g,l):ts)) = "emsg"++l
> sname ((Stime _),((g,l):ts)) = "stime"++l
> sname	((Itime _),((g,l):ts)) = "itime"++l
> sname	((Ierror _),((g,l):ts)) = "ierror"++l
> sname ((End i),_) = "end"++(show i)
> sname ((Abort i),_) = "abort"++(show i)
> sname	((Eerror i j),_) = "eerror"++(show i)
> sname ((Task n _),_) = n
> sname	((Miseq n _ _ _),_) = n
> sname	((Miseqs n _ _ _),_) = n
> sname	((Mipar n _ _ _),_) = n
> sname	((Mipars n _ _ _),_) = n
> sname	((Bpmn n _),_) = n
> sname (x,y) = "start1"


return index for end events

> endindex :: (PName,Int) -> String
> endindex (p,i) = p++(show i) 

return specific abstract processes e.g. deadlock free process 

> makeSpec :: (PName,[StateSet]) -> Script -> Script
> makeSpec (n,ss) (Script d c p se r) = (Script d c np se r)
>	where np = mprocs p [((dfname n),[],Intern (Indintern ("i",(List Set ae)) (Prefix "i" (ProcId (dfname n)))) fap)]
>	      fs = [ "fin."++((show.invEnd.getType) s) | s <- level ss, ((isEnd.getType) s) ]
>	      er = [ ("error."++(show.fst.invEerror.getType) s) | s <- level ss, ((isEerror.getType) s) ]
>	      ab = [ ("aborts."++(show.invAbort.getType) s) | s <- allstates ss, ((isAbort.getType) s) ]
>	      aee = allstates ss
>	      ae = (((map (awork.getType)).(filter isTasks)) aee)
>	           ++ (filter (flip notElem fs) [ "fin."++((show.invEnd.getType) s) | s <- aee, ((isEnd.getType) s) ])
>		   ++ (filter (flip notElem ab) [ ("aborts."++(show.invAbort.getType) s) | s <- aee, ((isAbort.getType) s) ])
>		   ++ (filter (flip notElem er) [ ("error."++(show.fst.invEerror.getType) s) | s <- aee, ((isEerror.getType) s) ])
>	      fap = if null ab && null er then (Indintern ("i",(List Set fs)) (Prefix "i" Skip)) 
>		    else if null ab then (Intern (Indintern ("i",(List Set fs)) (Prefix "i" Skip)) (Indintern ("i",(List Set er)) (Prefix "i" Skip)))
>		    	 else if null er then (Intern (Indintern ("i",(List Set fs)) (Prefix "i" Skip)) (Indintern ("i",(List Set ab)) (Prefix "i" Stop)))
>			      else (Intern (Intern (Indintern ("i",(List Set fs)) (Prefix "i" Skip)) (Indintern ("i",(List Set er)) (Prefix "i" Skip))) (Indintern ("i",(List Set ab)) (Prefix "i" Stop)))

	      ae = filter (`notElem` (union fs ab)) (aprocess ss)
	      
return specific abstract processes e.g. deadlock free process (for global diagrams)

> makeCSpec :: [(PName,[StateSet])] -> Script -> Script
> makeCSpec nss (Script d c p se r) = (Script d c np se r)
>	where np = mprocs p [((dfname ""),(if null ab then [] else [LP chaos]),Intern (Indintern ("i",(List Set ae)) (Prefix "i" (ProcId (dfname "")))) fap)]
>	      fs = [ [ "fin."++((show.invEnd.getType) s) | s <- level ss, ((isEnd.getType) s) ] | (n,ss) <- nss ]
>	      ab = filter (not.null) [ [ ("aborts."++(show.invAbort.getType) s) | s <- allstates ss, ((isAbort.getType) s) ] | (n,ss) <- nss ]
>	      aees = concat [ allstates ns | (n,ns) <- nss ]
>	      ae = (((map (awork.getType)).(filter isTasks)) aees)
>	           ++ (filter (flip notElem (concat fs)) [ "fin."++((show.invEnd.getType) s) | s <- aees, ((isEnd.getType) s) ])
>		   ++ (filter (flip notElem (concat ab)) [ ("aborts."++(show.invAbort.getType) s) | s <- aees, ((isAbort.getType) s) ])
>	      fap = if null ab then inter [ (Indintern ("i",(List Set f)) (Prefix "i" Skip)) | f <- fs ]
>		    else (Intern (inter [ (Indintern ("i",(List Set f)) (Prefix "i" Skip)) | f <- fs ]) 
>				 (Indintern ("i",(List Set (concat ab))) (Prefix "i" (ProcId ("CHAOS("++showSet(List Set (ae++(concat ab)))++")")))))

> chaos ::(ProcVar,[Local],Process)
> chaos = ("CHAOS(S)",[],(Intern (Indintern ("i",(SName "S")) (Prefix "i" (ProcId "CHAOS(S)"))) Stop))
