> module XMLToBPMN where
> import Text.XML.HaXml.Types
> import Text.XML.HaXml.Parse (xmlParse)

 import WorkflowToBPMN (getXT)

> import Char (isDigit,digitToInt)
 
> import BPMN

 import System (getArgs)
 import IO (readFile,putStr)

> trans :: FilePath -> IO()
> trans i = do {xml <- readFile i;
>	        putStr ((show (xTob (xmlParse i xml)))++"\n")  }
		
> getXT :: String -> Time
> getXT ss = (MkTime (gDi ss) (gCo ss "" 'Y') (gMon ss ss) (gCo ss "" 'D') (gCo ss "" 'H') (gMin ss) (gCo ss "" 'S'))
>	where gDi ('-':ss) = Nve
>	      gDi _ = Pve
>	      gMon [] _ = 0
>	      gMon (s:ss) tt = if (s =='T') then 0 else (if (s == 'M') then gCo tt "" 'M' else gMon ss tt) 
>	      gMin [] = 0
>	      gMin (s:ss) = if (s =='T') then gCo ss "" 'M' else gMin ss
>	      gCo [] _ _ = 0
>	      gCo (s:ss) n t 
>		| (isDigit s) = gCo ss (n++[s]) t 
>		| (s == t) = (read n :: Integer)
>		| otherwise = gCo ss "" t

 xTobf :: FilePath -> BPMN
 xTobf i = do { xml <- readFile i;
		 (xTob . xmlParse i) xml }

> xTob :: Document -> BPMN
> xTob (Document p st (Elem "diagram" atts cs) ms) 
>		= (makePools (splitPool cs [ (getProperty c "Name") | c <- cs, (ckElement "Pool" c) ]))
> xTob _ = []

		= if ((length [ c | c <- cs, (ckElement "Pool" c) ]) <= 1) then (makePools [("Pool",cs)])  
		  else (makePools (splitPool cs [ (getProperty c "Name") | c <- cs, (ckElement "Pool" c) ]))  

Given a list of XML contents, group them into separate lists according to their associated BPMN pool

> splitPool :: [Content] -> [String] -> [(String,[Content])]
> splitPool _ [] = []
> splitPool cs (s:ss) = [(s,(putIn cs s)++(flowsMsg cs))]++(splitPool cs ss)
>	where putIn [] _ = []
>	      putIn (x:xs) s = if ((getProperty x "Lane") == s) then [x]++(putIn xs s) else (putIn xs s)   

	      putIn ((CElem (Elem nme atts cs)):xs) s = if (isTPool cs s) then [(CElem (Elem nme atts cs))]++(putIn xs s)
							else (putIn xs s)
	      isTPool [] s = False
	      isTPool ((CElem (Elem "property" [("name",(AttValue [(Left "Lane")]))] [(CString b t)])):xs) s = (s==t)
	      isTPool (x:xs) s = isTPool xs s

Get Id of the XML element (id of BPMN state)

> getId :: Content -> String
> getId = getAtt "id"

> getAtt :: String -> Content -> String	
> getAtt s (CElem (Elem nme atts cs)) = (gAtt s atts)
>	where 	gAtt s [] = ""
>		gAtt s ((st,(AttValue (at:att))):ass) = 
>					if (s == st) then case at of
>								(Left a) -> a
>								(Right b) -> ""
>					else gAtt s ass
>		gAtt s (a:ass) = gAtt s ass
> getAtt _ _ = ""

Return the tag name of a XML element

> getName :: Content -> Name
> getName (CElem (Elem nme atts cs)) = nme
> getName _ = ""

Get property value given name

> getProperty :: Content -> String -> String
> getProperty (CElem (Elem nme atts cs)) pnme = gProp cs pnme
>	where gProp [] _ = ""
>	      gProp ((CElem (Elem "property" [("name",(AttValue [(Left pn)]))] [(CString b t)])):cs) pnme = 
>				if (pnme == pn) then t else gProp cs pnme
>	      gProp (c:cs) pnme = gProp cs pnme
> getProperty _ _ = ""


> isElement :: Content -> Bool
> isElement (CElem (Elem nme cs ms)) =  True 
> isElement _ = False

> ckElement :: Name -> Content -> Bool
> ckElement n (CElem (Elem nme cs ms)) =  (n == nme) 
> ckElement _ _ = False

Turn a list of lists of XML contents, each list associates with a BPMN pool into list of lists of lists of BPMN States
[[StateSet01,StateSetforSP101,...,StateSetforSP1M],...,[StateSetI,StateSetforSPI01,...,StateSetforSPIN]]

> makePools :: [(String,[Content])] -> BPMN
> makePools = makePools1 (0,0,0) 

> makePools1 :: (Int,Int,Int) -> [(String,[Content])] -> BPMN
> makePools1 (i,j,k) [] = []
> makePools1 (i,j,k) ((s,cs):css) = [(a,b)]++(makePools1 (ni+1,nj+1,nk+1) css)
>	where (a,b) = analysePool (i,j,k) (s,cs)
>	      (ni,nj,nk) = ((maximum.countEnds) b,(maximum.countAborts) b,(maximum.countEerrors) b)

returns all identifiers (integer) of end states

> countEnds :: [StateSet] -> [Int]
> countEnds [] = []
> countEnds ((Atomic s):ss) = (((map (invEnd.getType)) . (filter (isEnd.getType))) s) ++ (countEnds ss)
> countEnds ((SubProcess s ss):sss) = (countEnds ss) ++ (countEnds sss)

returns all identifiers (integer) of abort states

> countAborts :: [StateSet] -> [Int]
> countAborts [] = []
> countAborts ((Atomic s):ss) = (((map (invAbort.getType)) . (filter (isAbort.getType))) s) ++ (countAborts ss)
> countAborts ((SubProcess s ss):sss) = (countAborts ss) ++ (countAborts sss)

returns all identifiers (integer) of eerror states

> countEerrors :: [StateSet] -> [Int]
> countEerrors [] = []
> countEerrors ((Atomic s):ss) = (((map (fst.invEerror.getType)) . (filter (isEerror.getType))) s) ++ (countEerrors ss)
> countEerrors ((SubProcess s ss):sss) = (countEerrors ss) ++ (countEerrors sss)

return all sequence and message flows

> flowsMsg :: [Content] -> [Content]
> flowsMsg [] = []
> flowsMsg ((CElem (Elem "SubProcess" atts fs)):cs) = (flowsMsg fs)++(flowsMsg cs)
> flowsMsg (c:cs) = if ((getName c) == "Flow" || (getName c) == "Message") then [c]++(flowsMsg cs)
>		    else flowsMsg cs
	      
 analysePool :: (String,[Content]) -> (String,SProcess)

> analysePool :: (Int,Int,Int) -> (String,[Content]) -> (String,[StateSet])
> analysePool (i,j,k) (nme,cs) =  (nme,analyseDiagram (i,j,k) [ c | c <- cs, ((getName c) /= "Intermediate")])

 analyseDiagram :: (Int,Int) -> [Content] -> SProcess

> analyseDiagram :: (Int,Int,Int) -> [Content] -> [StateSet]
> analyseDiagram (i,j,k) cs = (analyseSub cs indvSet (getEndAbortError (i,j,k) [(Atomic indvSet)])) where indvSet = (analyseIndv cs cs i j k)

> analyseIndv :: [Content] -> [Content] -> Int -> Int -> Int -> [State]
> analyseIndv [] _ _ _ _ = []
> analyseIndv (e:es) cs i j k 
>	| (isTypeMiseq e) = [(makeMiseq e cs)]++(analyseIndv es cs i j k)
>	| (isTypeMipar e) = [(makeMipar e cs)]++(analyseIndv es cs i j k)
>	| (isTypeTask e) = [(makeTask e cs)]++(analyseIndv es cs i j k)
>	| (isTypeAbort e) = [(makeAbort e cs j)]++(analyseIndv es cs i (j+1) k)
>	| (isTypeEerror e) = [(makeEerror e cs k)]++(analyseIndv es cs i j (k+1))
>	| (isTypeEnd e) = [(makeEnd e cs i)]++(analyseIndv es cs (i+1) j k)
>	| (isTypeIerror e) = [(makeIerror e cs)]++(analyseIndv es cs i j k)
>	| (isTypeItime e) = [(makeItime e cs)]++(analyseIndv es cs i j k)
>	| (isTypeStime e) = [(makeStime e cs)]++(analyseIndv es cs i j k)
>	| (isTypeImessage e) = [(makeImessage e cs)]++(analyseIndv es cs i j k)
>	| (isTypeSmessage e) = [(makeSmessage e cs)]++(analyseIndv es cs i j k)
>	| (isTypeEmessage e) = [(makeEmessage e cs)]++(analyseIndv es cs i j k)
>	| (isTypeStart e) = [(makeStart e cs)]++(analyseIndv es cs i j k)
>	| (isTypeXgate e) = [(makeXgate e cs)]++(analyseIndv es cs i j k)
>	| (isTypeExgate e) = [(makeExgate e cs)]++(analyseIndv es cs i j k)
>	| (isTypeAgate e) = [(makeAgate e cs)]++(analyseIndv es cs i j k)
> 	| otherwise = (analyseIndv es cs i j k)

 analyseSub :: [Content] -> [State] -> (Int,Int) -> SProcess

> analyseSub :: [Content] -> [State] -> (Int,Int,Int) -> [StateSet]
> analyseSub ct ss (i,j,k) = [(Atomic ss)]++(makeSub ct ct (i,j,k))
>	where makeSub [] _ _ = [] 
>	      makeSub (c:cs) es (i,j,k)
>		| (isTypeMiseqs c) = [(makeMiseqs c es (i,j,k))]++(makeSub cs es (getEndAbortError (i,j,k) (getSubStates mksubP)))
>		| (isTypeMipars c) = [(makeMipars c es (i,j,k))]++(makeSub cs es (getEndAbortError (i,j,k) (getSubStates mksubP)))
>		| (isTypeSubP c) = [mksubP]++(makeSub cs es (getEndAbortError (i,j,k) (getSubStates mksubP)))
>		| otherwise = makeSub cs es (i,j,k)
>			where mksubP = makeSubP c es (i,j,k)
>			      getSubStates (SubProcess s ss) = ss
>			      getSubStates _ = []

> isTypeSubP, isTypeMiseqs, isTypeMipars :: Content -> Bool
> isTypeSubP c = (isElement c) && (getName c == "SubProcess")
> isTypeMiseqs c = (isTypeSubP c) && (getProperty c "LoopType" == "true")
> isTypeMipars c = (isTypeSubP c) && (getProperty c "Timing" == "Parallel")

> makeSubP, makeMiseqs, makeMipars :: Content -> [Content] -> (Int,Int,Int) -> StateSet  
> makeSubP c cs (i,j,k) = (SubProcess (makeInSubP c cs) sub) where sub = (analyseDiagram (i,j,k) [ k | k <- (findSubContent c), ((getName k) /= "Intermediate")]) 
> makeMiseqs c cs (i,j,k) = (SubProcess (makeInMiseqs c cs) sub) where sub = (analyseDiagram (i,j,k) [ k | k <- (findSubContent c), ((getName k) /= "Intermediate")])
> makeMipars c cs (i,j,k) = (SubProcess (makeInMipars c cs) sub) where sub = (analyseDiagram (i,j,k) [ k | k <- (findSubContent c), ((getName k) /= "Intermediate")])

> findSubContent :: Content -> [Content]
> findSubContent (CElem (Elem nme ms cs)) = subCon cs
>	where subCon = filter (not.(ckElement "property"))

> makeInSubP, makeInMiseqs, makeInMipars :: Content -> [Content] -> State
> makeInSubP c cs = (State (Bpmn (getProperty c "Name") (getXPType c)) (makeInc c cs) (makeOut c cs) (makeExc c cs) (makeTR c) (makeRec c cs) (makeSnd c cs) (makeRep c cs) (makeAcc c cs) (makeBk c cs) 1)
> makeInMiseqs c cs = (State (Miseqs (getProperty c "Name") (mkLoop c (mkInt (getProperty c "Iteration"))) (getXPType c) (getXCondition c)) (makeInc c cs) (makeOut c cs) (makeExc c cs) (makeTR c) (makeRec c cs) (makeSnd c cs) (makeRep c cs) (makeAcc c cs) (makeBk c cs) 1)
> makeInMipars c cs = (State (Mipars (getProperty c "Name") (mkLoop c (mkInt (getProperty c "Iteration"))) (getXPType c) (getXCondition c)) (makeInc c cs) (makeOut c cs) (makeExc c cs) (makeTR c) (makeRec c cs) (makeSnd c cs) (makeRep c cs) (makeAcc c cs) (makeBk c cs) 1)

> mkLoop :: Content -> Int -> Loops
> mkLoop c i = if (getProperty c "Loop") == "Fix" then Fix i else Ndet i

> getXPType :: Content -> BpmnType
> getXPType c  
>	| ((getProperty c "BpmnType") == "SequenceB") = SequenceB
>	| ((getProperty c "BpmnType") == "InterventionB") = InterventionB
>	| ((getProperty c "BpmnType") == "ScopeB") = ScopeB
>	| ((getProperty c "BpmnType") == "DependentB") = DependentB
>	| ((getProperty c "BpmnType") == "RepeatB") = RepeatB
>	| otherwise = SequenceB

 mkInt :: String -> Int
 mkInt s = read(s) :: Int

> mkInt :: [Char] -> Int
> mkInt s = mI 1 (map digitToInt (reverse s))
>	where mI _ [] = 0
>	      mI n (i:is) = (i*n)+(mI (n*10) is) 


 	where cToI c
		| (c == '0') = 0 
		| (c == '1') = 1 
		| (c == '2') = 2 
		| (c == '3') = 3 
		| (c == '4') = 4 
		| (c == '5') = 5 
		| (c == '6') = 6 
		| (c == '7') = 7 
		| (c == '8') = 8 
		| (c == '9') = 9


> getEndAbortError :: (Int,Int,Int) -> [StateSet] -> (Int,Int,Int)
> getEndAbortError (i,j,k) [] = (i,j,k)
> getEndAbortError (i,j,k) ((SubProcess s tt):ss) = (getEndAbortError (getEndAbortError (i,j,k) tt) ss) 
> getEndAbortError (i,j,k) ((Atomic tt):ss) = (getEndAbortError (getInvEA (i,j,k) tt) ss)

Get number of end/abort states from a list of non-subprocess states ([State])

> getInvEA :: (Int,Int,Int) -> [State] -> (Int,Int,Int)
> getInvEA (i,j,k) = foldr addEA (i,j,k) 
> 		where addEA x (m,n,o) 
>			| isEnd(getType x) = (m+1,n,o)
>			| isAbort (getType x) = (m,n+1,o)
>			| isEerror (getType x) = (m,n,o+1)
>			| otherwise = (m,n,o)

> isTypeStart, isTypeTask, isTypeMiseq, isTypeMipar, isTypeEnd, isTypeAbort, isTypeStime, isTypeItime, isTypeSmessage, isTypeImessage, isTypeXgate, isTypeExgate, isTypeAgate :: Content -> Bool
> isTypeStart c = (isElement c) && (getName c == "Start")
> isTypeStime c = (isTypeStart c) && (getProperty c "Trigger" == "Timer")
> isTypeSmessage c = (isTypeStart c) && (getProperty c "Trigger" == "Message")
> isTypeItime c = (not.isTypeStart) c && (getProperty c "Trigger" == "Timer")
> isTypeImessage c = (not.isTypeStart) c && (not.isTypeEnd) c && (getProperty c "Trigger" == "Message")
> isTypeIerror c = (not.isTypeStart) c && (not.isTypeEnd) c && (getProperty c "Trigger" == "Exception")
> isTypeTask c = (isElement c) && (getName c == "Task")
> isTypeMiseq c = (isTypeTask c) && (getProperty c "LoopType" == "true") 
> isTypeMipar c = (isTypeTask c) && (getProperty c "Timing" == "Parallel")
> isTypeEnd c = (isElement c) && (getName c == "End")
> isTypeEmessage c = (isTypeEnd c) && (getProperty c "Trigger" == "Message")
> isTypeEerror c = (isTypeEnd c) && (getProperty c "Trigger" == "Exception")
> isTypeAbort c = (isTypeEnd c) && (getProperty c "Trigger" == "Terminate")
> isTypeXgate c = (isElement c) && (getProperty c "Name" == "Data-XOR")
> isTypeExgate c = (isElement c) && (getProperty c "Name" == "Event-XOR")
> isTypeAgate c = (isElement c) && (getProperty c "Name" == "AND")

> makeStart, makeTask, makeMiseq, makeMipar, makeStime, makeItime, makeSmessage, makeImessage, makeEmessage, makeXgate, makeExgate, makeAgate :: Content -> [Content] ->  State
> makeStart c cs = (State Start [] (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeStime c cs = (State (Stime (getXTime c)) [] (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeItime c cs = (State (Itime (getXTime c)) (makeInc c cs) (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeTask c cs = (State (Task (getProperty c "Name") (getXTaskType c)) (makeInc c cs) (makeOut c cs) (makeExc c cs) (makeTR c) (makeRec c cs) (makeSnd c cs) (makeRep c cs) (makeAcc c cs) (makeBk c cs) 1)
> makeMiseq c cs = (State (Miseq (getProperty c "Name") (mkLoop c (mkInt(getProperty c "Iteration"))) (getXTaskType c) (getXCondition c)) (makeInc c cs) (makeOut c cs) (makeExc c cs) (makeTR c) (makeRec c cs) (makeSnd c cs) (makeRep c cs) (makeAcc c cs) (makeBk c cs) 1)
> makeMipar c cs = (State (Mipar (getProperty c "Name") (mkLoop c (mkInt(getProperty c "Iteration"))) (getXTaskType c) (getXCondition c)) (makeInc c cs) (makeOut c cs) (makeExc c cs) (makeTR c) (makeRec c cs) (makeSnd c cs) (makeRep c cs) (makeAcc c cs) (makeBk c cs) 1)
> makeXgate c cs = (State Xgate (makeInc c cs) (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeExgate c cs = (State Exgate (makeInc c cs) (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeAgate c cs = (State Agate (makeInc c cs) (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeSmessage c cs = let mf = (makeRec c cs) 
>		      in if null mf then (State (Smessage Nothing) [] (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
>			 else (State (Smessage (Just (head mf))) [] (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeImessage c cs = let mf = (makeRec c cs) 
>		      in if null mf then (State (Imessage Nothing) (makeInc c cs) (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
>			 else (State (Imessage (Just (head mf))) (makeInc c cs) (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeEmessage c cs = let mf = (makeRec c cs) 
>		      in if null mf then (State (Emessage Nothing) (makeInc c cs) [] [] (ZERO,ZERO) [] [] [] [] [] 1)
>			 else (State (Emessage (Just (head mf))) (makeInc c cs) [] [] (ZERO,ZERO) [] [] [] [] [] 1)

> makeIerror c cs = (State (Ierror (getException c)) (makeInc c cs) (makeOut c cs) [] (ZERO,ZERO) [] [] [] [] [] 1)

> makeEnd, makeAbort :: Content -> [Content] -> Int -> State
> makeEnd c cs n = (State (End n) (makeInc c cs) [] [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeAbort c cs n = (State (Abort n) (makeInc c cs) [] [] (ZERO,ZERO) [] [] [] [] [] 1)
> makeEerror c cs n = (State (Eerror n (getException c)) (makeInc c cs) [] [] (ZERO,ZERO) [] [] [] [] [] 1)

> getXTaskType :: Content -> TaskType
> getXTaskType c  
>	| ((getProperty c "TaskType") == "StandardT") = StandardT
>	| ((getProperty c "TaskType") == "InterventionT") = InterventionT (getIntervention c)
>	| otherwise = StandardT

> getIntervention :: Content -> Intervention
> getIntervention c =
>	let 
>	  n = getProperty c "IName" 
>	  q = getProperty c "IDose" 
>	  m = getProperty c "Method"
>	in 
>	 if n == "" then NoDetails 
>	 else Intervention (getProperty c "IName") (getProperty c "IDose") (getProperty c "Method")

> getXTime :: Content -> Time
> getXTime c = if ((getProperty c "Time") == "") then NOBOUND 
>	       else getXT (getProperty c "Time")

> getXCondition :: Content -> BCondition
> getXCondition c = if ((getProperty c "LoopCond") == "") then NoCond else NoCond

> makeInc, makeOut :: Content -> [Content] -> [Transition]
> makeInc c cs = mkFlow "to" (getId c) (flowsMsg cs)
> makeOut c cs = mkFlow "from" (getId c) (flowsMsg cs)

> mkFlow :: String -> String -> [Content] -> [Transition]
> mkFlow fd nm [] = []
> mkFlow fd nm (c:cs) = if (((getName c) == "Flow") && ((getAtt fd c) == nm)) 
>			then [(True,getId c)]++(mkFlow fd nm cs) else (mkFlow fd nm cs)

> makeTR :: Content -> TRange
> makeTR c = (getXT (getProperty c "Minrange"),getXT (getProperty c "Maxrange"))

> makeRec, makeSnd, makeRep, makeAcc, makeBk :: Content -> [Content] -> [Messageflow]
> makeRec c cs = mkMsgf "to" "init" (getId c) (flowsMsg cs)
> makeSnd c cs = (mkMsgf "from" "init" (getId c) (flowsMsg cs)) ++ (mkMsgf "from" "break" (getId c) (flowsMsg cs))
> makeRep c cs = mkMsgf "from" "done" (getId c) (flowsMsg cs)
> makeAcc c cs = mkMsgf "to" "done" (getId c) (flowsMsg cs)
> makeBk c cs = if null me then [] else mkMsgf "to" "break" ((getId.head) me) (flowsMsg cs)
>		where me = [ e | e <- findSubContent c, ((getName e) == "Intermediate" && (getProperty c "Trigger") == "Message") ]

> mkMsgf :: String -> String -> String -> [Content] -> [Messageflow]
> mkMsgf fd ty nm [] = []
> mkMsgf fd ty nm (c:cs) = if (((getName c) == "Message") && ((getAtt fd c) == nm) && (getProperty c "Message") == ty) 
>			   then [(ty,getId c)]++(mkMsgf fd ty nm cs) else (mkMsgf fd ty nm cs)

> makeExc :: Content -> [Content] -> [(Type,Transition)]
> makeExc c cs = if (null.findSubContent) c then [] else (mkEp (findSubContent c) (flowsMsg cs)) 
>		where mkEp [] _ = []
>		      mkEp (c:cs) ss = if ((getName c) == "Intermediate") && ((getProperty c "Interrupt") == "true") 
>				       then [(getET c,head (mkFlow "from" (getId c) ss))]++(mkEp cs ss)
>				       else (mkEp cs ss)
>		      getET c = case (getProperty c "Trigger") of
>				"Timer" -> (Itime (getXTime c))
>				"Exception" -> Ierror (getException c)  
>				"Message" -> let mf = (makeRec c cs) 
>					     in if null mf then (Imessage Nothing) 
>						else (Imessage (Just (head mf)))

> getException :: Content -> Exception
> getException c = case (getProperty c "exception") of
>			"" -> NoException
>			i -> Exception (mkInt i)

