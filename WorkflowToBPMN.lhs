> module WorkflowToBPMN (wTob,justSProcess) where

> import BPMN
> import Workflow
> import Char (isDigit,toUpper,toLower)
> import IO (writeFile)
> import BPMNToWorkflow (idsBname,sSuc)
> import List (find,findIndices,(\\),intersect,delete)

> testS =
>	[SubProcess (State (Bpmn "A1" SequenceB) [] [] [] (ZERO,ZERO) [] [] [] [] [] 0)
>        	    [Atomic [(State Start [] [(True,"line1")] [] (ZERO,ZERO) [] [] [] [] [] 0),
>		    	     (State (End 10) [(True,"line10")] [] [] (ZERO,ZERO) [] [] [] [] [] 0),
>	                     (State Agate [(True,"line1")] [(True,"line2"),(True,"line3")] [] (ZERO,ZERO) [] [] [] [] [] 0),
>	                     (State Agate [(True,"line13"),(True,"line9")] [(True,"line10")] [] (ZERO,ZERO) [] [] [] [] [] 0)],
>	             SubProcess (State (Bpmn "A1_Obv" DependentB) [(True,"line3")] [(True,"line9")] [] (ZERO,ZERO) [] [] [] [] [] 0)
>	                        [Atomic [(State Start [] [(True,"line4")] [] (ZERO,ZERO) [] [] [] [] [] 0),
>	 	                         (State (End 8) [(True,"line8")] [] [] (ZERO,ZERO) [] [] [] [] [] 0),
>		                         (State (Miseq "A1_ec_submission" (Fix 4) StandardT NoCond) [(True,"line4")] [(True,"line5")] [] (MkTime Pve 0 0 21 0 0 0,MkTime Pve 0 0 21 0 0 0) [] [] [] [] [] 0),
>		                         (State (Task "A1_mid_submission" StandardT) [(True,"line5")] [(True,"line6")] [] (MkTime Pve 0 0 0 0 0 0,MkTime Pve 0 0 0 0 0 0) [] [] [] [] [] 0),
>		                         (State (Miseq "A1_t_submission" (Fix 4) StandardT NoCond) [(True,"line6")] [(True,"line7")] [] (MkTime Pve 0 0 14 0 0 0,MkTime Pve 0 0 14 0 0 0) [] [] [] [] [] 0),
>		                         (State (Task "A1_end_submission" StandardT) [(True,"line7")] [(True,"line8")] [] (MkTime Pve 0 0 0 0 0 0,MkTime Pve 0 0 0 0 0 0) [] [] [] [] [] 0)]],
>	             SubProcess (State (Bpmn "A1_work" InterventionB) [(True,"line2")] [(True,"line13")] [] (ZERO,ZERO) [] [] [] [] [] 0)
>	                        [Atomic [(State Start [] [(True,"line3")] [] (ZERO,ZERO) [] [] [] [] [] 0),
>		                         (State (End 12) [(True,"line12")] [] [] (ZERO,ZERO) [] [] [] [] [] 0)],
>	                         SubProcess (State (Miseqs "A1_EC" (Fix 4) InterventionB NoCond) [(True,"line3")] [(True,"line9")] [] (ZERO,ZERO) [] [] [] [] [] 0)
>   	                                    [Atomic [(State (Stime (MkTime Pve 0 0 21 0 0 0)) [] [(True,"line4")] [] (ZERO,ZERO) [] [] [] [] [] 0),
>               				      (State (End 8) [(True,"line8")] [] [] (ZERO,ZERO) [] [] [] [] [] 0),
>				                      (State Agate [(True,"line4")] [(True,"line5"),(True,"line6")] [] (ZERO,ZERO) [] [] [] [] [] 0),
>				                      (State Agate [(True,"line6"),(True,"line7")] [(True,"line8")] [] (ZERO,ZERO) [] [] [] [] [] 0),
>				                      (State (Task "A1_EC_E" (InterventionT (Intervention "Epirubicin" "90" "slow push into fast drip"))) [(True,"line5")] [(True,"line6")] [] (MkTime Pve 0 0 0 0 0 0,MkTime Pve 0 0 0 0 0 0) [] [] [] [] [] 0),
>				                      (State (Task "A1_EC_C" (InterventionT (Intervention "cyclophosphamide" "600" "slow push infusion"))) [(True,"line6")] [(True,"line7")] [] (MkTime Pve 0 0 0 0 0 0,MkTime Pve 0 0 0 0 0 0) [] [] [] [] [] 0)]],
>	                	 SubProcess (State (Miseqs "A1_T" (Fix 4) InterventionB NoCond) [(True,"line9")] [(True,"line12")] [] (ZERO,ZERO) [] [] [] [] [] 0)
>                                     	     [Atomic [(State (Stime (MkTime Pve 0 0 14 0 0 0)) [] [(True,"line10")] [] (ZERO,ZERO) [] [] [] [] [] 0),
>				                      (State (End 11) [(True,"line11")] [] [] (ZERO,ZERO) [] [] [] [] [] 0),
>				                      (State (Task "A1_T_T" (InterventionT (Intervention "Paclitaxel" "175" "3 hour infusion"))) [(True,"line10")] [(True,"line11")] [] (MkTime Pve 0 0 0 0 0 0,MkTime Pve 0 0 0 0 0 0) [] [] [] [] [] 0)]]]]]

> testW =
>	[Event START (Pa (Id "BEGIN")) (De (Id "A1")) None None (SequentialD [Da (STOP,Dur "P0D",Dur "P0D",Rep 0,None,Manual)]) [] NoWork,
>	 Event (Id "A1") (Pa START) (De NORMAL_STOP) None None (SequentialD [Da (Id "A1_EC_SUBMISSION",Dur "P21D",Dur "P21D",Rep 4,None,Manual),Da (Id "A1_MID_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual),Da (Id "A1_T_SUBMISSION",Dur "P14D",Dur "P14D",Rep 4,None,Manual),Da (Id "A1_END_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual),Da (NORMAL_STOP,Dur "P0D",Dur "P0D",Rep 0,None,Manual)]) [] (SequentialW [Wk (WorkMult("A1_EC",ParWB [WorkUnit ("A1_EC_E",Treatment "Epirubicin" "90" "slow push into fast drip",Dur "P0D",Dur "P0D"),WorkUnit ("A1_EC_C",Treatment "cyclophosphamide" "600" "slow push infusion",Dur "P0D",Dur "P0D")],Dur "P21D",Rep 4)),Wk (WorkMult ("A1_T",WorkUnit ("A1_T_T",Treatment "Paclitaxel" "175" "3 hour infusion",Dur "P0D",Dur"P0D"),Dur "P14D",Rep 4))]),
>	 Event NORMAL_STOP (Pa (Id "A1")) (De (Id "END")) None None (SequentialD [Da (STOP,Dur "P0D",Dur "P0D",Rep 0,None,Manual)]) [] NoWork]

	[Event START (Pa (Id "BEGIN")) (De (Id "ELIGIBILITY")) None None (SequentialD [Da (STOP,Dur "P0D",Dur "P0D",Rep 0,None,Manual)]) [] NoWork,
	 Event (Id "ELIGIBILITY") (Pa START) (De (Id "RANDOMISATION")) None None (SequentialD [Da (Id "ELIGIBILITY_SUBMISSION",Dur "P0D",Dur "P14D",Rep 1,None,Manual)]) [] NoWork,
	 Event (Id "RANDOMISATION") (Pa (Id "ELIGIBILITY")) (OneOfD [De (Id "A1"),De (Id "A2"),De (Id "B2"),De (Id "B1")]) None None (SequentialD [ParD [SequentialD [Da (Id "ONSTUDY_SUBMISSION",Dur "P21D",Dur "P21D",Rep 1,None,Manual)],SequentialD [Da (Id "RANDOMISATION_SUBMISSION",Dur "P21D",Dur "P21D",Rep 1,None,Manual)]]]) [] (Wk (WorkMult ("Randomising",WorkUnit ("Randomise",NoTreatment,Dur "P0D",Dur "P0D"),Dur "P21D",Rep 1))),
	 Event (Id "FOLLOWUP") (OneOf [Pa (Id "A1"),Pa (Id "A2"),Pa (Id "B2"),Pa (Id "B1")]) (De NORMAL_STOP) None None (SequentialD [Da (Id "FOLLOWUP_SUBMISSION_1",Dur "P3M",Dur "P3M",Rep 4,None,Manual),Da (Id "FOLLOWUP_SUBMISSION_2",Dur "P4M",Dur "P4M",Rep 3,None,Manual),Da (Id "FOLLOWUP_SUBMISSION_3",Dur "P6M",Dur "P6M",Rep 5,None,Manual),Da (NORMAL_STOP,Dur "P0D",Dur"P0D",Rep 0,None,Manual)]) [] NoWork,
	 Event (Id "A1") (Pa (Id "RANDOMISATION")) (De (Id "FOLLOWUP")) None None(SequentialD [Da (Id "A1_EC_SUBMISSION",Dur "P21D",Dur "P21D",Rep 4,None,Manual),Da (Id "A1_MID_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual),Da (Id "A1_T_SUBMISSION",Dur "P14D",Dur "P14D",Rep 4,None,Manual),Da (Id"A1_END_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual)]) [] (SequentialW [Wk (WorkMult ("A1_EC",ParWB [WorkUnit ("A1_EC_E",Treatment "Epirubicin" "90" "slow push into fast drip",Dur "P0D",Dur "P0D"),WorkUnit ("A1_EC_C",Treatment "cyclophosphamide" "600" "slow push infusion",Dur "P0D",Dur "P0D")],Dur "P21D",Rep 4)),Wk (WorkMult ("A1_T",WorkUnit ("A1_T_T",Treatment "Paclitaxel" "175" "3 hour infusion",Dur "P0D",Dur "P0D"),Dur "P14D",Rep 4))]),
	 Event (Id "A2") (Pa (Id "RANDOMISATION")) (De (Id "FOLLOWUP")) None None (SequentialD [Da (Id "A2_T_SUBMISSION",Dur "P14D",Dur "P14D",Rep 4,None,Manual),Da (Id "A2_MID_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual),Da (Id "A2_EC_SUBMISSION",Dur "P21D",Dur "P21D",Rep 4,None,Manual),Da (Id "A2_END_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual)]) [] (SequentialW [Wk (WorkMult ("A2_T",WorkUnit ("A2_T_T",Treatment "Paclitaxel" "175" "3 hour infusion",Dur "P0D",Dur "P0D"),Dur "P14D",Rep 4)),Wk (WorkMult ("A2_EC",ParWB [WorkUnit ("A2_EC_E",Treatment "Epirubicin" "90" "slow push into fast drip",Dur "P0D",Dur "P0D"),WorkUnit ("A2_EC_C",Treatment "cyclophosphamide" "600" "slow push infusion",Dur "P0D",Dur "P0D")],Dur "P21D",Rep 4))]),
	 Event (Id "B2") (Pa (Id "RANDOMISATION")) (De (Id "FOLLOWUP")) None None (SequentialD [Da (Id "B2_T_SUBMISSION",Dur "P14D",Dur "P14D",Rep 4,None,Manual),Da (Id "B2_MID_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual),Da (Id "B2_EC_SUBMISSION",Dur "P21D",Dur "P21D",Rep 4,None,Manual),Da (Id "B2_END_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual)]) [] (SequentialW [Wk (WorkMult ("B2_TG",SequentialWB [WorkUnit ("B2_TG_T",Treatment "Paclitaxel" "175" "3hour infusion",Dur "P0D",Dur "P0D"),WorkUnit ("B2_TG_G",Treatment "Gemcitabine" "2000" "60 minute infusion",Dur "P0D",Dur "P0D")],Dur "P14D",Rep 4)),Wk (WorkMult ("B2_EC",ParWB [WorkUnit ("B2_EC_E",Treatment "Epirubicin" "90" "slow push into fast drip",Dur "P0D",Dur "P0D"),WorkUnit ("B2_EC_C",Treatment "cyclophosphamide" "600" "slow push infusion",Dur "P0D",Dur "P0D")],Dur "P21D",Rep 4))]),
	 Event (Id "B1") (Pa (Id "RANDOMISATION")) (De (Id "FOLLOWUP")) None None (SequentialD [Da (Id "B1_EC_SUBMISSION",Dur "P21D",Dur "P21D",Rep 4,None,Manual),Da(Id "B1_MID_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual),Da (Id "B1_TG_SUBMISSION",Dur "P14D",Dur "P14D",Rep 4,None,Manual),Da (Id "B1_END_SUBMISSION",Dur "P0D",Dur "P0D",Rep 1,None,Manual)]) [] (SequentialW [Wk (WorkMult ("B1_EC",ParWB [WorkUnit ("B1_EC_E",Treatment "Epirubicin" "90" "slow push into fast drip",Dur "P0D",Dur "P0D"),WorkUnit ("B1_EC_C",Treatment "cyclophosphamide" "600" "slow push infusion",Dur "P0D",Dur "P0D")],Dur "P21D",Rep 4)),Wk (WorkMult ("B1_TG",SequentialWB [WorkUnit ("B1_TG_T",Treatment "Paclitaxel" "175" "3 hourinfusion",Dur "P0D",Dur "P0D"),WorkUnit ("B1_TG_G",Treatment "Gemcitabine" "2000" "60 minute infusion",Dur "P0D",Dur "P0D")],Dur "P14D",Rep 4))]),
	 Event NORMAL_STOP (Pa (Id "FOLLOWUP")) (De (Id "END")) None None (SequentialD [Da (STOP,Dur "P0D",Dur "P0D",Rep 0,None,Manual)]) [] NoWork]

> testSS = (snd.head.wTob) testW
> testL = "line25"

> justSProcess :: Workflow -> [[StateSet]]
> justSProcess w = [] 

 justSProcess = snd.unzip.wTob

> firstLine :: Line
> firstLine = "line1"

 wTobf :: Workflow -> FilePath -> IO()
 wTobf w f = do { writeFile f ("> testBPMN2ANDNODEPEND = "++(show (wTob w)))}

 wTob w = (fst fr)
 
 [((head.snd) fr,(fst fr))]
 
 testR = concat [ if isAtomic x then getAtomic [x] else [(fst.getSP) x] | x <- (snd.head.wTob) test ]

> wTob :: Workflow -> BPMN
> wTob w = (buildDependency w (fst fr) ((head.snd) fr))

>	where fr = (unzip.(foldl cst [])) w
>	      cst (x:xs) w  = if elem (getNme w) specialId then (x:xs) 
>			      else [createStates w ((snd.head) (x:xs))]++(x:xs)
>	      cst [] w  = if elem (getNme w) specialId then [] else [createStates w firstLine]

 wTob w = [("Test",fst fr)]
 
> result = [State Start [] [(True,"line67")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State Xgate [(True,"line67")] [(True,"line69"),(True,"line70")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State (Bpmn "G" SequenceB) [(True,"line70")] [(True,"line52")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State (Bpmn "H" SequenceB) [(True,"line69")] [(True,"line53")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State Xgate [(True,"line52"),(True,"line53")] [(True,"line50")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State (Bpmn "A" SequenceB) [(True,"line50")] [(True,"line33")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State Xgate [(True,"line33")] [(True,"line35"),(True,"line36")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State (Bpmn "F" SequenceB) [(True,"line36")] [(True,"line65")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State (Bpmn "B" SequenceB) [(True,"line35")] [(True,"line64")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State Xgate [(True,"line64"),(True,"line65")] [(True,"line62")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State Xgate [(True,"line62")] [(True,"line58")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State Agate [(True,"line58")] [(True,"line46"),(True,"line47")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State (Bpmn "D" SequenceB) [(True,"line47")] [(True,"line57")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State (Bpmn "E" SequenceB) [(True,"line46")] [(True,"line56")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State Agate [(True,"line56"),(True,"line57")] [(True,"line54")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State (Bpmn "C" SequenceB) [(True,"line54")] [(True,"line66")] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State (End 66) [(True,"line66")] [] [] (ZERO,ZERO) [] [] [] [] [] 0,
>	    State Agate [(True,"line37")] [(True,"line39"),(True,"line40")] [] (ZERO,ZERO) [] [] [] [] [] 0]



> createStates :: EventSequencing -> Line -> (StateSet,Line)
> createStates (Event id pre dpe cc ec dpt reps wk) ln = ((SubProcess (State (Bpmn (idToBName id) SequenceB) [] [] [] (ZERO,ZERO) [] [] [] [] [] 0) s),l)
>			where (s,l) = (cInternal dpt reps wk ln id)

> cInternal :: DptAct -> [RepeatExp] -> Works -> Line -> ActivityId -> ([StateSet],Line)
> cInternal dpt reps wk ln id
>	| wk == NoWork = ([(Atomic [start,end])]++[sdpt]++sreps,el)
>	| otherwise = ([(Atomic ([start,end,aps,aj]))]++[sdpt]++sreps++swks,el)
>		where (start,sl) = mkstate Start ln []
>		      (sdpt,dnl) = if wk == NoWork then createDptBlk (bkLine sl) dpt id 
>				   else createDptBlk ((snd.last.getOut) aps) dpt id
>		      (sreps,rnl) = extReps dnl sdpt id reps
>		      (swks,wnl) = if wk == NoWork then ([],"") else (extWk ((snd.head.getOut) aps) dnl wk id)

		      (swks,wnl) = if wk == NoWork then ([],"") else (extWk ((snd.head.getOut) aps) ((inLine.snd.last.getOut) aps) wk id)

>		      (end,el) = if wk == NoWork then mkstate (End (read (snd (splitAt 4 rnl)) :: Int)  ) rnl []
>				 else mkstate (End (read (snd (splitAt 4 jl)) :: Int)  ) jl []
>		      (aps,al) = crtDNode (Left [ln]) (Right 2) "" Agate
>		      (aj,jl) = crtDNode (Left [wnl,rnl]) (Right 1) "" Agate

 testcInt = cInternal (SequentialD [Da (Id "RANDOMISATION FORM SUBMISSION",Dur "14D",Dur "",None),Da (NORMAL_STOP,Dur "",Dur "",None)])
		       [] (SequentialW [Wk "Run Randomisation"]) firstLine (Id "RANDOMISATION")

> createDptBlk :: Line -> DptAct -> ActivityId -> (StateSet,Line)
> createDptBlk ln dpt id = ((SubProcess (State (Bpmn ((idToBName id)++"_Obv") DependentB) [(True,ln)] [(True,el)] [] (ZERO,ZERO) [] [] [] [] [] 0) complete), el) 
>	where (start,sl) = mkstate Start (inLine ln) []
>	      (dpin,out) = extDpt (bkLine sl) sl dpt
>	      (end,el) = mkstate (End (read (snd (splitAt 4 out)) :: Int)) out []
>	      complete = [(Atomic ([start,end]++dpin))]

> extDpt :: Line -> Line -> DptAct -> ([State],Line)
> extDpt ln nl dpt = case dpt of
>			NoDepend -> ([],ln)
>			(ChoiceD dpts) -> (extDptM Xgate ln nl dpts) 
>			(ParD dpts) -> (extDptM Agate ln nl dpts)
>			(SequentialD dpts) -> ((concat.fst) seqs, (last.snd) seqs) where seqs = (unzip . (extDptS ln nl)) dpts
>			(Da act) -> if (elem (fst6 act) specialId) then ([],ln) else mkDpt act ln nl

> extDptS :: Line -> Line -> [DptAct] -> [([State],Line)]
> extDptS ln nl [] = []
> extDptS ln nl (d:dpts) = (ss,sl):(extDptS sl (inLine sl) dpts) 
>	where (ss,sl) = (extDpt ln nl d) 

> extDptM :: Type -> Line -> Line -> [DptAct] -> ([State],Line)
> extDptM t ln nl dpts = (([sp,j]++(concat dss)),jl)
>	where (sp,xpl) = crtDNode (Left [ln]) (Left (inLines nl (length dpts))) "" t
>	      (dss,dl) = unzip (createDptM dpts ((snd.unzip.getOut) sp) ((inLine.last.snd.unzip.getOut) sp))
>	      (j,jl) = crtDNode (Left dl) (Right 1) "" t

> createDptM :: [DptAct] -> [Line] -> Line -> [([State],Line)]
> createDptM _ [] _ = []
> createDptM [] _ _ = []
> createDptM (d:dpts) (l:lns) nl = [(nd,dl)]++(createDptM dpts lns (inLine dl))
>	where (nd,dl) = (extDpt l nl d)

Creating a single Dependent activity

> mkDpt :: Activity -> Line -> Line -> ([State],Line)
> mkDpt (id,min,max,rp,cond,atype) ln nl =
>	if rp == Rep 1 then ([(State (Task (idToTName id) StandardT) [(True,ln)] [(True,nl)] [] ((mkTime min),(mkTime max)) [] [] [] [] [] 0)],nl)
>	else ([(State (Miseq (idToTName id) (Fix (mkRep rp)) StandardT (getBCond cond)) [(True,ln)] [(True,nl)] [] ((mkTime min),(mkTime max)) [] [] [] [] [] 0)],nl)

Using function getXT from module XMLToBPMN to extract Workflow's duration into BPMN's time

> mkTime :: Duration -> Time
> mkTime UNBOUNDED = NOBOUND
> mkTime (Dur dur) = getXT dur

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

Creating a non-decision state

> mkstate :: Type -> Line -> [Type] -> (State,Line)
> mkstate t l ts
>	| (t == Start || isStime t) = ((State t [] [(True,l)] [] (ZERO,ZERO) [] [] [] [] [] 0),next) 
>	| isTask t && null ts = ((State t [(True,l)] [(True,next)] [] (ZERO,ZERO) [] [] [] [] [] 0),inLine next)
>	| isTask t = ((State t [(True,l)] [(True,next)] except (ZERO,ZERO) [] [] [] [] [] 0),inLine ((snd.snd.last) except))
>	| isEnd t = ((State t [(True,l)] [] [] (ZERO,ZERO) [] [] [] [] [] 0),next)
>	where next = inLine l
>	      except = (zip ts (mkTrans (inLines next (length ts))))

	      except = ((zip ts).(map mkS)) (mkTrans (inLines next (length ts)))
	      mkS a = [a]

Creating states of type Xgate and Agate

> crtDNode :: Either [Line] Int -> Either [Line] Int -> Line -> Type -> (State,Line)
> crtDNode (Left a) (Right b) l t = (createDNode 0 b t a []) 
> crtDNode (Right a) (Right b) l t = (createDNode a b t [l] [])
> crtDNode (Right a) (Left b) l t = (createDNode a 0 t b [])
> crtDNode (Left a) (Left b) l t = (createDNode 0 0 t a b)

 crtDNode (Left a) (Right b) l t = Just (createDNode 0 b t a) 
 crtDNode (Right a) (Right b) l t = Just (createDNode a b [l] t)
 crtDNode (Right a) (Left b) l t = Nothing
 crtDNode (Left a) (Left b) l t = Nothing


> createDNode :: Int -> Int -> Type -> [Line] -> [Line] -> (State,Line)
> createDNode inc out t lns lnss
>	| (inc == 0 && out == 0) = ((State t (mkTrans lns) (mkTrans lnss) [] (ZERO,ZERO) [] [] [] [] [] 0),last lnss)
>	| inc == 0 = ((State t (mkTrans lns) (mkTrans aOut) [] (ZERO,ZERO) [] [] [] [] [] 0),last aOut)
>	| out == 0 = ((State t (mkTrans allIn) (mkTrans lns) [] (ZERO,ZERO) [] [] [] [] [] 0),last allIn)
>	| otherwise = ((State t (mkTrans allIn) (mkTrans allOut) [] (ZERO,ZERO) [] [] [] [] [] 0),last allOut) 
>		where allIn = (inLines (last lns) inc)
>		      allOut = (inLines (last allIn) out)
>		      aOut = (inLines (last lns) out)

Convert a line into a transition

> mkTrans :: [Line] -> [Transition]
> mkTrans lns = zip (replicate (length lns) True) lns

Create new line(s)

> inLines :: Line -> Int -> [Line]
> inLines ln 0 = []
> inLines ln n = inc:(inLines inc (n-1)) 
>	where inc = (inLine ln) 

> inLine :: Line -> Line
> inLine ln = (fst cur)++(show ((read (snd cur) :: Int)+1))
>	where cur = (splitAt 4) ln

Backtrack the previous line

> bkLine :: Line -> Line
> bkLine ln = (fst cur)++(show ((read (snd cur) :: Int)-1))
>	where cur = (splitAt 4) ln

Comparing two line(s)

> gt :: Line -> Line -> Bool
> l1 `gt` l2 = (read ((drop 4) l1) :: Int) > (read ((drop 4) l2) :: Int)

Repeat

> extReps :: Line -> StateSet -> ActivityId -> [RepeatExp] -> ([StateSet],Line)
> extReps l s a rs = if null rs then ([],l) else (ss,last ll)
>	where (ss,ll) = unzip (extRepS l s a rs)

> extRepS :: Line -> StateSet -> ActivityId -> [RepeatExp] -> [(StateSet,Line)]
> extRepS ln _ _ [] = []
> extRepS ln sst id (rp:reps) = (rep,rl):(extRepS rl sst id (reps)) 
>	where (rep,rl) = (extRep ln sst id rp) 

> extRep :: Line -> StateSet -> ActivityId -> RepeatExp -> (StateSet,Line)
> extRep ln s id (min,max,minl,maxl,cond) = ((SubProcess (State (Miseqs ((idToBName id)++" Repeat") (mkReps minl maxl) RepeatB (getBCond cond)) 
>								  [(True,ln)] [(True,(inLine.snd.head.getIn) end)] [] ((mkTime min),(mkTime max)) [] [] [] [] [] 0) 
>					  	     	 [(Atomic [start,end]),dep]),(inLine.snd.head.getIn) end)
>	where (start,sl) = mkstate Start (inLine ln) []
>	      dep = (changeStateSetTrans s [(True,sl)] [(True,inLine sl)])
>	      (end,el) = mkstate (End (read (snd (splitAt 4 ((snd.head.getOut.fst.getSP) dep))) :: Int)) (inLine sl) []

 extRep :: Line -> StateSet -> ActivityId -> RepeatExp -> (StateSet,Line)
 extRep ln s id (min,max,minl,maxl,cond) = ((SubProcess (State (Miseqs ((idToBName id)++" Repeat") (mkRep maxl) RepeatB (getBCond cond)) 
								  [(True,ln)] [(True,(inLine.snd.head.getIn) end)] [] [] [] 0) 
					  	     	 [(Atomic [stime,delay,xjoin,end]),dep]),(inLine.snd.head.getIn) end)
	where (stime,sl) = mkstate (Stime (mkTime min)) (inLine ln) []
	      (delay,dl) = mkstate (Task "Delay" StandardT) sl [(Itime (mkTime max))]
	      (xjoin,xl) = crtDNode (Left [((snd.head.getOut) delay),dl]) (Right 1) "" Xgate
	      dep = (changeStateSetTrans s [(True,xl)] [(True,inLine xl)])
	      (end,el) = mkstate (End (read (snd (splitAt 4 ((snd.head.getOut.fst.getSP) dep))) :: Int)) (inLine xl) []


Convert Repeat into numeral (incomplete)

> mkReps :: Repeat -> Repeat -> Loops
> mkReps (Rep 0) (Rep i) = Ndet i
> mkReps (Rep i) (Rep j) = Fix j

> mkRep :: Repeat -> Int
> mkRep (Rep x) = x
> mkRep Any = 1

Works

> extWk :: Line -> Line -> Works -> ActivityId -> ([StateSet],Line)
> extWk ln nl wk id = ([(SubProcess (State (Bpmn ((idToBName id)++"_work") InterventionB) [(True,ln)] [(True,el)] [] (ZERO,ZERO) [] [] [] [] [] 0) complete)], el)
>		where (start,sl) = mkstate Start (inLine nl) [] 
>		      (wkin,out) = extWks (bkLine sl) sl wk
>	      	      (end,el) = mkstate (End (read (snd (splitAt 4 out)) :: Int)) out []
>	      	      complete = [(Atomic ([start,end]++(getAtomic wkin)))]++[ w | w <- wkin, (not.isAtomic) w ]

> extWks :: Line -> Line -> Works -> ([StateSet],Line)
> extWks ln nl wk = case wk of
>			NoWork -> ([],ln)
>			(Wk (WorkSingle (w,t,m,n,i))) -> let (s,nll) = mkWk w t m n (mkReps i i) ln nl in ([(Atomic s)],nll)
>			(Wk (WorkMult wb)) -> let (x,y) = extWkB ln nl wb in ([x],y)
>			(ChoiceW wks) -> (extWkM Xgate ln nl wks) 
>			(ParW wks) -> (extWkM Agate ln nl wks)
> 			(SequentialW wks) -> let (ss,ls) = (unzip . (extWkS ln nl)) wks
>						 sort = [(Atomic [ y | s <- (concat ss), isAtomic s, y <- (getAtomic [s]) ])] ++ [ s | s <- (concat ss), (not.isAtomic) s ]
>					     in	(sort,last ls) 

> extWkS :: Line -> Line -> [Works] -> [([StateSet],Line)]
> extWkS ln nl [] = []
> extWkS ln nl (w:wks) = (ww,wl):(extWkS wl (inLine wl) wks) 
>	where (ww,wl) = (extWks ln nl w)

> extWkM :: Type -> Line -> Line -> [Works] -> ([StateSet],Line)
> extWkM t ln nl wks = (wsort,jl)
>	where (sp,xpl) = crtDNode (Left [ln]) (Left (inLines nl (length wks))) "" t
>	      (wss,wl) = unzip (createWkM wks ((snd.unzip.getOut) sp) ((inLine.last.snd.unzip.getOut) sp))
>	      (j,jl) = crtDNode (Left wl) (Right 1) "" t
>	      wsort = [(Atomic ([sp,j]++[ y | s <- (concat wss), isAtomic s, y <- (getAtomic [s]) ]))] ++ [ s | s <- (concat wss), (not.isAtomic) s ]

> createWkM :: [Works] -> [Line] -> Line -> [([StateSet],Line)]
> createWkM _ [] _ = []
> createWkM [] _ _ = []
> createWkM (w:wks) (l:lns) nl = [(nw,wl)]++(createWkM wks lns (inLine wl))
>	where (nw,wl) = (extWks l nl w)

 extWks ln nl NoWork = ([],ln)
 extWks ln nl (ChoiceW wks) = (extWkM Xgate ln nl wks) 
 extWks ln nl (ParW wks) = (extWkM Agate ln nl wks)
 extWks ln nl (SequentialW wks) = ((concat.fst) seqs, (last.snd) seqs) where seqs = (unzip . (extWkS ln nl)) wks
 extWks ln nl (Wk (WorkMult (w,t,m,i))) = if (ln == nl) then ([ws],wl) else ([(changeStateTrans ws [] [(True,nl)])],nl) 
 				  where ([ws],wl) = extWksB w t m (mkReps i i) ln
 extWks ln nl (Wk (WorkSingle (w,t,m,n,i))) = if (ln == nl) then ([ws],wl) else ([(changeStateTrans ws [] [(True,nl)])],nl) 
				  where ([ws],wl) = mkWk w t m n (mkReps i i) ln

 extWkS :: Line -> Line -> [Works] -> [([State],Line)]
 extWkS ln nl [] = []
 extWkS ln nl (w:wks) = (ww,wl):(extWkS wl wl wks) 
	where (ww,wl) = (extWks ln nl w) 

 extWkM :: Type -> Line -> Line -> [Works] -> ([State],Line)
 extWkM t ln nl wks = (([sp,j]++(concat wss)),jl)
	where (sp,xpl) = crtDNode (Left [ln]) (Left (inLines (bkLine nl) (length wks))) "" t
	      (wss,wl) = unzip (createWkM wks ((snd.unzip.getOut) sp))
	      (j,jl) = crtDNode (Left wl) (Right 1) "" t

 createWkM :: [Works] -> [Line] -> [([State],Line)]
 createWkM _ [] = []
 createWkM [] _ = []
 createWkM (w:wks) (l:lns) = [(extWks l l w)]++(createWkM wks lns)

WorkBlock

> extWkB :: Line -> Line -> WorkMBlock -> (StateSet,Line)
> extWkB ln nl (id,bk,md,rp) = 
>	((SubProcess (State (Miseqs (workToTask id) (mkReps rp rp) InterventionB NoCond) [(True,ln)] [(True,el)] [] (ZERO,ZERO) [] [] [] [] [] 0) complete), el)
>	where (start,sl) = if (mkTime md) == ZERO then mkstate Start nl [] 
>			   else mkstate (Stime (mkTime md)) nl []
>	      (wkin,out) = extWksB (bkLine sl) sl bk
>	      (end,el) = mkstate (End (read (snd (splitAt 4 out)) :: Int)) out []
>	      complete = [(Atomic ([start,end]++wkin))]

> extWksB :: Line -> Line -> WorkBlock -> ([State],Line)
> extWksB ln nl NoWorkB = ([],ln)
> extWksB ln nl (ChoiceWB wks) = (extWkMB Xgate ln nl wks) 
> extWksB ln nl (ParWB wks) = (extWkMB Agate ln nl wks)
> extWksB ln nl (SequentialWB wks) = ((concat.fst) seqs, (last.snd) seqs) where seqs = (unzip . (extWkSB ln nl)) wks
> extWksB ln nl (WorkUnit (w,t,m,n)) = mkWk w t m n (Fix 0) ln nl

> extWkSB :: Line -> Line -> [WorkBlock] -> [([State],Line)]
> extWkSB ln nl [] = []
> extWkSB ln nl (w:wks) = (ww,wl):(extWkSB wl (inLine wl) wks) 
>	where (ww,wl) = (extWksB ln nl w) 

> extWkMB :: Type -> Line -> Line -> [WorkBlock] -> ([State],Line)
> extWkMB t ln nl wks = (([sp,j]++(concat wss)),jl)
>	where (sp,xpl) = crtDNode (Left [ln]) (Left (inLines nl (length wks))) "" t
>	      (wss,wl) = unzip (createWkMB wks ((snd.unzip.getOut) sp) ((inLine.last.snd.unzip.getOut) sp)) 
>	      (j,jl) = crtDNode (Left wl) (Right 1) "" t

> createWkMB :: [WorkBlock] -> [Line] -> Line -> [([State],Line)]
> createWkMB _ [] _ = []
> createWkMB [] _  _ = []
> createWkMB (w:wks) (l:lns) nl = [(nwb,wl)]++(createWkMB wks lns (inLine wl))
>	where (nwb,wl) = (extWksB l nl w)


Creating a single intervention

> mkWk :: WorkId -> Treatment -> Duration -> Duration -> Loops -> Line -> Line -> ([State],Line)
> mkWk w t min max i ln nl  =
>	let 
>	 tt = case t of
>		 NoTreatment -> InterventionT NoDetails
>		 Treatment n q m -> (InterventionT (Intervention n q m)) 
>	in if getLoops i == 0 then ([ (State (Task (workToTask w) tt) [(True,ln)] [(True,nl)] [] (mkTime min,mkTime max) [] [] [] [] [] 0) ], nl)
>	   else ([ (State (Miseq (workToTask w) i tt NoCond) [(True,ln)] [(True,nl)] [] (mkTime min,mkTime max) [] [] [] [] [] 0) ], nl)

GetCondition (incomplete)

> getBCond :: Condition -> BCondition
> getBCond _ = NoCond

Building dependency (transition between subprocess states)

> buildDependency :: Workflow -> [StateSet] -> Line -> BPMN
> buildDependency w ss ln = if False then (error.show) ss else [ ("Pool",(normal clean1 clean1))]
>	where (bulk,bl) = (makeControl w ss ln)
>	      (wends,el) = makeEnds w bulk bl
>	      (wstart,sl) = makeStart w wends el
>	      clean1 = noredun wstart

> makeEnds :: Workflow -> [StateSet] -> Line -> ([StateSet],Line)
> makeEnds w ss ln = makeJoin eq ess ln (inLine ln)
>	where ess = [(Atomic ((getAtomic ss)++[end]))]++(filter (not.isAtomic) ss)
>	      (end,el) = mkstate (End (read (snd (splitAt 4 ln)) :: Int)) ln []
>	      eq = (getPr.head) [ s | s <- w, getNme s == NORMAL_STOP ]

> makeStart :: Workflow -> [StateSet] -> Line -> ([StateSet],Line)
> makeStart w ss ln = makeSplit sq ess ln (inLine ln)
>	where ess = [(Atomic ((getAtomic ss)++[start]))]++(filter (not.isAtomic) ss)
>	      (start,sl) = (mkstate Start ln [])
>	      sq = (getDe.head) [ s | s <- w, getNme s == START ]

> makeControl :: [EventSequencing] -> [StateSet] -> Line -> ([StateSet],Line)
> makeControl es ss ln = (jss,jl)
>	where (jss,jl) = makeJoins es nss nl
>	      (nss,nl) = makeSplits es ss ln

> makeSplits :: [EventSequencing] -> [StateSet] -> Line -> ([StateSet],Line)
> makeSplits [] ss ln = (ss,ln)
> makeSplits (e:es) ss ln = 
>	let cs = [ s | s <- ss, (not.isAtomic) s && chknme (getNme e) s ]
>	    ncs = [(changeStateSetTrans (head cs) [] [(True,ln)])]++(delete (head cs) ss)
>	    (nss,nl) = makeSplit ((normDve.getDe) e) ncs ln (inLine ln)
>	in if getNme e /= NORMAL_STOP && getNme e /= START 
>	   then makeSplits es nss nl else makeSplits es ss ln

> makeSplit :: DptEvent -> [StateSet] -> Line -> Line -> ([StateSet],Line)
> makeSplit de ss ln rl =  
>	let nl = makeGate (Left de) ln rl
>	    gd (De id) = id
>	    gds (OneOfD ds) = ds
>	    gds (AllD ds) = ds
>	    ost = [ s | s <- ss, (not.isAtomic) s && (gd de) /= NORMAL_STOP && chknme (gd de) s ]
>	    nss = [Atomic ((getAtomic ss)++[(fst.head) nl])]++(filter (not.isAtomic) ss) 
>	in if null nl then if null ost then (ss,rl) else (([(changeStateSetTrans (head ost) [(True,ln)] [])]++(delete (head ost) ss)),rl)
>	   else makeSplit2 (zip (gds de) ((snd.unzip.getOut.fst.head) nl)) nss ((inLine.snd.head) nl)

> makeSplit2 :: [(DptEvent,Line)] -> [StateSet] -> Line -> ([StateSet],Line)
> makeSplit2 [] ss ln = (ss,ln)
> makeSplit2 ((d,l):ds) ss ln = makeSplit2 ds nss ls 
>	where (nss,ls) = makeSplit d ss l ln

> makeJoins :: [EventSequencing] -> [StateSet] -> Line -> ([StateSet],Line)
> makeJoins [] ss ln = (ss,ln)
> makeJoins (e:es) ss ln = 
>	let p = ((normPre.getPr) e)
>	    cs = (findIns ss ((fst.getSP.head) [ s | s <- ss, (not.isAtomic) s && chknme (getNme e) s ]))
>	    ncs = case cs of 
>		  	Left c -> [Atomic ([(changeStateTrans c [(True,ln)] [])]++(delete c (getAtomic ss)))]++(filter (not.isAtomic) ss)
>			Right c -> [(changeStateSetTrans c [(True,ln)] [])]++(delete c ss)
>	    (nss,nl) = makeJoin p ncs ln (inLine ln)
>	in case p of 
>		(Pa i) -> makeJoins es ss ln
>		pr -> makeJoins es nss nl

> findIns :: [StateSet] -> State -> (Either State StateSet)
> findIns ss s = 
>	let bs = filter ((elem ((head.getIn) s)).getOut) (allstates ss)
>	in if null bs then error "findIns" 
>	   else if isSubs s 
>		then if (not.isSubs.head) bs then findIns ss (head bs)
>		     else Right (head [ x | x <- ss, (not.isAtomic) x && (fst.getSP) x == s ])
>	        else if (isSubs.head) bs then Left s 
>		     else findIns ss (head bs)

> makeJoin :: PreAct -> [StateSet] -> Line -> Line -> ([StateSet],Line)
> makeJoin pe ss ln rl =  
>	let nl = makeGate (Right pe) ln rl
>	    gd (Pa id) = id
>	    gds (OneOf ps) = ps
>	    gds (All ps) = ps
>	    ost = [ s | s <- ss, (not.isAtomic) s && (gd pe) /= START && chknme (gd pe) s ]
>	    nss = [Atomic ((getAtomic ss)++[(fst.head) nl])]++(filter (not.isAtomic) ss) 
>	in if null nl 
>	   then if null ost then (ss,rl) 
>	   	else (([(changeStateSetTrans (head ost) [] [(True,ln)])]++(delete (head ost) ss)),rl)
>	   else makeJoin2 (zip (gds pe) ((snd.unzip.getIn.fst.head) nl)) nss ((inLine.snd.last.getIn.fst.head) nl)

			if or [ (head.getOut.fst.getSP.head) ost == (head.getIn) x | x <- getAtomic ss, (length.getIn) x == 1 && getType x == Xgate || getType x == Agate ]
			then (ss,rl) else 

> makeJoin2 :: [(PreAct,Line)] -> [StateSet] -> Line -> ([StateSet],Line)
> makeJoin2 [] ss ln = (ss,ln)
> makeJoin2 ((p,l):ps) ss ln = makeJoin2 ps nss ls 
>	where (nss,ls) = makeJoin p ss l ln

> makeGate :: (Either DptEvent PreAct) -> Line -> Line -> [(State,Line)]
> makeGate (Left (OneOfD ds)) ln rl = if length ds > 1 then [crtDNode (Left [ln]) (Left (inLines rl (length ds))) "" Xgate] else []
> makeGate (Left (AllD ds)) ln rl = if length ds > 1 then  [crtDNode (Left [ln]) (Left (inLines rl (length ds))) "" Agate] else []
> makeGate (Right (OneOf ps)) ln rl = if length ps > 1 then  [crtDNode (Left (inLines rl (length ps))) (Left [ln]) "" Xgate] else []
> makeGate (Right (All ps)) ln rl = if length ps > 1 then  [crtDNode (Left (inLines rl (length ps))) (Left [ln]) "" Agate] else []
> makeGate _ ln rl = []


 buildDependency :: Workflow -> [StateSet] -> Line -> BPMN
 buildDependency w ss ln = [ ("Pool",newss)]
	where (end,el) = mkstate (End (read (snd (splitAt 4 (inLine ln))) :: Int)) (inLine ln) []
	      (abort,abl) = mkstate (End (read (snd (splitAt 4 (inLine sl))) :: Int)) (inLine sl) []

	      newss = if null asst then ([(Atomic ([end]))]++nss) else ([(Atomic ([start,end,abort]))]++nss)

	      newss = if null asst then ([(Atomic ([start,end]))]++nss) else ([(Atomic ([start,end,abort]))]++nss)
	      
	      asst = (if apre == [] then [] else [ (fst (buildBMain ss abl ((getPr.head) apre) aprelist w)) ])

	      asst = []

	      (nss,sl) = (buildBMain ss (bkLine el) ((getPr.head) epre) eprelist START w)
	      (start,ll) = mkstate Start sl []
	      epre = (filter (equalNm NORMAL_STOP) w)
	      apre = (filter (equalNm ABNORMAL_STOP) w)
	      eprelist = preList w [] ((getPr.head) epre)
	      aprelist = preList w [] ((getPr.head) apre)


 buildtest = buildBMain testSTATES2ANDNODEPEND "line17" (All [Pa (Id "ELIGIBILITY"),Pa (Id "RANDOMISATION")]) 
		         [[START,Id "ELIGIBILITY"],[START,Id "RANDOMISATION"]]   START simpleWorkflow2AND 

 buildBMain :: [StateSet] -> Line -> PreAct -> [[ActivityId]] -> ActivityId -> [EventSequencing] -> ([StateSet],Line)
 buildBMain ss ln pa plist stopid es = (ns,last nl)
	where (ns,nl) = unzip (buildS ln pa plist stopid ss es)

 buildS :: Line -> PreAct -> [[ActivityId]] -> ActivityId -> [StateSet] ->[EventSequencing] -> [(StateSet,Line)]
 buildS ln pa plist stopid ss es = case pa of
					(OneOf pas) -> [(or,xsl)]++(if orpr == stopid then [] else (buildS (inLine ln) (getPr (ckn orpr)) (sublistofPList (toStopInPList plist) plist) stopid onsss es))
					(All pas) -> [(and,asl)]++(if andpr == stopid then [] else (buildS (inLine ln) (getPr (ckn andpr)) (sublistofPList (toStopInPList plist) plist) stopid ansss es))						     
					(Pa aid) ->  if aid == stopid then [] else [(nst,inLine ln)]++(buildS (inLine ln) (getPr (ckn aid)) (sublistofPList aid plist) stopid nss es) 
	where ckn i = head (filter (equalNm i) es) 
	      s = head (filter (chkNme (ckn (getPId pa))) ss)
	      nst = (changeStateSetTrans s [(True,inLine ln)] [(True,ln)])
	      nss = (updateStateSetListByNme nst ss)

	      (end,el) = mkstate (End (read (snd (splitAt 4 (inLine ln))) :: Int)  ) (inLine ln) []

States created for OneOf (Exclusive Or) prerequisite activities 

	      (xstart,xsl) = mkstate Start xil []
	      (xmiddle,xil) = buildInternal Xgate (bkLine el) (bkLine el) plist (toStopInPList plist) (getPs pa) ss es
	      xinternal = [(Atomic (((getAT.head) xmiddle)++[xstart,end]))]++(tail xmiddle)
	      or = (SubProcess (State (Bpmn ("Scopeflow"++(drop 4 ln)) ScopeB) [(True,xsl)] [(True,ln)] [] (ZERO,ZERO) [] [] [] [] [] 0) xinternal)
	      orpr = findScopePre (True,xil) ((snd.getSP) or)
	      onsss = (removeStateSetListByNmes ((tail.snd.getSP) or) ss)++[or]

States created for All (synchronised) prerequisite activities 

	      (astart,asl) = mkstate Start ail []
	      (amiddle,ail) = buildInternal Agate (bkLine el) (bkLine el) plist (toStopInPList plist) (getPs pa) ss es
	      ainternal = [(Atomic (((getAT.head) amiddle)++[astart,end]))]++(tail amiddle)
	      and = (SubProcess (State (Bpmn ("Scopeflow"++(drop 4 ln)) ScopeB) [(True,asl)] [(True,ln)] [] (ZERO,ZERO) [] [] [] [] [] 0) ainternal)
	      andpr = findScopePre (True,ail) ((snd.getSP) and)
	      ansss = (removeStateSetListByNmes ((tail.snd.getSP) and) ss)++[and]

Create the internal data structure of a "scope" subprocess

 buildInternal :: Type -> Line -> Line -> [[ActivityId]] -> ActivityId -> [PreAct] -> [StateSet] -> [EventSequencing] -> ([StateSet],Line)
 buildInternal t jl nl plist stopid ps ss es = (([(Atomic ([tsplit,tjoin]++midnon))]++midsub),((snd.head.getIn) tsplit))
 	where (tjoin,tl) = crtDNode (Right (length ps)) (Left [jl]) "" t
	      (tsplit,tsl) = crtDNode (Right 1) (Left ml) "" t
	      (middle,ml) = intMStep (zip ps ((snd.unzip.getIn) tjoin)) ((inLine.last.snd.unzip.getIn) tjoin) plist stopid ss es
	      midnon = (getAtomic middle)
	      midsub = stateDiff midnon middle

 intMStep :: [(PreAct,Line)] -> Line -> [[ActivityId]] -> ActivityId -> [StateSet] -> [EventSequencing] -> ([StateSet],[Line])
 intMStep pls ln plist stopid ss es = (nss,ls)
		where nss = [(Atomic (concatMap getAtomic conss))]++(concatMap stateDiffs conss)
		      (conss,ls) = unzip (internalMStep pls ln plist stopid ss es)
		      stateDiffs xx = xx \\ [(Atomic (getAtomic xx))]

 test04 = crtDNode (Right (length [Pa (Id "ELIGIBILITY"),Pa (Id "RANDOMISATION")])) (Left ["line21"]) "" Agate

 test02 = buildInternal Agate "line21" "line21" [[START,Id "ELIGIBILITY"],[START,Id "RANDOMISATION"]] START 
			 [Pa (Id "ELIGIBILITY"),Pa (Id "RANDOMISATION")] testSTATES2ANDNODEPEND simpleWorkflow2AND 

 test03 = intMStep [(Pa (Id "ELIGIBILITY"),"line19"),(Pa (Id "RANDOMISATION"),"line20")] "line21" 
 		   [[START,Id "ELIGIBILITY"],[START,Id "RANDOMISATION"]] START testSTATES2ANDNODEPEND simpleWorkflow2AND 

 test01 = internalMStep [(Pa (Id "ELIGIBILITY"),"line23"),(Pa (Id "RANDOMISATION"),"line22")] "line24" 
			 [[START,Id "ELIGIBILITY"],[START,Id "RANDOMISATION"]] START testSTATES2ANDNODEPEND simpleWorkflow2AND

 newss = (removeStateSetListByNmes singlesub testSTATES2ANDNODEPEND)++(singles)
	where (singleSets,singleLines) = unzip (internalSStep (Pa (Id "ELIGIBILITY"),"line23") "line24" subsetPlist START testSTATES2ANDNODEPEND simpleWorkflow2AND)
	      singlenonsub = (concatMap getAtomic singleSets)
	      singlesub = concatMap (stateDiff singlenonsub) singleSets
	      (singles,nl) = ([(Atomic singlenonsub)]++singlesub,last singleLines)
	      subsetPlist = (subsetofPList [(getPId (Pa (Id "ELIGIBILITY")))] [[START,Id "ELIGIBILITY"],[START,Id "RANDOMISATION"]])

 internalMStep :: [(PreAct,Line)] -> Line -> [[ActivityId]] -> ActivityId -> [StateSet] -> [EventSequencing] -> [([StateSet],Line)]
 internalMStep [] _ _ _ _ _ = []
 internalMStep ((p,l):pls) ln plist stopid ss es = case p of
					(Pa id) -> [(singles,nl)]++(internalMStep pls (inLine nl) (plist \\ subsetPlist) stopid nss es)
					(OneOf pas) -> [(xset,xl)]++(internalMStep pls (inLine xl) (plist \\ subsetPlistM) stopid xnss es)
				        (All pas) -> [(aset,al)]++(internalMStep pls (inLine al) (plist \\ subsetPlistM) stopid anss es)
	where (aset,al) = (buildInternal Agate l ln subsetPlistM (toStopInPList subsetPlistM) (getPs p) ss es)
	      (xset,xl) = (buildInternal Xgate l ln subsetPlistM (toStopInPList subsetPlistM) (getPs p) ss es)
	      (singleSets,singleLines) = unzip (internalSStep (p,l) ln subsetPlist stopid ss es)
	      singlenonsub = (concatMap getAtomic singleSets)
	      singlesub = concatMap (stateDiff singlenonsub) singleSets
	      (singles,nl) = ([(Atomic singlenonsub)]++singlesub,last singleLines)
	      subsetPlistM = (subsetofPList ((allPid.getPs) p) plist)
	      subsetPlist = (subsetofPList [(getPId p)] plist)
	      nss = (removeStateSetListByNmes singlesub ss)++(singles)
	      xnss = (removeStateSetListByNmes (tail xset) ss)++(xset)
	      anss = (removeStateSetListByNmes (tail aset) ss)++(aset)

 internalSStep :: (PreAct,Line) -> Line -> [[ActivityId]] -> ActivityId -> [StateSet] -> [EventSequencing] -> [([StateSet],Line)]
 internalSStep (p,l) ln plist stopid ss es = case p of 
				  (Pa id) -> if (id == stopid) then [] else [([ns],(bkLine nl))]++(internalSStep (np,nl) nl (sublistofPList id plist) stopid nss es)
				  (OneOf pas) -> [(xset,al)]++(internalSStep ((Pa stopM),xl) xl subM stopM xnss es) 
				  (All pas) -> [(aset,al)]++(internalSStep ((Pa stopM),al) al subM stopM anss es)
	where ckn i = head (filter (equalNm i) es) 
	      s = head (filter ((chkNme.ckn.getPId) p) ss)
	      ns = (changeStateSetTrans s [(True,ln)] [(True,l)])
	      nl = inLine ((snd.head.getIn.fst.getSP) ns)
	      nss = (updateStateSetListByNme ns ss)
	      np = ((getPr.ckn.getPId) p)
	      (aset,al) = (buildInternal Agate l ln plist (toStopInPList plist) (getPs p) ss es)
	      (xset,xl) = (buildInternal Xgate l ln plist (toStopInPList plist) (getPs p) ss es)
	      xnss = (removeStateSetListByNmes (tail xset) ss)++(xset)
	      anss = (removeStateSetListByNmes (tail aset) ss)++(aset)
	      stopM = (toStopInPList plist)
	      subM = sublistofPList stopM plist

This function removes all the state set from the given list which contain any of the non-process state from the first argument.

> stateDiff :: [State] -> [StateSet] -> [StateSet]
> stateDiff s ss = ss \\ [(Atomic s)]

create a list of lists of eventsequence paths from a given prerequisite activity id to START 

> preList :: [EventSequencing] -> [ActivityId] -> PreAct -> [[ActivityId]]
> preList es ans p = case p of
>		  	(Pa START) -> [[START]++ans]
>		  	(Pa a) -> preList es ([a]++ans) ((getPr.head) (filter (equalNm a) es)) 
>		  	(OneOf pas) -> ps
>		 	(All pas) -> ps
>	where ps = concatMap (preList es ans) (getPs p) 

 testpreList = preList testWorkflow [NORMAL_STOP] ((getPr.head) (filter (equalNm NORMAL_STOP) testWorkflow))

Find the next common activity from a list of lists of sequence paths

> toStopInPList :: [[ActivityId]] -> ActivityId
> toStopInPList plist = case find (common (plist \\ [shl])) (reverse shl) of
>				Just a -> a
> 				Nothing -> Id ""
>	where shl = head (filter ckl plist) 
>	      sh = minimum (map length plist) 
>	      ckl pl = (length pl) == sh
>	      common ps p = and (map (elem p) ps)  

 testStop = (filter ckl plist)
	where sh = minimum (map length plist) 
	      ckl pl = (length pl) == sh
	      plist = (map init (preList forpaperPreList [] (Pa (Id "SEQ4"))))

returns a list of sublists of sequence paths by a given common activity 

> sublistofPList :: ActivityId -> [[ActivityId]] -> [[ActivityId]]
> sublistofPList id plist = if (toStopInPList plist /= id) then plist 
>			   else map (reverse.(dropWhile (/=id)).reverse) plist

return a list of sequence paths which contain the given activity

> subsetofPList :: [ActivityId] -> [[ActivityId]] -> [[ActivityId]]
> subsetofPList [] plist = plist
> subsetofPList (i:ids) plist = subsetofPList ids (filter (elem i) plist)   

given an outgoing transition of a start state within a subprocess state 
and a list of all the states in that subprocess, findScopePre returns the id of the activity described by
"nearest" subprocess state which succedes this outgoing transition (for finding prerequisite activity of the whole subprocess)

> findScopePre :: Transition -> [StateSet] -> ActivityId
> findScopePre _ [] = Id ""
> findScopePre t ((Atomic a):s) = case sSuc t a of
>					[] -> findScopePre t s
>					[k] -> findScopePre ((head.getOut) k) ((Atomic a):s)
> findScopePre t ((SubProcess b c):s) = if elem t (getIn b) then ((idsBname.fst.invBpmn.getType) b)
>					else findScopePre t s

this function if the eventsequence's name is the same as the supplied activity id.

> equalNm :: ActivityId -> EventSequencing -> Bool
> equalNm id e = (getNme e) == id

this function splits a list of stateset (subprocess states) by matching the name of a given subprocess with one in the list.  

> splitStateSetListByNme :: StateSet -> [StateSet] -> ([StateSet],[StateSet]) 
> splitStateSetListByNme s ss = splitAt ((head.(findIndices (chkN ((fst.invBpmn.getType.fst.getSP) s)))) ss) ss
>	where chkN bn (SubProcess s x) = (isBpmn.getType) s && bn == (fst.invBpmn.getType) s

 testsplit = splitStateSetListByNme (head newss) testSTATES2ANDNODEPEND
	where chkN bn (SubProcess s x) = (isBpmn.getType) s && bn == (fst.invBpmn.getType) s

this function replaces a stateset in a list with a given one.

> updateStateSetListByNme :: StateSet -> [StateSet] -> [StateSet]
> updateStateSetListByNme s ss = a++[s]++(tail b)
>	where (a,b) = splitStateSetListByNme s ss

this function removes all the statesets of a list with a given list of statesets.

> removeStateSetListByNmes :: [StateSet] -> [StateSet] -> [StateSet]
> removeStateSetListByNmes [] ss = ss
> removeStateSetListByNmes (x:xs) ss  = removeStateSetListByNmes xs (a++(tail b)) 
>	where (a,b) = splitStateSetListByNme x ss

Check to see if a name of a stateset matches the name of an eventsequence.

> chkNme :: EventSequencing -> StateSet -> Bool
> chkNme e s = chknme (getNme e) s

> chknme :: ActivityId -> StateSet -> Bool
> chknme _ (Atomic _)  = False
> chknme e (SubProcess s _ )
>	| (isBpmn.getType) s = (idToBName e) == (fst.invBpmn.getType) s
>	| otherwise = False


> idToBName :: ActivityId -> BName
> idToBName (Id s) = foldl choose [] s 
>		where choose [] c  = [(toUpper c)]
>		      choose cs c  = case last cs of
>					' ' -> cs++[(toUpper c)]
>					any -> cs++[(toLower c)]

 idToBName NORMAL_STOP = ""
 idToBName STOP = "" 

> idToTName :: ActivityId -> TaskName
> idToTName = idToBName

> workToTask :: WorkId -> TaskName
> workToTask w = w
