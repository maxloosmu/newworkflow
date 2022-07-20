> module BPMNToWorkflow (bTow,bTowf,idsBname,sSuc) where
> import Char (toUpper)
> import IO (writeFile)
> import List (find)

 import WorkflowData

-- BPMN syntax definition

> import BPMN

-- workflow syntax definition

> import Workflow

-- XML to BPMN

> testdata = [("Pool",[Atomic [(State Start [] [(True,"Flow432")] [] (ZERO,ZERO) [] [] [] [] [] 1),
>                              (State (End 0) [(True,"Flow54")] [] [] (ZERO,ZERO) [] [] [] [] [] 1)],
>		       SubProcess (State (Bpmn "Eligibility" SequenceB) [(True,"Flow432")] [(True,"Flow54")] [] (MkTime Pve 0 0 0 0 0 0,MkTime Pve 0 0 0 0 0 0) [] [] [] [] [] 1)
>           			   [Atomic [(State Start [] [(True,"Flow28429")] [] (ZERO,ZERO) [] [] [] [] [] 1),
>                  			    (State (End 1) [(True,"Flow29431")] [] [] (ZERO,ZERO) [] [] [] [] [] 1)],
>             			    SubProcess (State (Bpmn "Eligibity_Obv" DependentB) [(True,"Flow28429")] [(True,"Flow29431")] [] (MkTime Pve 0 0 0 0 0 0,MkTime Pve 0 0 0 0 0 0) [] [] [] [] [] 1)
>                        		      	[Atomic [(State Start [] [(True,"Flow63")] [] (ZERO,ZERO) [] [] [] [] [] 1),
>                  					 (State (End 2) [(True,"Flow64")] [] [] (ZERO,ZERO) [] [] [] [] [] 1),
>                  					 (State (Task "Eligibility_Form_Submission" StandardT) [(True,"Flow63")] [(True,"Flow64")] [] (MkTime Pve 0 0 0 0 0 0,MkTime Pve 0 0 14 0 0 0) [] [] [] [] [] 1)] 
>						]
>				   ]])]



> bTowf :: BPMN -> FilePath -> IO()
> bTowf w f = do { case bTow w of
>			[] -> writeFile f ""
>		  	a -> writeFile f ("> testWorkflow = "++(show a))}

> bTow :: BPMN -> Workflow
> bTow w = if (length(w) > 1) then [] else wf
>	where ss = (snd (head w))
>	      (p1,p2) = findTermin ss (filter (isEnd.getType) ((getAT.head) ss)) (filter (isAbort.getType) ((getAT.head) ss))
>	      wf = [start]++(map (checkTermin p1 p2) (ltrans ss ss))++end++abort
>	      start = makeStartSeq ss
>	      end = (if p1 == Nothing then []
>		     else [(Event NORMAL_STOP (gP p1) (De END) None None (SequentialD [Da (STOP,Dur "",Dur "",Rep 0,None,Manual)]) [] NoWork)])
>	      abort = if p2 == Nothing then []
>		      else [(Event ABNORMAL_STOP (gP p2) (De END) None None (SequentialD [Da (STOP,Dur "",Dur "",Rep 0,None,Manual)]) [] NoWork)]
>	      gP (Just p) = p

Find the prerequisite activities to terminate this workflow

> findTermin :: [StateSet] -> [State] -> [State] -> (Maybe PreAct, Maybe PreAct)
> findTermin sp es abs = (if null ep then Nothing else Just (head ep),if null ap then Nothing else Just (head ap))
> 	where ep = map (getPre sp) (map (head.getIn) es)
>	      ap = map (getPre sp) (map (head.getIn) abs)

Check for terminating dependent activities 

> checkTermin :: Maybe PreAct -> Maybe PreAct -> EventSequencing -> EventSequencing
> checkTermin Nothing (Just p2) es = ckTermin p2 ABNORMAL_STOP es
> checkTermin Nothing Nothing es = es
> checkTermin (Just p1) (Just p2) es = ((ckTermin p1 NORMAL_STOP) . (ckTermin p2 ABNORMAL_STOP)) es
> checkTermin (Just p1) Nothing es = ckTermin p1 NORMAL_STOP es

> ckTermin :: PreAct -> ActivityId -> EventSequencing -> EventSequencing
> ckTermin pa id (Event n p de c1 c2 da r w) 
>	| isInPre n pa = (Event n p de c1 c2 (insertD (Da (id,Dur "",Dur "",Rep 0,None,Manual)) da) r w)
>	| otherwise = (Event n p de c1 c2 da r w)
>	where isInPre nm (OneOf ps) = (any (isInPre nm) ps)
>	      isInPre nm (All ps) = (any (isInPre nm) ps)
>	      isInPre nm (Pa p) = p == nm
>	      insertD a NoDepend = (SequentialD [a])
>	      insertD a (SequentialD acts) = (SequentialD (acts++[a]))

> isTerminalState :: State -> Bool
> isTerminalState s = ((isEnd.getType) s || (isAbort.getType) s)

> makeStartSeq :: [StateSet] -> EventSequencing
> makeStartSeq ss = (Event START (Pa BEGIN) (getDEv ss ((head.getOut) start)) None None (SequentialD [Da (STOP,Dur "",Dur "",Rep 0,None,Manual)]) [] NoWork)
>	where start = head [ s | s <- getAtomic ss, getType s == Start ] 

ltrans is a function that takes two copies of a BPMN diagram (single process) and returns a Workflow instance
described by the diagram

> ltrans :: [StateSet] -> [StateSet] -> Workflow
> ltrans tt = foldr (ckState tt) []
>	where ckState tt s ws
>		| (isSeqSub s) = let devs = if (null.getError.fst.getSP) s then (getDEv tt ((head.getOut.fst.getSP) s))
>					    else OneOfD ([(getDEv tt ((head.getOut.fst.getSP) s))]++(map (getDEv tt) ((snd.unzip.getError.fst.getSP) s)))
>				 in [ (Event (getEvName s) (getPre tt ((head.getIn.fst.getSP) s)) devs (getCond s) (getFCond s) 
>				  	     (getDpt s) (getRepeat s) ((normW.getInv) s))]++ws
>		| (isScopeSub s) = (ltrans (tt++((snd.getSP) s)) ((snd.getSP) s)) ++ ws
>		| otherwise = ws

isSeqSub is a predicate function which a subprocess state describing a single eventsequence satisfies

> isSeqSub :: StateSet -> Bool
> isSeqSub (SubProcess w s) = isBpmn(getType w) && (snd.invBpmn.getType) w  == SequenceB  
> isSeqSub _ = False

isScopeSub is a predicate function which a subprocess state describing a control flow of several eventsequences satisfies

> isScopeSub :: StateSet -> Bool
> isScopeSub (SubProcess w s) = isBpmn(getType w) && (snd.invBpmn.getType) w  == ScopeB  
> isScopeSub _ = False

idsBname takes a subprocess name and returns the id of the activity which the subprocess describes.

> idsBname :: BName -> ActivityId
> idsBname n = Id (map toUpper n)

idsTname takes a task name and returns the id of the activity which the task state describes.

> idsTname :: TaskName -> ActivityId
> idsTname n = Id (map toUpper n)

taskToWork takes a task name and returns an description of work (manual) which the task state describes.

> taskToWork :: TaskName -> WorkId
> taskToWork n = n

findSubProcess takes a BpmnType (SequenceB | ScopeB | DependentB | RepeatB | InterventionB) and a list of subprocess states
and returns the list of the subsets which have been labelled with that BpmnType.

> findSubProcess :: BpmnType -> [StateSet] -> [StateSet]
> findSubProcess bt = filter (chk bt) 
>	where chk bt (Atomic _) = False
>	      chk bt (SubProcess s w)
>		| (isMiseqs.getType) s = (trd4.invMiseqs.getType) s == bt
>		| (isMipars.getType) s = (trd4.invMipars.getType) s == bt
>		| (isBpmn.getType) s = (snd.invBpmn.getType) s == bt
>		| otherwise = False

Given a subprocess state, getEvName returns the id of the activity which that state describes.

> getEvName :: StateSet -> ActivityId
> getEvName (SubProcess s w) = (idsBname.fst.invBpmn.getType) s

Given a list of statesets (head of the list being the list of non-subprocess states, with a tail of subprocess states)
and a list of incoming transitions, apply one copy of statesets list with each transition to the function getPre,
and returns a list of prerequsite activity ids.

> getPreMult :: [StateSet] -> [Transition] -> [PreAct]
> getPreMult sp = map (getPre sp)

Given a list of statesets and an incoming transition, returns the data structure of type PreAct describing the
synchronisation of all the prerequisite activities prior the transition of the state that contains the incoming
transition.

> getPre :: [StateSet] -> Transition -> PreAct
> getPre ((SubProcess v w):tt) t 
>	| (elem t (map snd (getError v))) = getPre (tt++[(SubProcess v w)]) ((head.getIn) v)
>	| elem t (getOut v) = case (snd.invBpmn.getType) v of
>				 ScopeB -> getPreScope w
>				 SequenceB -> Pa ((idsBname.fst.invBpmn.getType) v)
>	| otherwise = getPre (tt++[(SubProcess v w)]) t
> getPre ((Atomic w):tt) t = case (sPre t w) of
>				Nothing -> getPre (tt++[Atomic w]) t
>				Just a -> (if isXJ a then (OneOf (getPreMult ((Atomic w):tt) (getIn a) ))
>				       	   else (if isAJ a then (All (getPreMult ((Atomic w):tt) (getIn a) ))
>				    	      	 else (if (isXS a || isAS a) then (getPre ((Atomic w):tt) ((head.getIn) a))
>					               else (if (getType a == Start) 
>							     then (case find (contn a) tt of
>									Just (SubProcess i j) -> (getPre ((Atomic w):tt) ((head.getIn) i))
>									Nothing -> Pa START)
>							     else Pa START))))
>		where contn s (SubProcess sp ((Atomic ss):ws)) = elem s ss
>		      contn s (Atomic ss) = False


> getPreScope :: [StateSet] -> PreAct
> getPreScope ((Atomic w):tt) = getPre ((Atomic w):tt) ((head.getIn.head.(filter (isEnd.getType))) w) 
> getPreScope (t:tt) = getPreScope (tt++[t])

Given a list of statesets and an outgoing transition, returns the data structure of type DptEvent describing the
all dependent eventsequences after the transition of the state that contains the outoing transition.

> getDEvMult :: [StateSet] -> [Transition] -> [DptEvent]
> getDEvMult sp = map (getDEv sp)

> getDEv :: [StateSet] -> Transition -> DptEvent
> getDEv ((SubProcess v w):tt) t
> 	| elem t (getIn v) = case (snd.invBpmn.getType) v of
>				 ScopeB -> getDEvScope w
>				 SequenceB -> De ((idsBname.fst.invBpmn.getType) v)
>	| otherwise = getDEv (tt++[(SubProcess v w)]) t
> getDEv ((Atomic w):tt) t = case (sSuc t w) of
>				[] -> getDEv (tt++[Atomic w]) t
>				[a] -> (if isXS a then (OneOfD (getDEvMult ((Atomic w):tt) (getOut a) ))
>				       	   else (if isAS a then (AllD (getDEvMult ((Atomic w):tt) (getOut a) ))
>				    	      	 else (if (isXJ a || isAJ a) then (getDEv ((Atomic w):tt) ((head.getOut) a))
>					               else (if ((isEnd.getType) a) 
>							     then (case find (contn a) tt of
>									Just (SubProcess i j) -> (getDEv ((Atomic w):tt) ((head.getOut) i))
>									Nothing -> De NORMAL_STOP)
>							     else De ABNORMAL_STOP ))))
>		where contn s (SubProcess sp ((Atomic ss):ws)) = elem s ss
>		      contn s (Atomic ss) = False

> getDEvScope :: [StateSet] -> DptEvent
> getDEvScope ((Atomic w):tt) = getDEv ((Atomic w):tt) ((head.getOut.head.(filter ((== Start).getType))) w) 
> getDEvScope (t:tt) = getDEvScope (tt++[t])

sPre takes a transition and a list of states, and returns the first state in the list that contains
the transition as one of its outgoing transitions, or return Nothing if the list does not contain any of such states.

> sPre :: Transition -> [State] -> Maybe State 
> sPre t = find (chk t) where chk t st = elem t (getOut st)

sSuc takes a transition and a list of states, and returns a list of states which contain
the transition as one of their incoming transitions. 

> sSuc :: Transition -> [State] -> [State]
> sSuc t = filter (chk t) where chk t st = elem t (getIn st)

isXS, isAS, isXJ and isAJ are predicates to see if a given state 
is a XOR Split, AND Split, XOR Join and AND Join state.

> isXS, isAS, isXJ, isAJ :: State -> Bool
> isXS s = (getType s == Xgate) && (length.getIn) s == 1
> isAS s = (getType s == Agate) && (length.getIn) s == 1
> isXJ s = (getType s == Xgate) && (length.getOut) s == 1
> isAJ s = (getType s == Agate) && (length.getOut) s == 1

> getCond :: StateSet -> Condition
> getCond s = None

> getFCond :: StateSet -> Condition
> getFCond s = None

getDpt takes a subprocess state representing an eventsequence and returns a data structure of type DptAct representing the control
flow of all the sequence's dependent activities.

> getDpt :: StateSet -> DptAct
> getDpt s = case (((findSubProcess DependentB).snd.getSP) s) of
>		[] -> NoDepend
>		[x] -> getDpts ((snd.getSP) x)

getDNme takes a set of states of a subprocess state describing a dependent activity and returns.
the id of that dependent activity

 getDNme :: [StateSet] -> ActivityId
 getDNme ((Atomic ss):s) = case (find (chkForm ss) ss) of
		Just a -> (idsTname.fst.invTask.getType) a
		Nothing -> Id "ERROR"
	where chkForm ss s = case (sSuc ((head.getOut) s) ss) of
				[a] -> (isEnd.getType) a
				_ -> False
				
> getDNme :: State -> ActivityId
> getDNme s = let t = getType s 
>	      in if isMiseq t then (idsTname.fst4.invMiseq) t else (idsTname.fst.invTask) t

mkAct takes a subprocess state describing a single dependent activity and that dependent activity in Workflow syntax.

> mkAct :: State -> DptAct
> mkAct s = Da ((getDNme s),(getT.fst.getRange) s,(getT.snd.getRange) s, rp, None, Manual)
>	where rp = if (isTask.getType) s then Rep 1
>		   else if (getLoops.snd4.invMiseq.getType) s == 0 then Any else ((Workflow.Rep).getLoops.snd4.invMiseq.getType) s

 mkAct :: StateSet -> DptAct
 mkAct (SubProcess s st) = Da ((getDNme st),(getMinT st),(getMaxT st),(getCond (SubProcess s st)),Manual)

----------------------------- Begin New getObv and getInv ---------------------------------------------------------------------

 getSWM :: BpmnType -> StateSet -> StructuredModel a
 getSWM b s = case (((findSubProcess b).snd.getSP) s) of
		 [] -> NoItem
		 [s] -> getSWMs b ((getAtomic.snd.getSP) s)

 getSWMs :: BpmnType -> [State] -> StructuredModel a
 getSWMs b ss = case (find checkS ss) of
		Just a ->  gSWM b ((head.getOut) a) ss
		Nothing -> NoItem
	where checkS s = (getType s == Start)

 gSWM :: BpmnType -> Transition -> [State] -> StructuredModel a
 gSWM b t s = case (sSuc t s) of
		[x] -> Sequential (fst (getSeqSWM b t s))
		[] -> NoItem

 getSMWMult :: BpmnType -> [State] -> [Transition] -> ([StructuredModel a],Maybe Transition)
 getSMWMult b ss tt = case find (((/=).head.snd) getAP) ((tail.snd) getAP) of
			Just a -> ((fst getAP),Nothing)
			Nothing -> ((fst getAP),((head.snd) getAP))
	where getAP = (unzip.(map (getPathSMW b ss))) tt 

 getPathSMW :: BpmnType -> [State] -> Transition -> (StructuredModel a, Maybe Transition)
 getPathSMW b ss t = case nextSt t ss of
			[a] -> (if isXS a then ((Choice ((fst.dpmult) a)),((snd.dpmult) a))  
			        else (if isAS a then ((Par ((fst.dpmult) a)),((snd.dpmult) a))
				      else (if (isXJ a || isAJ a) then (NoItem,Just ((head.getOut) a))
					    else (if b == InterventionB then (if (isMiseq.getType) a then (Sequential (fst mkSD),(snd mkSD)) else (NoItem,Nothing) )
						  else (if (isTask.getType) a then (Sequential (fst mkSD),(snd mkSD)) else (NoItem,Nothing) )))))
			[] -> (NoItem,Nothing)
	where mkSD = (getSeqSWM b t ss)
	      dpmult a = getSMWMult b ss (getOut a)

 getSeqSWM :: BpmnType -> Transition -> [State] -> ([StructuredModel a],Maybe Transition)
 getSeqSWM b t ss = if (last.fst) gd == NoItem then ((init.fst) gd,(last.snd) gd)
		     else (fst gd,(last.snd) gd)
	where gd = unzip (getSeqSWMs b t ss)

 getSeqSWMs :: BpmnType -> Transition -> [State] -> [(StructuredModel a,Maybe Transition)]
 getSeqSWMs b t ss = case (nextSt t ss) of
			[x] -> (if isXS x then [((Choice ((fst.mult) x)),((snd.mult) x))] ++ carryOn ((snd.mult) x) 
				else (if isAS x then [((Par ((fst.mult) x)),((snd.mult) x))] ++ carryOn ((snd.mult) x)
				      else ( if (isXJ x || isAJ x) then [(NoItem,Just ((head.getOut) x))] 
					     else (if b == InterventionB then (if (isMiseq.getType) x then [(mkItem x, Just ((head.getOut) x))] ++ (getSeqSWMs b ((head.getOut) x) ss) else [] )
						   else (if (isTask.getType) x then [(mkItem x, Just ((head.getOut) x))] ++ (getSeqSWMs b ((head.getOut) x) ss) else [] )))))
			[] -> [] 	
	where mult s = getSMWMult b ss (getOut s)
	      carryOn jt = case jt of
				Just t -> getSeqSWMs b t ss
				Nothing -> []

/*

class MkItem a where
   mkitem :: State -> StructuredModel a
   
instance MkItem Activity where
   mkitem (State _ _ _ _) | a  (mItem ((getDNme s),(getT.fst.getRange) s,(getT.snd.getRange) s,None,Manual))...
   
instance MkItem Work where
   mkitem

*/

 mkItem :: State -> StructuredModel a
 mkItem s = if (isTask.getType) s then (mItem ((getDNme s),(getT.fst.getRange) s,(getT.snd.getRange) s,None,Manual)) 
	     else (mItem (((taskToWork.fst4.invMiseq.getType) s),(getTreat.trd4.invMiseq.getType) s,(getT.fst.getRange) s,(getT.snd.getRange) s,rp))
	where rp = if (snd4.invMiseq.getType) s == 0 then Any else ((Workflow.Rep).snd4.invMiseq.getType) s

 mItem :: a -> StructuredModel a
 mItem a = Item a 


----------------------------- End New getObv and getInv ---------------------------------------------------------------------

getDpts takes a set of states of a subprocess state describing the control flow of the dependent activities of an eventsequence
and returns a data structure of type DptAct representing the control flow of that eventsequence's dependent activities
(We ensure the output data structure is a list of nested choices and parallels of dependent activities.)

> getDpts :: [StateSet] -> DptAct
> getDpts ((Atomic s):ss) = case (find checkS s) of
>				Just a -> gDpt ((head.getOut) a) s
>				Nothing -> NoDepend
>	where checkS s = (getType s == Start)

> gDpt :: Transition -> [State] -> DptAct
> gDpt t s = case (sSuc t s) of
>		[] -> NoDepend
>		[x] -> SequentialD dSeq
>	where dSeq = fst (getSeqDpt t s)

 gDpt :: Transition -> [StateSet] -> DptAct
 gDpt t ((Atomic s):ss) = case (sSuc t s) of
				[] -> SequentialD dSeq
				[x] -> (if (isXS x || isAS x) then SequentialD dSeq else NoDepend)
	where dSeq = fst (getSeqDpt t (Atomic s) ss)

					[x] -> (if isXS x then (ChoiceD (getSucMult (Atomic s) ss (getOut x) ))
					   	 else (if isAS x then (ParD (getSucMult (Atomic s) ss (getOut x) ))
						       else NoDepend))

Takes a list of non-subprocess states, a list of subprocess states and a list of outgoing transitions of a non-subprocess state
and returns a list of dependent activities modelled by all of succeding states within this subprocess

> getSucMult :: [State] -> [Transition] -> ([DptAct],Maybe Transition)
> getSucMult ss tt = case find (((/=).head.snd) getAP) ((tail.snd) getAP) of
>			Just a -> ((fst getAP),Nothing)
>			Nothing -> ((fst getAP),((head.snd) getAP))
>	where getAP = (unzip.(map (getPathDt ss))) tt 

 getSucMult :: StateSet -> [StateSet] -> [Transition] -> ([DptAct],Maybe Transition)
 getSucMult s ss tt = case find (((/=).head.snd) getAP) ((tail.snd) getAP) of
			Just a -> ((fst getAP),Nothing)
			Nothing -> ((fst getAP),((head.snd) getAP))
	where getAP = (unzip.(map (getPathDt s ss))) tt 

> nextSt :: Transition -> [State] -> [State]
> nextSt t = sSuc t 

 filter (chkSt t) where chkSt t l = elem t (getIn l)

 nextSub :: Transition -> [StateSet] -> [StateSet]
 nextSub t = filter (chkSub t) where chkSub t l = elem t ((getIn.fst.getSP) l)

 nextST :: Transition -> StateSet -> [State] 
 nextST t (Atomic ls) = sSuc t ls

> getPathDt :: [State] -> Transition -> (DptAct, Maybe Transition)
> getPathDt ss t = case nextSt t ss of
>			[a] -> (if isXS a then ((ChoiceD ((fst.dpmult) a)),((snd.dpmult) a))  
>			        else (if isAS a then ((ParD ((fst.dpmult) a)),((snd.dpmult) a))
>				      else ( if  (isTask.getType) a || (isMiseq.getType) a then (SequentialD (fst mkSD),(snd mkSD))
>					     else (if (isXJ a || isAJ a) then (NoDepend,Just ((head.getOut) a)) 
>					     	   else (NoDepend,Nothing) ))))
>			[] -> (NoDepend,Nothing)
>	where mkSD = (getSeqDpt t ss)
>	      dpmult a = getSucMult ss (getOut a)

 getPathDt :: StateSet -> [StateSet] -> Transition -> (DptAct, Maybe Transition)
 getPathDt s ss t = case nextSub t ss of 
			[a] -> (SequentialD (fst mkSD),(snd mkSD))
			[] -> case (nextST t s) of
				[] -> (NoDepend,Nothing)
				[a] -> (if isXS a then ((ChoiceD ((fst.dpmult) a)),((snd.dpmult) a))  
				        else (if isAS a then ((ParD ((fst.dpmult) a)),((snd.dpmult) a))
					      else ( if (isXJ a || isAJ a) then (NoDepend,Just ((head.getOut) a)) 
						     else (NoDepend,Nothing) )))

	where mkSD = (getSeqDpt t s ss)
	      dpmult a = getSucMult s ss (getOut a)

> getSeqDpt :: Transition -> [State] -> ([DptAct],Maybe Transition)
> getSeqDpt t ss = if (last.fst) gd == NoDepend then ((init.fst) gd,(last.snd) gd)
>		   else (fst gd,(last.snd) gd)
>	where gd = unzip (getSeqDpts t ss)

> getSeqDpts :: Transition -> [State] -> [(DptAct,Maybe Transition)]
> getSeqDpts t ss = case (nextSt t ss) of
>			[a] -> (if isXS a then [((ChoiceD ((fst.dpmult) a)),((snd.dpmult) a))] ++ carryOn ((snd.dpmult) a) 
>				else (if isAS a then [((ParD ((fst.dpmult) a)),((snd.dpmult) a))] ++ carryOn ((snd.dpmult) a)
>				      else ( if (isXJ a || isAJ a) then [(NoDepend,Just ((head.getOut) a))] 
>					     else (if (isTask.getType) a || (isMiseq.getType) a 
>						   then [(mkAct a, Just ((head.getOut) a))] ++ (getSeqDpts ((head.getOut) a) ss)
>						   else [] ))))
>			[] -> [] 	
>	where dpmult a = getSucMult ss (getOut a)
>	      carryOn jt = case jt of
>				Just t -> getSeqDpts t ss
>				Nothing -> []

 getSeqDpt :: Transition -> StateSet -> [StateSet] -> ([DptAct],Maybe Transition)
 getSeqDpt t s ss = if (last.fst) gd == NoDepend then ((init.fst) gd,(last.snd) gd)
		     else (fst gd,(last.snd) gd)
	where gd = unzip (getSeqDpts t s ss)

 getSeqDpts :: Transition -> StateSet -> [StateSet] -> [(DptAct,Maybe Transition)]
 getSeqDpts t s ss = case find ((elem t).getIn) (getAtomic ss) of
			Just a -> [(mkAct a, Just ((head.getOut) a))] ++ (getSeqDpts ((head.getOut) a) s ss)
			Nothing -> case (nextST t s) of
					[] -> []
					[a] -> (if isXS a then [((ChoiceD ((fst.dpmult) a)),((snd.dpmult) a))] ++ carryOn ((snd.dpmult) a) 
				        	else (if isAS a then [((ParD ((fst.dpmult) a)),((snd.dpmult) a))] ++ carryOn ((snd.dpmult) a)
					      	      else ( if (isXJ a || isAJ a) then [(NoDepend,Just ((head.getOut) a))] 
						     	     else [] )))
	where dpmult a = getSucMult s ss (getOut a)
	      carryOn jt = case jt of
				Just t -> getSeqDpts t s ss
				Nothing -> []


		      case nextSub t ss of 
			[a] -> [(mkAct a, Just ((head.getOut.fst.getSP) a))] ++ (getSeqDpts ((head.getOut.fst.getSP) a) s ss)
			[] -> case (nextST t s) of


Duration

> getMinT,getMaxT :: [StateSet] -> Duration
> getMaxT ((Atomic ss):sts) = case findItime ss of
>					Just a -> (getT.invItime) a
>					Nothing -> UNBOUNDED
>	where 	findItime [] = Nothing
>		findItime (s:ss) = case chkItime (getError s) of
>					Just a -> Just a
>					Nothing -> findItime ss
>		chkItime [] = Nothing
>		chkItime ((y,tt):es) = if isItime y then Just y else chkItime es

> getMinT ((Atomic ss):sts) = case find chkStime ss of
>					Just a -> (getT.invStime.getType) a
>					Nothing -> UNBOUNDED
>	where chkStime s = (isStime.getType) s

> getT :: Time -> Duration
> getT t = case d of
>	       Dur "P" -> Dur "P0D"
>	       dr -> dr
>	where d = getT' t

> getT' :: Time -> Duration
> getT' NOBOUND = UNBOUNDED
> getT' (MkTime ve yr mh dy hr mn sc) = 
>	if (hr == 0 && mn == 0 && sc == 0) then (Dur ((gDi ve)++"P"++(gCo yr "Y")++(gCo mh "M")++(gCo dy "D")))
>	else (Dur ((gDi ve)++"P"++(gCo yr "Y")++(gCo mh "M")++(gCo dy "D")++"T"++(gCo hr "H")++(gCo mn "M")++(gCo sc "S")))
>		where gDi ve = (if ve == Pve then "" else "-")
>	      	      gCo i t = (if i == 0 then "" else (show i)++t)

Make minimum and maximum repeat from a given state (incomplete)

> getMinL,getMaxL :: Loops -> Repeat
> getMinL (Fix i) = Rep i
> getMinL (Ndet i) = Rep 0
> getMaxL (Fix i) = Rep i
> getMaxL (Ndet i) = Rep i

> getRepeat :: StateSet -> [RepeatExp]
> getRepeat s =  ((map getRp).(findSubProcess RepeatB).snd.getSP) s

> getRp :: StateSet -> RepeatExp
> getRp (SubProcess s ss) =
>	let (min,max) = getRange s
>	    lp = if (isMiseqs.getType) s then (snd4.invMiseqs.getType) s else error "getRp"
>	in (getT min, getT max , getMinL lp, getMaxL lp, getCond (SubProcess s ss))

 getRp (SubProcess s ss) = (getMinT ss, getMaxT ss , getMinL s, getMaxL s, getCond (SubProcess s ss))


Searhing for Intervention (Work)

> getInv :: StateSet -> Works
> getInv s = case (((findSubProcess InterventionB).snd.getSP) s) of
>		[] -> NoWork
>		[s] -> getInvs ((snd.getSP) s) (((getAtomic.snd.getSP) s) ++ [ (fst.getSP) x | x <- ((snd.getSP) s), (not.isAtomic) x && (isMiseqs.getType.fst.getSP) x ])

		[s] -> getInvs ((snd.getSP) s)

> chkInv :: State -> Bool
> chkInv s = (isMiseq.getType) s && (isIntervention.trd4.invMiseq.getType) s 
>	     || (isMiseqs.getType) s && (isInterventionS.trd4.invMiseqs.getType) s

(SubProcess sp ((Atomic s):ss)) =

> getInvs :: [StateSet] -> [State] -> Works
> getInvs xs ss = case (find checkS ss) of
>			Just a ->  gInv xs ((head.getOut) a) ss
>			Nothing -> NoWork
>	 	  where checkS s = (getType s == Start)

> gInv :: [StateSet] -> Transition -> [State] -> Works
> gInv xs t s = case (sSuc t s) of
>		[] -> NoWork
>		[x] -> SequentialW dSeq
>	where dSeq = fst (getSeqInv xs t s)

> getInvMult :: [StateSet] -> [State] -> [Transition] -> ([Works],Maybe Transition)
> getInvMult xs ss tt = case find (((/=).head.snd) getAP) ((tail.snd) getAP) of
>			Just a -> ((fst getAP),Nothing)
>			Nothing -> ((fst getAP),((head.snd) getAP))
>	where getAP = (unzip.(map (getPathIv xs ss))) tt 

> getPathIv :: [StateSet] -> [State] -> Transition -> (Works, Maybe Transition)
> getPathIv xs ss t = case nextSt t ss of
>			[a] -> (if isXS a then ((ChoiceW ((fst.dpmult) a)),((snd.dpmult) a))  
>			        else (if isAS a then ((ParW ((fst.dpmult) a)),((snd.dpmult) a))
>				      else ( if (isMiseq.getType) a || (isMiseqs.getType) a then (SequentialW (fst mkSD),(snd mkSD))
>					     else (if (isXJ a || isAJ a) then (NoWork,Just ((head.getOut) a)) 
>					     	   else (NoWork,Nothing) ))))
>			[] -> (NoWork,Nothing)
>	where mkSD = (getSeqInv xs t ss)
>	      dpmult a = getInvMult xs ss (getOut a)

> getSeqInv :: [StateSet] -> Transition -> [State] -> ([Works],Maybe Transition)
> getSeqInv xs t ss = if (last.fst) gd == NoWork then ((init.fst) gd,(last.snd) gd)
>		      else (fst gd,(last.snd) gd)
>	where gd = unzip (getSeqInvs xs t ss)

> getSeqInvs :: [StateSet] -> Transition -> [State] -> [(Works,Maybe Transition)]
> getSeqInvs xs t ss = case (nextSt t ss) of
>			[a] -> (if isXS a then [((ChoiceW ((fst.dpmult) a)),((snd.dpmult) a))] ++ carryOn ((snd.dpmult) a) 
>				else (if isAS a then [((ParW ((fst.dpmult) a)),((snd.dpmult) a))] ++ carryOn ((snd.dpmult) a)
>				      else ( if (isXJ a || isAJ a) then [(NoWork,Just ((head.getOut) a))] 
>					     else (if (isMiseq.getType) a || (isMiseqs.getType) a then [(mkW xs a, Just ((head.getOut) a))] ++ (getSeqInvs xs ((head.getOut) a) ss)
>						   else [] ))))
>			[] -> [] 	
>	where dpmult a = getInvMult xs ss (getOut a)
>	      carryOn jt = case jt of
>				Just t -> getSeqInvs xs t ss
>				Nothing -> []

> mkW :: [StateSet] -> State -> Works
> mkW xs s = if (isMiseq.getType) s 
>	     then let rp = if (getLoops.snd4.invMiseq.getType) s == 0 then Any else ((Workflow.Rep).getLoops.snd4.invMiseq.getType) s
>	          in Wk (WorkSingle (((taskToWork.fst4.invMiseq.getType) s),(getTreat.trd4.invMiseq.getType) s,(getT.fst.getRange) s,(getT.snd.getRange) s,rp))
>	     else let rp = if (getLoops.snd4.invMiseqs.getType) s == 0 then Any else ((Workflow.Rep).getLoops.snd4.invMiseqs.getType) s
>		      ys = [ y | x <- xs, (not.isAtomic) x && (isMiseqs.getType.fst.getSP) x 
>			     	 && (fst4.invMiseqs.getType.fst.getSP) x == (fst4.invMiseqs.getType) s, y <- (getAtomic.snd.getSP) x ]
>		      dur = case find (isStime.getType) ys of
>				Just k -> (getT.invStime.getType) k
>				Nothing -> getT ZERO
>	          in Wk (WorkMult (((taskToWork.fst4.invMiseqs.getType) s),(getWorkMult ys),dur,rp))

> getTreat :: TaskType -> Treatment
> getTreat (InterventionT (Intervention n q m)) = Treatment n q m
> getTreat _ = NoTreatment 


Sets of Work Units 
-- WorkSingle - A structured set of repetitive work unit  e.g. drug A x 5 followed by drug B x 7
-- WorkMult - A repetitive structured set of work unit e.g. drug A followed by drug B x 6

> getWorkMult :: [State] -> WorkBlock
> getWorkMult ss = case (find checkS ss) of
>			Just a ->  gInvB ((head.getOut) a) ss
>			Nothing -> NoWorkB
>	 	  where checkS s = (getType s == Start) || (isStime.getType) s 

> gInvB :: Transition -> [State] -> WorkBlock
> gInvB t s = case (sSuc t s) of
>		[] -> NoWorkB
>		[x] -> SequentialWB dSeq
>	where dSeq = fst (getSeqInvB t s)

> getInvMultB :: [State] -> [Transition] -> ([WorkBlock],Maybe Transition)
> getInvMultB ss tt = case find (((/=).head.snd) getAP) ((tail.snd) getAP) of
>			Just a -> ((fst getAP),Nothing)
>			Nothing -> ((fst getAP),((head.snd) getAP))
>	where getAP = (unzip.(map (getPathIvB ss))) tt 

> getPathIvB :: [State] -> Transition -> (WorkBlock, Maybe Transition)
> getPathIvB ss t = case nextSt t ss of
>			[a] -> (if isXS a then ((ChoiceWB ((fst.dpmult) a)),((snd.dpmult) a))  
>			        else (if isAS a then ((ParWB ((fst.dpmult) a)),((snd.dpmult) a))
>				      else ( if (isTask.getType) a then (SequentialWB (fst mkSD),(snd mkSD))
>					     else (if (isXJ a || isAJ a) then (NoWorkB,Just ((head.getOut) a)) 
>					     	   else (NoWorkB,Nothing) ))))
>			[] -> (NoWorkB,Nothing)
>	where mkSD = (getSeqInvB t ss)
>	      dpmult a = getInvMultB ss (getOut a)

> getSeqInvB :: Transition -> [State] -> ([WorkBlock],Maybe Transition)
> getSeqInvB t ss = if (last.fst) gd == NoWorkB then ((init.fst) gd,(last.snd) gd)
>		      else (fst gd,(last.snd) gd)
>	where gd = unzip (getSeqInvsB t ss)

> getSeqInvsB :: Transition -> [State] -> [(WorkBlock,Maybe Transition)]
> getSeqInvsB t ss = case (nextSt t ss) of
>			[a] -> (if isXS a then [((ChoiceWB ((fst.dpmult) a)),((snd.dpmult) a))] ++ carryOn ((snd.dpmult) a) 
>				else (if isAS a then [((ParWB ((fst.dpmult) a)),((snd.dpmult) a))] ++ carryOn ((snd.dpmult) a)
>				      else ( if (isXJ a || isAJ a) then [(NoWorkB,Just ((head.getOut) a))] 
>					     else (if (isTask.getType) a then [(mkWB a, Just ((head.getOut) a))] ++ (getSeqInvsB ((head.getOut) a) ss)
>						   else [] ))))
>			[] -> [] 	
>	where dpmult a = getInvMultB ss (getOut a)
>	      carryOn jt = case jt of
>				Just t -> getSeqInvsB t ss
>				Nothing -> []

> mkWB :: State -> WorkBlock
> mkWB s = WorkUnit (((taskToWork.fst.invTask.getType) s),(getTreat.snd.invTask.getType) s,(getT.fst.getRange) s,(getT.snd.getRange) s)


 getInvs :: [StateSet] -> Works
 getInvs ((Atomic s):ss) = case find chkInv s of
				Just a -> searchInv s
				Nothing -> NoWork
				

 searchInv :: [State] -> Works
 searchInv (s:ss) = if (getType s /= Start) then searchInv (ss++[s]) 
		     else case find isAS (sSuc ((head.getOut) s) ss) of
				Nothing -> NoWork
				Just a -> case find (forInvT ss) (getOut a) of
						Just t -> SequentialW (fst (mkInvSeq t ss))
						Nothing -> NoWork
	where forInvT ss t = (sSuc t ss /= [])

 mkInvM :: [State] -> [Transition] -> ([Works], Maybe Transition)
 mkInvM ss tt =  case find (((/=).head.snd) mAll) ((tail.snd) mAll) of
			Just a -> ((fst mAll),Nothing)
			Nothing -> ((fst mAll),((head.snd) mAll))
	where mAll = (unzip.(map (mkInvs ss))) tt

 mkInvs :: [State] -> Transition -> (Works, Maybe Transition)
 mkInvs (s:ss) t 
	| elem t (getIn s) && isXS s = (ChoiceW (fst mkM),(snd mkM))
	| elem t (getIn s) && isAS s = (ChoiceW (fst mkM),(snd mkM))
	| elem t (getIn s) && (isTask.getType) s = (SequentialW (fst mkS),(snd mkS))
	| elem t (getIn s) && (isAJ s || isXJ s) = (NoWork, Just ((head.getOut) s))
	| elem t (getIn s) && (isEnd.getType) s = (NoWork, Nothing)
	| not (elem t (getIn s)) = mkInvs (ss++[s]) t
	where mkM = (mkInvM ss (getOut s))
	      mkS = (mkInvSeq t ([s]++ss))

 mkInvSeq :: Transition -> [State] -> ([Works], Maybe Transition)
 mkInvSeq t ss = if (last.fst) mAlluzip == NoWork then ((init.fst) mAlluzip,(last.snd) mAlluzip)
		  else (fst mAlluzip,(last.snd) mAlluzip)
	where mAlluzip = unzip (mkInvS t ss)
	
 mkInvS :: Transition -> [State] -> [(Works, Maybe Transition)]
 mkInvS t ss = case sSuc t ss of
		[] -> []
		[a] -> (if (isXS a) then [((ChoiceW ((fst.invmult) a)),((snd.invmult) a))] ++ carryOn ((snd.invmult) a)
		        else (if (isAS a) then [((ParW ((fst.invmult) a)),((snd.invmult) a))] ++ carryOn ((snd.invmult) a)
		       	      else if (isAJ a || isXJ a) then [(NoWork,Just ((head.getOut) a))]
		       	    	   else (if (isMiseq.getType) a then [(mkW a,Just ((head.getOut) a))] ++ (mkInvS ((head.getOut) a) ss) 
				  	 else [])))
	where invmult s = mkInvM ss (getOut s)
	      mkW s = Wk (((taskToWork.fst4.invMiseq.getType) s),(getTreat.trd4.invMiseq.getType) s,(getT.fst.getRange) s,(getT.snd.getRange) s,Rep 0)
	      carryOn jt = case jt of
				Just t -> mkInvS t ss
				Nothing -> []
	      getTreat (InterventionT (Intervention n q m)) = Treatment n q m
	      getTreat _ = NoTreatment

