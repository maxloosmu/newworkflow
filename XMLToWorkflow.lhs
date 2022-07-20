> module XMLToWorkflow where

> import Text.XML.HaXml.Types
> import Text.XML.HaXml.Parse (xmlParse)
> import XMLToBPMN (getId,getName,mkInt,ckElement)
> import List (find,elemIndex)
> import Workflow

> import Text.XML.HaXml.Verbatim

 import WorkflowData

> import Char (toLower,toUpper)


 xmlToOutf :: FilePath -> IO()
 xmlToOutf i = do { xml <- readFile i;
			  putStr ((show (xmlToOut (xmlParse i xml)))++"\n") }

 xmlToOut :: Document -> String
 xmlToOut (Document p st (Elem "workflow" atts cs) ms) = getAtt "xsi:schemaLocation" (CElem (Elem "workflow" atts cs))
 xmlToOut _ = []


> xTowf :: FilePath -> IO()
> xTowf i = do { xml <- readFile i;
>		 putStr ((show (xTow (xmlParse i xml)))++"\n") }

> xTow :: Document -> Workflow
> xTow (Document p st (Elem "workflow" atts cs) ms) = concatMap mkEventSeq cs 
> xTow _ = []

> mkEventSeq :: Content -> [EventSequencing]
> mkEventSeq (CElem (Elem "eventSequencing" atts cs)) = [ (Event (getENme "activityId" atts) (getPreA cs) (getDptE cs) (getESCond cs) (getETCond cs)  (getObv cs) (getERpt cs) ((normW.getEWk) cs)) ]
> mkEventSeq _ = []

> getENme :: String -> [Attribute] -> ActivityId
> getENme s ass = case gAtt s ass of
>		    Just id -> fid id
>		    Nothing -> error "No identifier"
>	where fid id
>		| isSame "start" id = START
>		| isSame "stop" id = STOP
>		| isSame "abnormal stop" id || isSame "abnormal_stop" id = ABNORMAL_STOP
>		| isSame "normal stop" id || isSame "normal_stop" id = NORMAL_STOP
>		| otherwise = Id id

> getPreA :: [Content] -> PreAct
> getPreA cs = case find ((=="prerequisites").getName) cs of
>		 Just (CElem (Elem n a css)) -> (gPreA . head) (filter isElement css)
>		 Nothing -> (Pa START)

> gPreA :: Content -> PreAct
> gPreA (CElem (Elem "prerequisiteGroup" a cs)) = (gPreA . head) (filter isElement cs)  
> gPreA (CElem (Elem "oneOf" a cs)) = (OneOf (map gPreA (filter isElement cs)))
> gPreA (CElem (Elem "all" a cs)) = (All (map gPreA (filter isElement cs)))
> gPreA (CElem (Elem "prerequisite" a cs)) = (Pa . getENme "prerequisiteActivityId") a
> gPreA c = (error.getName) c

> getDptE :: [Content] -> DptEvent
> getDptE cs = case find ((=="dependent").getName) cs of
>		 Just (CElem (Elem n a css)) -> (gDptE . head) (filter isElement css)
>		 Nothing -> (De END)

> gDptE :: Content -> DptEvent
> gDptE (CElem (Elem "dependentGroup" a cs)) = (gDptE . head) (filter isElement cs)  
> gDptE (CElem (Elem "oneOf" a cs)) = (OneOfD (map gDptE (filter isElement cs)))
> gDptE (CElem (Elem "all" a cs)) = (AllD (map gDptE (filter isElement cs)))
> gDptE (CElem (Elem "dependent" a cs)) = (De . getENme "dependentActivityId") a

----------- Get all condition -------------------------------- 

> getESCond :: [Content] -> Condition
> getESCond = getECond "start"

> getETCond :: [Content] -> Condition
> getETCond = getECond "terminate"

> getECond :: String -> [Content] -> Condition
> getECond s cs = case find (ckElement s) cs of
>			Just (CElem (Elem n a css)) -> Ands (map getOrs css) 
>			Nothing -> Workflow.None

> getOrs :: Content -> Ors
> getOrs (CElem (Elem "disjunct" a cs)) = Ors (map getOr cs)

> getOr :: Content -> SCondition
> getOr (CElem (Elem "condition" a [(CElem (Elem "range" ra rcs)),(CElem (Elem "property" pa pcs))] )) = ((getERange.head) rcs, gPp pa)

> gPp :: [Attribute] -> String
> gPp prop = case gAtt "ID" prop of
>		Just p -> p
>		Nothing -> error "Properties with no identifier"

> getERange :: Content -> Range
> getERange (CElem (Elem "valueSet" sa scss)) = Emu (map getEmu scss) where getEmu (CElem (Elem "enumerationCode" a [CString False ecode])) = ecode
> getERange (CElem (Elem "valueInterval" ia icss)) = Bound lb ub
>		where lb = case find (ckElement "lowerBound") icss of
>				Just c -> getERge c
>				Nothing -> NoBound
>		      ub = case find (ckElement "upperBound") icss of
>				Just c -> getERge c
>				Nothing -> NoBound

> getERge :: Content -> RangeBound
> getERge (CElem (Elem "absoluteDate" a [(CString False d)])) = if null d then NoBound else Abdate (Dur d)
> getERge (CElem (Elem "absoluteInteger" a [(CString False d)])) = if null d then NoBound else Abint (mkInt d)
> getERge (CElem (Elem "absoluteDecimal" a [(CString False d)])) = if null d then NoBound else Abdec (mkDecimal d)
> getERge (CElem (Elem "evalToDate" a [(CElem (Elem "offset" _ [CString False off])),(CElem (Elem "property" pa pc))])) = Rldate (gPp pa) (if null off then error "offset no defined" else (Dur off))
> getERge (CElem (Elem "evalToInteger" a [(CElem (Elem "offset" _ [CString False off])),(CElem (Elem "property" pa pc))])) = Rlint (gPp pa) (if null off then error "offset no defined" else (mkInt off))
> getERge (CElem (Elem "evalToDecimal" a [(CElem (Elem "offset" _ [CString False off])),(CElem (Elem "property" pa pc))])) = Rldec (gPp pa) (if null off then error "offset no defined" else (mkDecimal off))

----------- Get observations --------------------------------

> getObv :: [Content] -> DptAct
> getObv cs = case find ((=="observations").getName) cs of
>		 Just (CElem (Elem n a css)) -> (gObvS . head) (filter isElement css)
>		 Nothing -> NoDepend

> gObvS :: Content -> DptAct
> gObvS (CElem (Elem "observationGroup" a cs)) = (gObvS . head) (filter isElement cs)  
> gObvS (CElem (Elem "parallel" a cs)) = (ParD (map gObvS (filter isElement cs)))
> gObvS (CElem (Elem "choice" a cs)) = (ChoiceD (map gObvS (filter isElement cs)))
> gObvS (CElem (Elem "sequential" a cs)) = (SequentialD (map gObvS (filter isElement cs)))
> gObvS (CElem (Elem "observation" a cs)) = (Da  ((getENme "activityId" a), (getXWT "minDelay" a), (getXWT "maxDelay" a), oc, (getECond "condition" cs), (getAType cs)))
>  		where  oc = case (gAtt "occur" a) of
>	    		Just m -> Rep (mkInt m)
>	    		Nothing -> Any

> getAType :: [Content] -> ActType
> getAType cs = case find ((=="actType").getName) cs of
>		 Just (CElem (Elem n a ((CString False "Manual"):css))) -> Manual
>		 Just (CElem (Elem n a ((CString False "Auto"):css))) -> Automatic
>		 Nothing -> Manual

> getXWT :: String -> [Attribute] -> Duration
> getXWT s ass = case gAtt s ass of
>		  Just a -> Dur a
>		  Nothing -> UNBOUNDED

> getERpt :: [Content] -> [RepeatExp]
> getERpt = concatMap getER

> getER :: Content -> [RepeatExp]
> getER (CElem (Elem "eventRepeat" a cs)) = 
>	let 
>	   mid = case (gAtt "minDelay" a) of
>		    Just m -> Dur m
>		    Nothing -> UNBOUNDED
>	   mad = case (gAtt "maxDelay" a) of
>		    Just m -> Dur m
>		    Nothing -> UNBOUNDED
>	   mio = case (gAtt "minOccur" a) of
>		    Just m -> Rep (mkInt m)
>		    Nothing -> Any
>	   mao = case (gAtt "maxOccur" a) of
>		    Just m -> Rep (mkInt m)
>		    Nothing -> Any
>	in [(mid,mad,mio,mao,getECond "condition" cs)]
> getER _ = []

> getEWk :: [Content] -> Works
> getEWk cs = case find ((=="workUnits").getName) cs of
>		 Just (CElem (Elem n a css)) -> (gWkS . head) (filter isElement css)
>		 Nothing -> NoWork

> gWkS :: Content -> Works
> gWkS (CElem (Elem "workGroup" a cs)) = (gWkS . head) (filter isElement cs)  
> gWkS (CElem (Elem "work_parallel" a cs)) = (ParW (map gWkS (filter isElement cs)))
> gWkS (CElem (Elem "work_choice" a cs)) = (ChoiceW (map gWkS (filter isElement cs)))
> gWkS (CElem (Elem "work_sequential" a cs)) = (SequentialW (map gWkS (filter isElement cs)))
> gWkS (CElem (Elem "work" a cs)) =
>		let 
>		     id = case gAtt "workId" a of
>				Just s -> s
>				Nothing -> error "No work has been defined"
>		     mi = case gAtt "minDelay" a of
>				Just s -> Dur s
>				Nothing -> UNBOUNDED
>		     ma = case gAtt "maxDelay" a of
>				Just s -> Dur s
>				Nothing -> UNBOUNDED
>	  	     oc = case (gAtt "occur" a) of
>		    		Just m -> Rep (mkInt m)
>		    		Nothing -> Any
>		     ty = case find ((=="treatment").getName) cs of 
>				Just (CElem (Elem n a css)) -> getWType css
>				Nothing -> NoTreatment
>		in (Wk (WorkSingle (id,ty,mi,ma,oc)))
> gWkS (CElem (Elem "works" a cs)) =
>		let 
>		     id = case gAtt "workId" a of
>				Just s -> s
>				Nothing -> error "No work has been defined"
>		     mi = case gAtt "minDelay" a of
>				Just s -> Dur s
>				Nothing -> UNBOUNDED
>	  	     oc = case (gAtt "occur" a) of
>		    		Just m -> Rep (mkInt m)
>		    		Nothing -> Any
>		     ty = case find ((=="workBlock").getName) cs of 
>				Just (CElem (Elem n a css)) -> getEWkB css
>				Nothing -> NoWorkB
>		in Wk (WorkMult (id,ty,mi,oc))
> gWkS c = (error "gWkS")

> getEWkB :: [Content] -> WorkBlock
> getEWkB cs = (gWkSB . head) (filter isElement cs)

> gWkSB :: Content -> WorkBlock
> gWkSB (CElem (Elem "workBlock" a cs)) = (gWkSB . head) (filter isElement cs)  
> gWkSB (CElem (Elem "workbk_parallel" a cs)) = (ParWB (map gWkSB (filter isElement cs)))
> gWkSB (CElem (Elem "workbk_choice" a cs)) = (ChoiceWB (map gWkSB (filter isElement cs)))
> gWkSB (CElem (Elem "workbk_sequential" a cs)) = (SequentialWB (map gWkSB (filter isElement cs)))
> gWkSB (CElem (Elem "workunit" a cs)) =
>		let 
>		     id = case gAtt "workId" a of
>				Just s -> s
>				Nothing -> error "No work has been defined"
>		     mi = case gAtt "minDelay" a of
>				Just s -> Dur s
>				Nothing -> UNBOUNDED
>		     ma = case gAtt "maxDelay" a of
>				Just s -> Dur s
>				Nothing -> UNBOUNDED
>	  	     ty = case find ((=="treatment").getName) cs of 
>				Just (CElem (Elem n a css)) -> getWType css
>				Nothing -> NoTreatment
>		in WorkUnit (id,ty,mi,ma)

> getWType :: [Content] -> Treatment
> getWType cs = (Treatment n q m)
>	where n = case find ((=="name").getName) cs of
>			Just (CElem (Elem n a css)) -> case (head css) of
>							 (CString b t) -> t
>							 _ -> error "No treatment name has been defined"
>	      q = case find ((=="quantity").getName) cs of
>			Just (CElem (Elem n a css)) -> case (head css) of
>							 (CString b t) -> t
>							 _ -> error ("No dosage has been defined for "++n)
>	      m = case find ((=="method").getName) cs of
>			Just (CElem (Elem n a css)) -> case (head css) of
>							 (CString b t) -> t
>							 _ -> error ("No method has been defined for "++n)

getAttribute 

> gAtt :: String -> [Attribute] -> Maybe String
> gAtt s [] = Nothing
> gAtt s ((st,(AttValue (at:att))):ass) = 
>		if (s == st) then case at of
>			(Left a) -> Just a
>			(Right b) -> Nothing
>		else gAtt s ass

> isSame :: [Char] -> [Char] -> Bool
> isSame cs ds = length cs == length ds && (and . map isS . zip cs) ds 
>	where isS (c,d) = (toLower d == toLower c) 

> mkDecimal :: String -> Float
> mkDecimal s = case elemIndex '.' s of
>		 Just ind -> intf + (decf / place) 
>			where (integer,integral) = (splitAt ind s)  
>	      		      intf = (fromInteger . toInteger . mkInt) integer
>			      decf = (fromInteger . toInteger . mkInt . tail) integral
>	      		      place = (fromInteger . toInteger . (10 ^) . length . tail) integral
>		 Nothing -> (fromInteger . toInteger . mkInt) s :: Float

 getAtt :: String -> Content -> String	
 getAtt s (CElem (Elem nme atts cs)) = (gAtt s atts)
	where 	gAtt s [] = ""
		gAtt s ((st,(AttValue (at:att))):ass) = 
					if (s == st) then case at of
								(Left a) -> a
								(Right b) -> ""
					else gAtt s ass
		gAtt s (a:ass) = gAtt s ass
 getAtt _ _ = ""

Return the tag name of a XML element

 getName :: Content -> Name
 getName (CElem (Elem nme atts cs)) = nme
 getName _ = ""

Get subelement given name

 findSubContent :: Content -> String -> [Content]
 findSubContent (CElem (Elem nme ms cs)) sch = filter (ckElement sch) cs

> isElement :: Content -> Bool
> isElement (CElem (Elem nme cs ms)) =  True 
> isElement _ = False

 ckElement :: Name -> Content -> Bool
 ckElement n (CElem (Elem nme cs ms)) =  (n == nme) 
 ckElement _ _ = False
