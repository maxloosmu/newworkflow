> module BPMNToXML where

> import BPMN

 import WorkflowData
 
> import List (unzip5,zip4,intersect,(\\),find)
> import Char (isDigit)
> import XMLToBPMN (getProperty,getId,getName,findSubContent)
> import Text.XML.HaXml.Pretty (document)
> import Text.PrettyPrint.HughesPJ (Doc)
> import Text.XML.HaXml.Types

> bToxf :: BPMN -> FilePath -> IO()
> bToxf b f = if f == "" then putStr (((show.document.bTox) b)++"\n")
>	      else writeFile f ((show.document.bTox) b)  

> bTox :: BPMN -> Document
> bTox b = (Document prolog emptyST (Elem "diagram" [] (foldr mkPandS [] b)) [])	
>	where prolog = (Prolog (Just (XMLDecl "1.0" (Just (EncodingDecl "UTF-8")) Nothing)) [] Nothing [])

> mkPandS :: (CName,[StateSet]) -> [Content] -> [Content]
> mkPandS (n,sp) elems = if null elems then [(CElem (Elem "Pool" [("id",(AttValue [Left (incre "pool")]))] (stdProperties++[(mkProp "Name" n)]) ))] ++ (mkStates 2 n sp )
>			 else [(CElem (Elem "Pool" [("id",(AttValue [Left nid]))] ((mkProperties ["Length","y","x"])++[(mkProp "sdm:y" "71.740005"),(mkProp "sdm:x" "642.34753"),(mkProp "Size" "160.01791"),(mkProp "Name" n)]) ))] ++ (mkStates int n sp) ++elems
>	where nid = (incre (getProperty (head elems) "id"))
>	      int = (read (dropWhile (not.isDigit) (incre nid)) :: Int)

> mkStates :: Int -> CName -> [StateSet] -> [Content]
> mkStates n c ss = content++links
>	where (content,fin,fout,ferror,is) = unzip5 (mkStates' n c ss)
>	      links = (mkLinks (zip4 content fin fout ferror) (zip4 content fin fout ferror))

> mkStates' :: Int -> CName -> [StateSet] -> [(Content,[String],[String],[(Type,String)],Int)]
> mkStates' _ _ [] = []
> mkStates' n c (s:ss) = case s of
>			(Atomic t) -> content1++(mkStates' ((fth5.last) content1) c ss) 
>			(SubProcess t tt) -> [((CElem (Elem "SubProcess" [("id",(AttValue [Left ("SubProcess"++(show n))]))] 
>						      ((mkProperties ["y","x"])
>						       ++ (if (isBpmn.getType) t
>					     		   then [(mkProp "sdm:y" "35.0"),(mkProp "sdm:x" "170.0"),(mkProp "Name" ((fst.invBpmn.getType) t)),(mkProp "BpmnType" ((show.snd.invBpmn.getType) t))]
>							   else [(mkProp "sdm:y" "205.0"),(mkProp "sdm:x" "170.0"),(mkProp "Name" ((fst4.invMiseqs.getType) t)),(mkProp "BpmnType" ((show.trd4.invMiseqs.getType) t)),
>								 (mkProp "LoopType" "true"),(mkProp "Loop" (if (isFix.snd4.invMiseqs.getType) t then "Fix" else "Ndet")), 
>				         			 (mkProp "Iteration" ((show.getLoops.snd4.invMiseqs.getType) t))] )
>						       ++ [(mkProp "Expanded" "false"),(mkProp "Transaction" "false"),(mkProp "CSSclass" "Activity")]++ content2
>						       ++ (if c == "" then [] else [(mkProp "Lane" c)]) ))),
>						(snd.unzip.getIn) t,(snd.unzip.getOut) t,
>						(if (null.getError) t then [] else ((map mkErrorData).getError) t),last i2)]++(mkStates' (last i2) c ss)
>	where mks x xs = if null xs then [(mkonestate n c x)] else [(mkonestate ((fth5.head) xs) c x)]++xs
>	      content1 = (foldr mks [] (getAT s))
>	      (c2,f1,f2,fe,i2) = unzip5 (mkStates' (n+1) "" ((snd.getSP) s))
>	      link2 = (mkLinks (zip4 c2 f1 f2 fe) (zip4 c2 f1 f2 fe))
>	      content2 = c2++link2

			<SubProcess id="SubProcess367314577">
				<property name="LoopType">true</property>
				<property name="sdm:y">205.0</property>
				<property name="sdm:x">170.0</property>
				<property name="Expanded">false</property>
				<property name="Iteration">4</property>
				<property name="Name">B1_EC</property>
				<property name="CSSclass">Activity</property>
				<property name="Transaction">false</property>
				<property name="y">-76.171814</property>
				<property name="x">504.70062</property>
				<property name="Loop">Fix</property>
				<property name="BpmnType">InterventionB</property>


 mkErrorData :: (Type,[Transition]) -> (Type,[String])
 mkErrorData (t,ts) = (t,(snd.unzip) ts)

> mkErrorData :: (Type,Transition) -> (Type,String)
> mkErrorData (t,ts) = (t,snd ts)


> fth5 :: (a,b,c,d,e) -> e
> fth5 (a,b,c,d,e) = e

<property name="Expanded">false</property>
<property name="CSSclass">Activity</property>
<property name="Transaction">false</property>

> mkLinks :: [(Content,[String],[String],[(Type,String)])] -> [(Content,[String],[String],[(Type,String)])] -> [Content]
> mkLinks [] _ = []
> mkLinks ((c1,i1,o1,e1):c3) cs = lcs ++ (mkLinks nc3 cs)
>	where lcs = buildLinks c1 i1 o1 e1 cs
>	      lid = map getId lcs
>	      nc3 = map (rmIO lid) c3
>	      rmIO ids (c,ins,outs,ees) = (c, ins \\ ids, outs \\ ids, concatMap (cleare ids) ees)
>	      cleare ids' (t',es')  = if elem es' ids' then [] else [(t',es')] 

	      rmIO ids (c,ins,outs,ees) = (c, ins \\ ids, outs \\ ids, concatMap (cleare ids) ees)
	      cleare ids' (t',es')  = if (es' \\ ids') == [] then [] else [(t',es' \\ ids')] 

	      ncs = map (rmIO lid) cs

  where fbegin (cc,ii,oo) = ((getType cc == Start) || (isStime.getType) cc)

> buildLinks :: Content -> [String] -> [String] -> [(Type,String)] -> [(Content,[String],[String],[(Type,String)])] -> [Content]
> buildLinks c is os es cs = concat ((map out os) ++ (map inn is) ++ (map err es)) 
>	where out ol = case find ((elem ol).snd4) cs of
>				Just (c2,_,_,_) -> [(CElem (Elem "Flow" [("islink",(AttValue [Left "true"])),("id",(AttValue [Left ol])),("from",(AttValue [Left (getId c)])),("to",(AttValue [Left (getId c2)]))] (mkProperties ["x","y"])))]
>				Nothing -> []
>	      inn il = case find ((elem il).trd4) cs of
>			Just (c2,_,_,_) -> [(CElem (Elem "Flow" [("islink",(AttValue [Left "true"])),("id",(AttValue [Left il])),("from",(AttValue [Left (getId c2)])),("to",(AttValue [Left (getId c)]))] (mkProperties ["x","y"])))]
>			Nothing -> case find (fdError il) (filter (not.null.fth4) cs) of
>					Just (c3,_,_,e3) -> [(CElem (Elem "Flow" [("islink",(AttValue [Left "true"])),("id",(AttValue [Left il])),("from",(AttValue [Left (findErrorsId il e3 c3)])),("to",(AttValue [Left (getId c)]))] ((mkProperties ["x","y"])++[(mkProp "CSSclass" "FromEvent")])))]
>					Nothing -> []
>	      fdError l (_,_,_,ee) = ((elem l).snd.unzip) ee
>	      err (t,el) = case find ((elem el).snd4) cs of
>				Just (c2,i2,_,_) -> [(CElem (Elem "Flow" [("islink",(AttValue [Left "true"])),("id",(AttValue [Left el])),("from",(AttValue [Left (findErrorId t c)])),("to",(AttValue [Left (getId c2)]))] ((mkProperties ["x","y"])++[(mkProp "CSSclass" "FromEvent")])))]
>				Nothing -> []

				Just (c2,i2,_,_) -> [(CElem (Elem "Flow" [("islink",(AttValue [Left "true"])),("id",(AttValue [Left ((head.(intersect el)) i2)])),("from",(AttValue [Left (findErrorId t c)])),("to",(AttValue [Left (getId c2)]))] ((mkProperties ["x","y"])++[(mkProp "CSSclass" "FromEvent")])))]
				Nothing -> []

	      err (t,el) = case find (not.null.(intersect el).snd4) cs of
				Just (c2,i2,_,_) -> [(CElem (Elem "Flow" [("islink",(AttValue [Left "true"])),("id",(AttValue [Left ((head.(intersect el)) i2)])),("from",(AttValue [Left (findErrorId t c)])),("to",(AttValue [Left (getId c2)]))] ((mkProperties ["x","y"])++[(mkProp "CSSclass" "FromEvent")])))]
				Nothing -> []
 
> findErrorsId :: String -> [(Type,String)] -> Content -> String
> findErrorsId l ee cc = case find ((== l).snd) ee of
>			     Just (t,et) -> findErrorId t cc
>			     Nothing -> ""

> findErrorId :: Type -> Content -> String
> findErrorId tt cc = case find ((==(typ)).((flip getProperty) "Trigger")) (filter ((=="Intermediate").getName) (findSubContent cc)) of
>			Just inter -> getId inter
>			Nothing -> ""
>	where typ = if (isIerror tt) then "Exception" else "Timer"

head ((intersect el))

<Flow from="Task16" id="Flow18" islink="true" to="Gateway24">
    <property name="x">100.98547</property>
    <property name="y">73.4977</property>
</Flow>

> mkonestate :: Int -> CName -> State -> (Content,[String],[String],[(Type,String)],Int)
> mkonestate i n s = if (null.getError) s then (c,(snd.unzip.getIn) s,(snd.unzip.getOut) s,[],ni)
>		     else  (c,(snd.unzip.getIn) s,(snd.unzip.getOut) s,((map mkErrorData).getError) s,ni)
>	where (c,ni) = mkones i n s

> mkones :: Int -> CName -> State -> (Content,Int)
> mkones i n (State ty it ot e tr im om rm am bm l)
>	| ( ty == Start || isStime ty ) = ((CElem (Elem "Start" [("id",(AttValue [Left ("Start"++(show i))]))] 
>				     		       (lane++[(mkProp "sdm:y" "177.0"),(mkProp "sdm:x" "337.0"),(mkProp "y" "1.0"),(mkProp "x" "1.0"),(mkProp "CSSclass" "Event"),(mkProp "height" "30.0"),(mkProp "width" "30.0")]
>							    ++(if isStime ty then [(mkProp "Trigger" "Timer"),(mkProp "Time" ((getTime.invStime) ty))] else []) ) )),i+1)
>	| isEnd ty = ((CElem (Elem "End" [("id",(AttValue [Left (incre ("End"++(show (invEnd ty))))]))] 
>				   (lane++[(mkProp "sdm:y" "107.0"),(mkProp "sdm:x" "417.0"),(mkProp "y" "1.0"),(mkProp "x" "1.0"),(mkProp "CSSclass" "Event"),(mkProp "height" "30.0"),(mkProp "width" "30.0")]) ) ),i)
>	| isAbort ty = ((CElem (Elem "End" [("id",(AttValue [Left (incre ("End"++(show (invAbort ty))))]))] 
>				   (lane++[(mkProp "sdm:y" "421.47876"),(mkProp "sdm:x" "416.47876"),(mkProp "y" "1.0"),(mkProp "x" "1.0"),(mkProp "Trigger" "Terminate"),(mkProp "CSSclass" "Event"),(mkProp "height" "30.0"),(mkProp "width" "30.0")]) ) ),i)
>	| ty == Xgate = ((CElem (Elem "Gateway" [("id",(AttValue [Left ("Gateway"++(show i))]))]
>				   ((mkProperties ["y","x"])++lane++[(mkProp "height" "51.414215"),(mkProp "width" "51.414215"),(mkProp "GatewayType" "XOR"),(mkProp "Name" "Data-XOR")]) ) ),i+1)
>	| ty == Agate = ((CElem (Elem "Gateway" [("id",(AttValue [Left ("Gateway"++(show i))]))]
>				   ((mkProperties ["y","x"])++lane++[(mkProp "height" "51.414215"),(mkProp "width" "51.414215"),(mkProp "GatewayType" "AND"),(mkProp "Name" "AND")]) ) ),i+1)
>	| isTask ty =  ((CElem (Elem "Task" [("id",(AttValue [Left ("Task"++(show i))]))] 
>				     ((mkProperties ["y","x"])++lane
>				      ++[(mkProp "sdm:y" "35.0"),(mkProp "sdm:x" "60.0"),(mkProp "TaskType" ((show.snd.invTask) ty)),(mkProp "Name" ((fst.invTask) ty)),(mkProp "CSSclass" "Activity"),
>					 (mkProp "Maxrange" ((getTime.snd) tr)),(mkProp "Minrange" ((getTime.fst) tr))]++(mkIntervention ((snd.invTask) ty))++interrupt) ) ),i+1)
>	| isMiseq ty = ((CElem (Elem "Task" [("id",(AttValue [Left ("Task"++(show i))]))] 
>				     ((mkProperties ["y","x"])++lane
>				      ++[(mkProp "sdm:y" "205.0"),(mkProp "sdm:x" "60.0"),(mkProp "TaskType" ((show.trd4.invMiseq) ty)),(mkProp "Name" ((fst4.invMiseq) ty)),(mkProp "CSSclass" "Task"),(mkProp "Loop" (if (isFix.snd4.invMiseq) ty then "Fix" else "Ndet")),
>				         (mkProp "Iteration" ((show.getLoops.snd4.invMiseq) ty)),(mkProp "Maxrange" ((getTime.snd) tr)),(mkProp "Minrange" ((getTime.fst) tr)),(mkProp "LoopType" "true")]++(mkIntervention ((trd4.invMiseq) ty))++interrupt) ) ),i+1)
>	where lane = if n == "" then [] else [(mkProp "Lane" n)]
>	      interrupt = if null e then [] else foldr mkInterrupt [] e
> 	      mkInterrupt (ty,ts) cs = [(CElem (Elem "Intermediate" [("id",(AttValue [Left (if null cs then (incre "Intermediate"++(show (i+1))) else (incre.getId.head) cs) ]))] 
>					((mkProperties ["sdm:y","sdm:x"])++[(if isItime ty then mkProp "Trigger" "Timer" else mkProp "Trigger" "Exception")]
>					++[(mkProp "y" "1.0"),(mkProp "x" "1.0"),(mkProp "CSSclass" "Event"),(mkProp "height" "30.0"),(mkProp "width" "30.0"),(mkProp "Interrupt" "true")])))]
>					++cs

> mkIntervention :: TaskType -> [Content]
> mkIntervention (InterventionT (Intervention n d m)) = [(mkProp "IName" n),(mkProp "IDose" d),(mkProp "Method" m)]
> mkIntervention _ = []

 mkInterrupt :: (Type,[Transition]) -> [(Content,[String],[String],Int)] -> [(Content,[String],[String],Int)]
 
<property name="x">355.016</property>
<property name="Trigger">Timer</property>
<property name="CSSclass">Event</property>
<property name="sdm:y">177.0</property>
<property name="sdm:x">377.0</property>
<property name="Time">14D</property>
<property name="Interrupt">true</property>
<property name="y">160.0</property>
 
> getTime :: Time -> String
> getTime NOBOUND = ""
> getTime (MkTime ve 0 0 0 0 0 0) = "P0D"
> getTime (MkTime ve yr mh dy hr mn sc) = if null t then "P0D" else t 
>	where t = if (hr == 0 && mn == 0 && sc == 0) then ((gDi ve)++(gCo yr "Y")++(gCo mh "M")++(gCo dy "D"))
>		  else ((gDi ve)++(gCo yr "Y")++(gCo mh "M")++(gCo dy "D")++"T"++(gCo hr "H")++(gCo mn "M")++(gCo sc "S"))
>	      gDi ve = (if ve == Pve then "" else "-")
>	      gCo i t = (if i == 0 then "" else (show i)++t)
> getTime ZERO = "P0D"

> incres :: String -> Int -> [String]
> incres s is = zipWith (++) (replicate is s) (map show ns)
>	where ns = if null ci then enumFromTo 1 is else enumFromTo (read ci :: Int) ((read ci :: Int)+is)  
>	      ci = snd (span (not.isDigit) s)

> incre :: String -> String
> incre s = if null int then st++"1" else st++(show ((read int :: Int) + 1))  
>	where (st,int) = span (not.isDigit) s

 tab :: EntityDef 
 tab = (DefExternalID (SYSTEM (SystemLiteral "")) Nothing)	

	<property name="width">1326.1338</property>
	<property name="sdm:y">71.740005</property>
	<property name="sdm:x">642.34753</property>
	<property name="Size">775.609</property>
	<property name="Length">1326.1338</property>
	<property name="y">-114.0</property>
	<property name="x">-602.7357</property>
	<property name="height">775.609</property>
	
	  [ (CElem (Elem "property" [("name",(AttValue [Left "width"]))] [CString False ""] ) ),
	    (CElem (Elem "property" [("name",(AttValue [Left "height"]))] [CString False ""] ) ),
	    (CElem (Elem "property" [("name",(AttValue [Left "sdm:y"]))] [CString False ""] ) ),
	    (CElem (Elem "property" [("name",(AttValue [Left "sdm:x"]))] [CString False ""] ) ),
	    (CElem (Elem "property" [("name",(AttValue [Left "Length"]))] [CString False ""] ) ),
	    (CElem (Elem "property" [("name",(AttValue [Left "Size"]))] [CString False ""] ) ),
	    (CElem (Elem "property" [("name",(AttValue [Left "y"]))] [CString False ""] ) ),
	    (CElem (Elem "property" [("name",(AttValue [Left "x"]))] [CString False ""] ) ) ]

> mkProperties :: [String] -> [Content]
> mkProperties = map ((flip mkProp) "")

> stdProperties :: [Content]
> stdProperties = mkProperties ["height","width","sdm:y","sdm:x","Length","Size","y","x"]

> mkProp :: String -> String -> Content
> mkProp n s = (CElem (Elem "property" [("name",(AttValue [Left n]))] [CString False s] ) )

