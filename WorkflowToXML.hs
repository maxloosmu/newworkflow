  module WorkflowToXML where
  import Workflow
  import Data.List (unzip5,zip4,intersect,(\\),find)
  import Char (isDigit)
  import XMLToBPMN (getProperty,getId,getName,findSubContent)
  import Text.XML.HaXml.Pretty (document)
  import Text.PrettyPrint.HughesPJ (Doc)
  import Text.XML.HaXml.Types
  wToxf :: Workflow -> FilePath -> IO()
  wToxf w f = if f == "" then putStr (((show.document.wTox) w)++"\n")
              else writeFile f ((show.document.wTox) w)
  wTox :: Workflow -> Document
  wTox w = (Document prolog emptyST (Elem "workflow" atts (foldr mkevent [] w)) [])
        where prolog = (Prolog (Just (XMLDecl "1.0" (Just (EncodingDecl "UTF-8")) Nothing)) [] Nothing [])
              atts = [("xmlns",(AttValue [Left "http://www.comlab.ox.ac.uk/peter.wong/observation"])),
                      ("xmlns:xsi",(AttValue [Left "http://www.w3.org/2001/XMLSchema-instance"])),
                      ("xsi:schemaLocation",(AttValue [Left "http://www.comlab.ox.ac.uk/peter.wong/observation workflow.xsd"]))]
  mkevent :: EventSequencing -> [Content] -> [Content]
  mkevent (Event n p d s t o r w) cs = ((CElem (Elem "eventSequencing" [("activityId",(AttValue [Left (gid n)]))] (pre++dev++sts++ter++obvs++rpt++wks))):cs)
        where   pre = [(CElem (Elem "prerequisites" [] (gPre p)))]
                dev = [(CElem (Elem "dependent" [] (gDEv d)))]
                sts = if s == Workflow.None then [] else (gCon "start" s)
                ter = if t == Workflow.None then [] else (gCon "terminate" s)
                obvs = if o == NoDepend then [] else [(CElem (Elem "observations" [] (gOb o)))]
                rpt = if r == [] then [] else (gRT r)
                wks = if w == NoWork then [] else [(CElem (Elem "workUnits" [] (gWk w)))]
  gPre :: PreAct -> [Content]
  gPre (All ps) = [(CElem (Elem "all" [] (concatMap gPreL ps)))]
  gPre (OneOf ps) = [(CElem (Elem "oneOf" [] (concatMap gPreL ps)))]
  gPre (Pa p) = [ (CElem (Elem "prerequisite" [("prerequisiteActivityId",(AttValue [Left (gid p)]))] []))]
  gPreL :: PreAct -> [Content]
  gPreL (All ps) = [CElem (Elem "prerequisiteGroup" [] [CElem (Elem "all" [] (concatMap gPreL ps))] ) ]
  gPreL (OneOf ps) = [CElem (Elem "prerequisiteGroup" [] [CElem (Elem "oneOf" [] (concatMap gPreL ps))] ) ]
  gPreL other = [CElem (Elem "prerequisiteGroup" []  (gPre other) ) ]
  gDEv :: DptEvent -> [Content]
  gDEv (AllD ps) = [(CElem (Elem "all" [] (concatMap gDEvL ps)))]
  gDEv (OneOfD ps) = [(CElem (Elem "oneOf" [] (concatMap gDEvL ps)))]
  gDEv (De p) = [ (CElem (Elem "dependent" [("dependentActivityId",(AttValue [Left (gid p)]))] []))]
  gDEvL :: DptEvent -> [Content]
  gDEvL (AllD ps) = [CElem (Elem "dependentGroup" [] [CElem (Elem "all" [] (concatMap gDEvL ps))] ) ]
  gDEvL (OneOfD ps) = [CElem (Elem "dependentGroup" [] [CElem (Elem "oneOf" [] (concatMap gDEvL ps))] ) ]
  gDEvL other = [CElem (Elem "dependentGroup" []  (gDEv other) ) ]
  gCon :: String -> Condition -> [Content]
  gCon s c = [(CElem (Elem s [] (concatMap gO (gc c))))]
        where   gc (Ands ss) = ss
                gO (Ors os) = [(CElem (Elem "disjunct" [] (concatMap gC os)))]
  gC :: SCondition -> [Content]
  gC (range,prop) = [(CElem (Elem "condition" [] [rge, getProp prop (getRType rge)]))]
        where rge = getRge range
  getRType :: Content -> String
  getRType (CElem (Elem "range" [] [(CElem (Elem "valueSet" atts cs))])) = "string"
  getRType (CElem (Elem "range" [] [(CElem (Elem _ atts ((CElem (Elem n _ _)):cs)))]))
         | (n == "absoluteDate" || n == "evalToDate") = "date"
         | (n == "absoluteDecimal" || n == "evalToDecimal") = "decimal"
         | (n == "absoluteInteger" || n == "evalToInteger") = "int"
  getProp :: Property -> String -> Content
  getProp prp t = (CElem (Elem "property" [("ID",(AttValue [Left prp]))] [(CElem (Elem "dataType" [] [CString False t]))]))
  getRge :: Range -> Content
  getRge (Bound rb1 rb2) = (CElem (Elem "range" [] [(CElem (Elem "valueInterval" [] ((getBd rb1)++(getBd rb2))))] ))
  getRge (Emu ss) = (CElem (Elem "range" [] [(CElem (Elem "valueSet" [] (map getEmu ss)))] ))
  getBd :: RangeBound -> [Content]
  getBd (Abdate UNBOUNDED) = []
  getBd (Rldate _ UNBOUNDED) = []
  getBd (Abdate (Dur s)) = [CElem (Elem "absoluteDate" [] [CString False s])]
  getBd (Abdec f) = [CElem (Elem "absoluteDecimal" [] [CString False (show f)])]
  getBd (Abint i) = [CElem (Elem "absoluteInteger" [] [CString False (show i)])]
  getBd others = [CElem (Elem ((fst . fst) evalType) [] [(CElem (Elem "offset" [] [CString False ((fst . snd) evalType)])),
                                                         (CElem (Elem "property" [] [(getProp ((snd . snd) evalType) ((snd . fst) evalType)) ]))])]
        where evalType = case others of
                                (Rldate p (Dur s)) -> (("evalToDate","date"),(s,p))
                                (Rldec p s) -> (("evalToDecimal","decimal"),(show s,p))
                                (Rlint p s) -> (("evalToInteger","int"),(show s,p))
  getEmu :: String -> Content
  getEmu s = (CElem (Elem "enumerationCode" [] [CString False s]))
  gOb :: DptAct -> [Content]
  gOb (ParD dd) = [CElem (Elem "parallel" [] (concatMap gObv dd))]
  gOb (ChoiceD dd) = [CElem (Elem "choice" [] (concatMap gObv dd))]
  gOb (SequentialD dd) = [CElem (Elem "sequential" [] (concatMap gObv dd))]
  gOb (Da (a,mi,ma,rp,c,t)) = [(CElem (Elem "observation" ([("activityId",(AttValue [Left (gid a)]))]
                                                         ++ (if mi == UNBOUNDED then [] else [("minDelay",(AttValue [Left (gDu mi)]))])
                                                         ++ (if ma == UNBOUNDED then [] else [("maxDelay",(AttValue [Left (gDu ma)]))])
                                                         ++ (if rp == Any then [] else [("occur",(AttValue [Left (gRe rp)]))]))
                                   ((if c == Workflow.None then [] else (gCon "conditions" c))
                                    ++ [(CElem (Elem "actType" [] [(CString False (show t))] ))])  ))]
  gObv :: DptAct -> [Content]
  gObv (ParD dd) = [CElem (Elem "observationGroup" [] [CElem (Elem "parallel" [] (concatMap gObv dd))] ) ]
  gObv (ChoiceD dd) = [CElem (Elem "observationGroup" [] [CElem (Elem "choice" [] (concatMap gObv dd))] ) ]
  gObv (SequentialD dd) = [CElem (Elem "observationGroup" [] [CElem (Elem "sequential" [] (concatMap gObv dd))] ) ]
  gObv other = [CElem (Elem "observationGroup" [] (gOb other) ) ]
  gRT :: [RepeatExp] -> [Content]
  gRT = map getR
  getR :: RepeatExp -> Content
  getR (mi,ma,mil,mal,c) = (CElem (Elem "eventRepeat" ((if mi == UNBOUNDED then [] else [("minDelay",(AttValue [Left (gDu mi)]))])
                                                       ++ (if ma == UNBOUNDED then [] else [("maxDelay",(AttValue [Left (gDu ma)]))])
                                                       ++ (if mil == Any then [] else [("minOccur",(AttValue [Left (gRe mil)]))])
                                                       ++ (if mal == Any then [] else [("maxOccur",(AttValue [Left (gRe mal)]))]))
                                                       (if c == Workflow.None then [] else gCon "conditions" c)  ))
  gWk :: Works -> [Content]
  gWk (ParW ww) = [CElem (Elem "work_parallel" [] (concatMap gWkg ww))]
  gWk (ChoiceW ww) = [CElem (Elem "work_choice" [] (concatMap gWkg ww))]
  gWk (SequentialW ww) = [CElem (Elem "work_sequential" [] (concatMap gWkg ww))]
  gWk (Wk (WorkSingle (id,t,mi,ma,r))) = [(CElem (Elem "work"
                                                        [("workId",(AttValue [Left id])),
                                                         ("minDelay",(AttValue [Left (gDu mi)])),
                                                         ("maxDelay",(AttValue [Left (gDu ma)])),
                                                         ("occur",(AttValue [Left (gRe r)]))]
                                                        (if t == NoTreatment then []
                                                         else [(CElem (Elem "treatment" [] (gType t)))])))]
  gWk (Wk (WorkMult (id,wb,mi,r))) = [(CElem (Elem "works"
                                                        [("workId",(AttValue [Left id])),
                                                         ("minDelay",(AttValue [Left (gDu mi)])),
                                                         ("occur",(AttValue [Left (gRe r)]))]
                                                        [(CElem (Elem "workBlock" [] (gWkB wb)))]))]
  gWkg :: Works -> [Content]
  gWkg (ParW ww) = [CElem (Elem "workGroup" [] [CElem (Elem "work_parallel" [] (concatMap gWkg ww))] ) ]
  gWkg (ChoiceW ww) = [CElem (Elem "workGroup" [] [CElem (Elem "work_choice" [] (concatMap gWkg ww))] ) ]
  gWkg (SequentialW ww) = [CElem (Elem "workGroup" [] [CElem (Elem "work_sequential" [] (concatMap gWkg ww))] ) ]
  gWkg other = [CElem (Elem "workGroup" [] (gWk other) ) ]
  gWkB :: WorkBlock -> [Content]
  gWkB NoWorkB = []
  gWkB (ParWB ww) = [CElem (Elem "workbk_parallel" [] (concatMap gWkgB ww))]
  gWkB (ChoiceWB ww) = [CElem (Elem "workbk_choice" [] (concatMap gWkgB ww))]
  gWkB (SequentialWB ww) = [CElem (Elem "workbk_sequential" [] (concatMap gWkgB ww))]
  gWkB (WorkUnit (id,t,mi,ma)) = [(CElem (Elem "workunit"
                                                [("workId",(AttValue [Left id])),
                                                 ("minDelay",(AttValue [Left (gDu mi)])),
                                                 ("maxDelay",(AttValue [Left (gDu ma)]))]
                                                 (if t == NoTreatment then []
                                                  else [(CElem (Elem "treatment" [] (gType t)))])))]
  gWkgB :: WorkBlock -> [Content]
  gWkgB (ParWB ww) = [CElem (Elem "workBlock" [] [CElem (Elem "workbk_parallel" [] (concatMap gWkgB ww))] ) ]
  gWkgB (ChoiceWB ww) = [CElem (Elem "workBlock" [] [CElem (Elem "workbk_choice" [] (concatMap gWkgB ww))] ) ]
  gWkgB (SequentialWB ww) = [CElem (Elem "workBlock" [] [CElem (Elem "workbk_sequential" [] (concatMap gWkgB ww))] ) ]
  gWkgB other = [CElem (Elem "workBlock" [] (gWkB other) ) ]
  gType :: Treatment -> [Content]
  gType (Treatment n q m) = [(CElem (Elem "name" [] [(CString False n)])),
                             (CElem (Elem "quantity" [] [(CString False q)])),
                             (CElem (Elem "method" [] [(CString False m)]))]
  gRe :: Repeat -> String
  gRe (Rep i) = show i
  gDu :: Duration -> String
  gDu (Dur "") = "P0D"
  gDu (Dur d) = d
  gid :: ActivityId -> String
  gid (Id s) = s
  gid sp = (show sp)
