  module BPMN where
  import Data.List (nub,intersect)
  type TaskName = String
  type CName = String
  type BName = String
  type PName = String
  type PLName = String
  type Channel = String
  type Line = String
  type Message = String
  type Guard = Bool
  type IName = String
  type IDose = String
  type Method = String
  data Direction = Pve | Nve deriving (Eq,Show)
  data Time = ZERO | NOBOUND | MkTime Direction Integer Integer Integer Integer Integer Integer deriving (Show)
  serial :: Time -> Integer
  serial (MkTime Nve yr mt dy hr mn s) = -1 * (yr * 31556926 + mt * 2629744 + dy * 86400 + hr * 3600 + mn * 60 + s)
  serial (MkTime Pve yr mt dy hr mn s) = yr * 31556926 + mt * 2629744 + dy * 86400 + hr * 3600 + mn * 60 + s
  serial _ = 0
  toTime :: Integer -> Time
  toTime 0 = ZERO
  toTime t = (MkTime dir yr mt dy hr mn s)
        where at = abs t
              dir = if signum t == 1 then Pve else Nve
              (yr,ym) = at `divMod` 31556926
              (mt,mm) = ym `divMod` 2629744
              (dy,dm) = mm `divMod` 86400
              (hr,hm) = dm `divMod` 3600
              (mn,s) = hm `divMod` 60
  instance Eq Time where
        x == y = serial x == serial y
  instance Ord Time where
        x <= y = serial x <= serial y
  instance Num Time where
        x + y = toTime (serial x + serial y)
        x - y = toTime (serial x - serial y)
        x * y = toTime (serial x * serial y)
        abs (MkTime dr yr mt dy hr mn s) = (MkTime Pve yr mt dy hr mn s)
        signum (MkTime Pve yr mt dy hr mn s) = toTime 1
        signum (MkTime Nve yr mt dy hr mn s) = toTime (-1)
        fromInteger x = toTime x
  getDir :: Time -> Direction
  getYr, getMth, getDay, getHr, getMinute, getSec :: Time -> Integer
  getDir (MkTime a b c d e f g) = a
  getYr (MkTime a b c d e f g) = b
  getMth (MkTime a b c d e f g) = c
  getDay (MkTime a b c d e f g) = d
  getHr (MkTime a b c d e f g) = e
  getMinute (MkTime a b c d e f g) = f
  getSec (MkTime a b c d e f g) = g
  data Quantity = Pty Property | Rge Range deriving (Eq,Show)
  data Property = Nm String VType deriving (Eq,Show)
  data VType = EmT | InT | FlT | TiT deriving (Eq,Show)
  data Range = Emv [String] | Inv Int Int | Flv Float Float | Tiv Time Time deriving (Eq,Show)
  data Compare = Gt Quantity Quantity | Lt Quantity Quantity | El Quantity Quantity |
                 Ne Quantity Quantity | Ge Quantity Quantity | Le Quantity Quantity deriving (Eq,Show)
  data BCondition = And [Literals] | SgB Compare | NoCond deriving (Eq,Show)
  data Literals = Or [Compare] | Sgl Compare deriving (Eq,Show)
  repEgType = (Miseqs "Rep01" (Ndet 10) RepeatB (SgB (Lt (Pty (Nm "this.LoopCounter" InT)) (Rge (Inv 5 10)))))
  repEgType02 = (Miseqs "Treatment" (Ndet 2) RepeatB cond01)
  cond01 = (And [ (Sgl (Lt (Pty (Nm "LoopCounter" InT)) (Rge (Inv 5 10)))),(Sgl (Ne (Pty (Nm "Abnormal Blood Count" EmT)) (Rge (Emv ["High","Low"]))))])
  condSeq = And [condSfst,condSsnd,conStrd,conSfth]
  condSfst = Or [conA,conC]
  condSsnd = Or [conA,conD]
  conStrd = Or [conB,conC]
  conSfth = Or [conB,conD]
  conA,conB,conC,conD :: Compare
  conA = El (Pty (Nm "this.status" EmT)) (Rge (Emv ["Ready"]))
  conB = El (Pty (Nm "Insulin Level" EmT)) (Rge (Emv ["Normal"]))
  conC = El (Pty (Nm "this.status" EmT)) (Rge (Emv ["Completing"]))
  conD = El (Pty (Nm "Insulin Level" EmT)) (Rge (Emv ["Low"]))
  data Exception = Exception Int | NoException deriving (Eq,Show)
  data TaskType = StandardT | InterventionT Intervention deriving (Eq,Show)
  data BpmnType = SequenceB | ScopeB | DependentB | InterventionB | RepeatB deriving (Eq,Show)
  data Type = Itime Time | Stime Time | Ierror Exception | Irule BCondition | Smessage (Maybe Messageflow) | Imessage (Maybe Messageflow) | Agate | Xgate | Exgate | Ogate | Start | End Int |
              Emessage (Maybe Messageflow) | Eerror Int Exception | Abort Int | Task TaskName TaskType | Bpmn BName BpmnType | Pool PLName | Miseq TaskName Loops TaskType BCondition |
              Miseqs BName Loops BpmnType BCondition | Mipar TaskName Loops TaskType BCondition | Mipars BName Loops BpmnType BCondition | Adhoc BName deriving (Eq,Show)
  data Loops = Fix Int | Ndet Int deriving (Eq,Show)
  data Intervention = Intervention IName IDose Method | NoDetails deriving (Eq,Show)
  type Transition = (Guard,Line)
  type Messageflow = (Message,Channel)
  type TRange = (Time,Time)
  data State = State Type [Transition] [Transition] [(Type,Transition)] TRange [Messageflow] [Messageflow] [Messageflow] [Messageflow] [Messageflow] Int deriving (Eq,Show)
  data BPD = States [State]
  data TChor = Bpmns [PName]
  type TrialLocal = (PName -> BPD)
  type TrialGlobal = (CName -> TChor)
  data StateSet = Atomic [State] | SubProcess State [StateSet] deriving (Eq)
  type BPMN = [(PName,[StateSet])]
  instance Show StateSet where
        show s = showStateSet "" s
                where showState sp s = sp++"("++(show s)++"),\n"
                      showStateS sp s = sp++(showStateSet sp s)++",\n"
                      showStateSet sp s = case s of
                                        (Atomic [s]) -> "Atomic [("++(show s)++")]\n"
                                        (Atomic ss) -> "Atomic [("++((show.head) ss)++"),\n"++(take (length intermediate - 2) intermediate)++"]\n"
                                        (SubProcess x ss) -> "SubProcess ("++show x++")\n"++sp++"           ["++(show (head ss))++(take (length intermediate2 - 2) intermediate2)++"]\n"
                                        where intermediate = (concatMap (showState (sp++"                  ")) ((tail.getAT) s))
                                              intermediate2 = (concatMap (showStateS (sp++"             ")) ((tail.snd.getSP) s))
  getSP :: StateSet -> (State,[StateSet])
  getSP (SubProcess s ss) = (s,ss)
  getSP s = (error.show) s
  getAT :: StateSet -> [State]
  getAT (Atomic ss) = ss
  getAT _ = []
  getAtomic :: [StateSet] -> [State]
  getAtomic [] = []
  getAtomic ((Atomic s):ss) = s
  getAtomic (s:ss) = getAtomic ss
  isAtomic :: StateSet -> Bool
  isAtomic (Atomic s) = True
  isAtomic _ = False
  fst3 :: (a,b,c) -> a
  fst3 (a,b,c) = a
  snd3 :: (a,b,c) -> b
  snd3 (a,b,c) = b
  trd3 :: (a,b,c) -> c
  trd3 (a,b,c) = c
  fst4 :: (a,b,c,d) -> a
  fst4 (a,b,c,d) = a
  fst5 :: (a,b,c,d,e) -> a
  fst5 (a,b,c,d,e) = a
  fst6 :: (a,b,c,d,e,f) -> a
  fst6 (a,b,c,d,e,f) = a
  snd4 :: (a,b,c,d) -> b
  snd4 (a,b,c,d) = b
  trd4 :: (a,b,c,d) -> c
  trd4 (a,b,c,d) = c
  fth4 :: (a,b,c,d) -> d
  fth4 (a,b,c,d) = d
  getLoops :: Loops -> Int
  getLoops (Fix i) = i
  getLoops (Ndet i) = i
  isFix :: Loops -> Bool
  isFix (Fix i) = True
  isFix (Ndet i) = False
  isAbort, isAdhoc, isEnd, isEerror, isEmessage, isTask, isMiseq, isMiseqs, isMipar, isMipars, isBpmn, isItime, isStime  :: Type -> Bool
  isAdhoc (Adhoc _) = True
  isAdhoc _ = False
  invAdhoc :: Type -> BName
  invAdhoc (Adhoc x) = x
  isEnd (End _) = True
  isEnd _ = False
  isAbort (Abort _) = True
  isAbort _ = False
  isEerror (Eerror _ _) = True
  isEerror _ = False
  isEmessage (Emessage _) = True
  isEmessage _ = False
  invEnd,invAbort :: Type -> Int
  invEnd (End x) = x
  invAbort (Abort x) = x
  invEerror :: Type -> (Int,Exception)
  invEerror (Eerror x y) = (x,y)
  invEmessage :: Type -> Maybe Messageflow
  invEmessage (Emessage x) = x
  invException :: Exception -> Int
  invException (Exception i) = i
  isTask (Task _ _) = True
  isTask _ = False
  invTask :: Type -> (TaskName,TaskType)
  invTask (Task x y) = (x,y)
  isMiseq (Miseq _ _ _ _) = True
  isMiseq _ = False
  invMiseq :: Type -> (TaskName,Loops,TaskType,BCondition)
  invMiseq (Miseq w x y z) = (w,x,y,z)
  isMiseqs (Miseqs _ _ _ _) = True
  isMiseqs _ = False
  invMiseqs :: Type -> (BName,Loops,BpmnType,BCondition)
  invMiseqs (Miseqs w x y z) = (w,x,y,z)
  isMipar (Mipar _ _ _ _) = True
  isMipar _ = False
  invMipar :: Type -> (TaskName,Loops,TaskType,BCondition)
  invMipar (Mipar w x y z) = (w,x,y,z)
  isMipars (Mipars _ _ _ _) = True
  isMipars _ = False
  invMipars :: Type -> (BName,Loops,BpmnType,BCondition)
  invMipars (Mipars w x y z) = (w,x,y,z)
  isBpmn (Bpmn _ _) = True
  isBpmn _ = False
  invBpmn :: Type -> (BName,BpmnType)
  invBpmn (Bpmn x y) = (x,y)
  invBpmn k = error (show k)
  isItime (Itime _) = True
  isItime _ = False
  invItime :: Type -> Time
  invItime (Itime x) = x
  isStime (Stime _) = True
  isStime _ = False
  invStime :: Type -> Time
  invStime (Stime x) = x
  isSmessage (Smessage _) = True
  isSmessage _ = False
  invSmessage :: Type -> Maybe Messageflow
  invSmessage (Smessage x) = x
  isImessage (Imessage _) = True
  isImessage _ = False
  invImessage :: Type -> Maybe Messageflow
  invImessage (Imessage x) = x
  isIerror (Ierror _) = True
  isIerror _ = False
  invIerror :: Type -> Exception
  invIerror (Ierror x) = x
  getType :: State -> Type
  getType (State a b c d e f g h i j k) = a
  getIn, getOut :: State -> [Transition]
  getIn (State a b c d e f g h i j k) = b
  getOut (State a b c d e f g h i j k) = c
  getError :: State -> [(Type,Transition)]
  getError (State a b c d e f g h i j k) = d
  getRange :: State -> TRange
  getRange (State a b c d e f g h i j k) = e
  getRec :: State -> [Messageflow]
  getRec (State a b c d e f g h i j k) = f
  getSd :: State -> [Messageflow]
  getSd (State a b c d e f g h i j k) = g
  getRep :: State -> [Messageflow]
  getRep (State a b c d e f g h i j k) = h
  getAcc :: State -> [Messageflow]
  getAcc (State a b c d e f g h i j k) = i
  getBk :: State -> [Messageflow]
  getBk (State a b c d e f g h i j k) = j
  getLm :: State -> Int
  getLm (State a b c d e f g h i j k) = k
  invStates :: BPD -> [State]
  invStates(States xs) = xs
  invBpmns :: TChor -> [PName]
  invBpmns(Bpmns xs) = xs
  isIntervention :: TaskType -> Bool
  isIntervention StandardT = False
  isIntervention _ = True
  isInterventionS :: BpmnType -> Bool
  isInterventionS InterventionB = True
  isInterventionS _ = False
  isTasks :: State -> Bool
  isTasks s = let t = getType s
              in (or [isTask t, isMiseq t, isMipar t])
  isSubs :: State -> Bool
  isSubs = issubs.getType
  issubs :: BPMN.Type -> Bool
  issubs t = (or [isBpmn t, isMiseqs t, isMipars t])
  level :: [StateSet] -> [State]
  level [] = []
  level ((Atomic s):ss) = s++(level ss)
  level ((SubProcess s ss):sss) = [s]++(level sss)
  allstates :: [StateSet] -> [State]
  allstates [] = []
  allstates ((Atomic s):ss) = s++(allstates ss)
  allstates ((SubProcess s ss):sss) = [s]++(allstates ss)++(allstates sss)
  noredun :: [StateSet] -> [StateSet]
  noredun ss = [Atomic [ s | s <- getAtomic ss, (not (isredun s ss)) ]]
               ++[ SubProcess ((fst.getSP) s) ((noredun.snd.getSP) s) | s <- ss, (not.isAtomic) s, (not (isredun ((fst.getSP) s) ss)) ]
  isredun :: State -> [StateSet] -> Bool
  isredun s ss = and [ null (intersect (getIn m) (getOut s)) | m <- level ss ] && and [ null (intersect (getOut m) (getIn s))  | m <- level ss ]
  normal :: [StateSet] -> [StateSet] -> [StateSet]
  normal _ [] = []
  normal bg ((SubProcess s ss):sss) = [SubProcess s (normal ss ss)]++(normal bg sss)
  normal bg ((Atomic s):ss) = let (ns:nss) = norm s ss in [ns]++(normal bg nss)
  norm :: [State] -> [StateSet] -> [StateSet]
  norm ss sss = if null bads then [Atomic ss]++sss else
                [Atomic (foldl b1 nobads bads)] ++ (foldl b2 sss bads)
        where bads = [ s | s <- ss, ((getType s == Agate) || (getType s == Xgate)) && (length.getIn) s == 1 && (length.getOut) s == 1 ]
              nobads = [ s | s <- ss, notElem s bads ]
              inc x = [ i | s <- nobads, (not.null) (intersect (getOut s) (getIn x)), i <- (intersect (getOut s) (getIn x))]
              b1 ns x = [ if (not.null) (intersect (getIn s) (getOut x)) then updateStateTrans s ((intersect (getIn s) (getOut x)),(inc x)) ([],[]) else s | s <- ns ]
              incs x = [ i | s <- sss, (not.null) (intersect ((getOut.fst.getSP) s) (getIn x)), i <- (intersect ((getOut.fst.getSP) s) (getIn x))]
              b2 ns x = [ if (not.null) (intersect ((getIn.fst.getSP) s) (getOut x)) then updateStateSetTrans s ((intersect ((getIn.fst.getSP) s) (getOut x)),(incs x)) ([],[]) else s | s <- ns ]
  updateStateSetTrans :: StateSet -> ([Transition],[Transition]) -> ([Transition],[Transition]) -> StateSet
  updateStateSetTrans (SubProcess s ss) (t1,t2) (t3,t4) =  (SubProcess (updateStateTrans s (t1,t2) (t3,t4)) ss)
  updateStateTrans :: State -> ([Transition],[Transition]) -> ([Transition],[Transition]) -> State
  updateStateTrans (State t it ot e tr im om rm am bm re) (t1,t2) (t3,t4) = (State t nit not e tr im om rm am bm re)
        where nit = ((filter (flip notElem t1) it)++t2)
              not = ((filter (flip notElem t3) ot)++t4)
  changeStateSetTrans :: StateSet -> [Transition] -> [Transition] -> StateSet
  changeStateSetTrans (SubProcess s ss) t1 t2 =  (SubProcess (changeStateTrans s t1 t2) ss)
  changeStateTrans :: State -> [Transition] -> [Transition] -> State
  changeStateTrans (State t it ot e tr im om rm am bm re) [] not = (State t it not e tr im om rm am bm re)
  changeStateTrans (State t it ot e tr im om rm am bm re) nit [] = (State t nit ot e tr im om rm am bm re)
  changeStateTrans (State t it ot e tr im om rm am bm re) nit not = (State t nit not e tr im om rm am bm re)
