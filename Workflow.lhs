> module Workflow where
> import List (nub)

 type Work = String

> type Property = String
> data ActivityId = BEGIN | START | STOP | NORMAL_STOP | ABNORMAL_STOP | END | Id String deriving (Eq,Show)
> data Duration = UNBOUNDED | Dur String deriving (Eq,Show)

> specialId :: [ActivityId]
> specialId = [BEGIN,START,STOP,NORMAL_STOP,ABNORMAL_STOP,END]

Free Types

> data TrialEvents = Registration | OnStudy | OffStudy deriving (Eq,Show)

> data Repeat = Rep Int | Any deriving (Eq,Show)
> type RepeatExp = (Duration,Duration,Repeat,Repeat,Condition)

\begin{syntax}
	Bool :: = tt | ff
\end{syntax}

Skip Condition

> data Condition = None | Ands [Ors] deriving (Eq,Show)
> data Ors = Ors [SCondition] deriving (Eq,Show)
> type SCondition = (Range,Property) 
> data Range = Bound RangeBound RangeBound | Emu [String] deriving (Eq,Show)  
> data RangeBound = NoBound | Abdate Duration | Abdec Float | Abint Int | Rldate Property Duration | Rldec Property Float | Rlint Property Int deriving (Eq,Show)    

> conBW = Ands [Ors [(Emu ["Low"],"Insulin Levels")]]
> conCW = Ands [Ors [(Emu ["High"],"Insulin Levels")]]

\begin{schema}{Activity}
	id : ActivityId \\
	mindl : Duration \\
	maxdl : Duration \\
	cond : Condition \\
	force : Condition
\end{schema}

> data ActType = Manual | Automatic deriving (Eq,Show)
> type Activity = (ActivityId,Duration,Duration,Repeat,Condition,ActType)

 instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a,b,c,d,e,f) where 
    show (a,b,c,d,e,f) = "("++(show a)++","++(show b)++","++(show c)++","++(show d)++","++(show e)++","++(show f)++")"

Workflow Schema Types

Prequisite Activity 

> data PreAct = All [PreAct] | OneOf [PreAct] | Pa ActivityId deriving (Eq,Show)

Dependent Activity (Structured Workflow Model with No Loops)

 data StructuredModel a = Choice [StructuredModel a] | Par [StructuredModel a] | Sequential [StructuredModel a] | Item a | NoItem deriving (Eq,Show)
 type DptAct2 = StructuredModel Activity

> data DptAct = ChoiceD [DptAct] | ParD [DptAct] | SequentialD [DptAct] | Da Activity | NoDepend deriving (Eq,Show)

Works

> data Works = ChoiceW [Works] | ParW [Works] | SequentialW [Works] | Wk Work | NoWork deriving (Eq,Show)
> data WorkBlock = ChoiceWB [WorkBlock] | ParWB [WorkBlock] | SequentialWB [WorkBlock] | WorkUnit WorkSUnit | NoWorkB deriving (Eq,Show)
> data Work = WorkMult WorkMBlock | WorkSingle WorkMUnit deriving (Eq,Show)
> type WorkMBlock = (WorkId,WorkBlock,Duration,Repeat)
> type WorkMUnit = (WorkId,Treatment,Duration,Duration,Repeat)
> type WorkSUnit = (WorkId,Treatment,Duration,Duration)
> type WorkId = String
> data Treatment = Treatment Name Quantity Method | NoTreatment deriving (Eq,Show)

> type Quantity = String
> type Name = String
> type Method = String

<cg:preciseDescription> 4 x EC followed by 4 x paclitaxel (T) Epirubicin 90 mg/m2, q 21
days Slow push/ fast drip, day 1 only, Cyclophosphamide 600 mg/m2, q 21 days Slow
push, day 1 only Paclitaxel 175 mg/m2, q 14 days 3 hr infusion, day 1 only
</cg:preciseDescription>

> data DptEvent = OneOfD [DptEvent] | AllD [DptEvent] | De ActivityId deriving (Eq,Show)

\begin{schema}{EventSequencing}
	id : ActivityId \\
	prereq : \finset_1 ActivityId \\
	cond : Condition \\
	force : Condition \\
	dependent : \finset_1 Activity \\
	repeat : \finset RepeatExp \\
	intervent : \finset Intervention
\where 
	id \notin prereq \\
	(\{~id~\} \cup prereq) \cap \{~d : dependent \spot d.id~\} = \emptyset\\
\end{schema}


> data EventSequencing = Event ActivityId PreAct DptEvent Condition Condition DptAct [RepeatExp] Works deriving (Eq,Show)

> isEventSq :: EventSequencing -> Bool
> isEventSq (Event a b c d e f g h) = (g == nub g)

> mkEventSq :: EventSequencing -> EventSequencing
> mkEventSq (Event a b c d e f g h) = Event a b c d e f (nub g) h

> type Workflow = [EventSequencing]

> equalW :: Workflow -> Workflow -> Bool
> equalW w1 = and.(map ((flip elem) w1)) 

 data Workflow = MkWorkflow [EventSequencing]
 instance Eq Workflow where
	a == b = a == b

> getNme :: EventSequencing -> ActivityId
> getNme (Event a b c d e f g h) = a

> getPr :: EventSequencing -> PreAct
> getPr (Event a b c d e f g h) = b

> allPid :: [PreAct] -> [ActivityId]
> allPid [] = []
> allPid ((Pa a):ps) = [a]++(allPid ps)
> allPid (p:ps) = ((allPid.getPs) p)++(allPid ps)

> getPs :: PreAct -> [PreAct]
> getPs (OneOf ps) = ps
> getPs (All ps) = ps
> getPs p = [p]

> getPId :: PreAct -> ActivityId
> getPId (Pa id) = id

> getDe :: EventSequencing -> DptEvent
> getDe (Event a b c d e f g h) = c

> getCon :: EventSequencing -> Condition
> getCon (Event a b c d e f g h) = d

> getExit :: EventSequencing -> Condition
> getExit (Event a b c d e f g h) = e

> getDt :: EventSequencing -> DptAct
> getDt (Event a b c d e f g h) = f

> getRe :: EventSequencing -> [RepeatExp]
> getRe (Event a b c d e f g h) = g

> getIns :: EventSequencing -> Works
> getIns (Event a b c d e f g h) = h

> normW :: Works -> Works
> normW (ChoiceW w) = if length w == 1 then (normW.head) w else ChoiceW (map normW w)
> normW (ParW w) = if length w == 1 then (normW.head) w else ParW (map normW w)
> normW (SequentialW w) = if length w == 1 then (normW.head) w else SequentialW (map normW w)
> normW (Wk (WorkMult (i,b,d,r))) = (Wk (WorkMult (i,(normWB b),d,r)))
> normW x = x

> normWB :: WorkBlock -> WorkBlock
> normWB (ChoiceWB wb) = if length wb == 1 then (normWB.head) wb else ChoiceWB (map normWB wb)
> normWB (ParWB wb) = if length wb == 1 then (normWB.head) wb else ParWB (map normWB wb)
> normWB (SequentialWB wb) = if length wb == 1 then (normWB.head) wb else SequentialWB (map normWB wb)
> normWB x = x

> normPre :: PreAct -> PreAct
> normPre (OneOf pre) = if length pre == 1 then (normPre.head) pre else OneOf (map normPre pre)
> normPre (All pre) = if length pre == 1 then (normPre.head) pre else All (map normPre pre)
> normPre x = x

> normDve :: DptEvent -> DptEvent
> normDve (OneOfD dve) = if length dve == 1 then (normDve.head) dve else OneOfD (map normDve dve)
> normDve (AllD dve) = if length dve == 1 then (normDve.head) dve else AllD (map normDve dve)
> normDve x = x
