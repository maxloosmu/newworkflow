Adopted from Lowe's original translation function

>module Formula(
>  Formula(..), Formulas,
>  add,      -- :: Ord a => a -> [a] -> [a]
>  saturate  -- :: Formulas -> [Formulas]
>  )

>where

>type Event = String

Extend Lowe's LTL grammar with bounded eventuality 

>data Formula =
>  Deadlocked | Live | Truef | Falsef |
>  Does Event | Doesnot Event | Available Event | Next Formula | 
>  And Formula Formula | Or Formula Formula | Implies Formula Formula | 
>  Release Formula Formula | BoundEventually Formula Int | BoundUntil Formula (Formula,Int) -- | Previously Formula
>  deriving (Eq, Ord, Show)

>type Formulas = [Formula] -- list of formulas, in order

Add x to list, assuming list is sorted

>add :: Ord a => a -> [a] -> [a]
>add x [] = [x]
>add x (y:ys) = if x<y then x:y:ys else if x==y then y:ys else y : add x ys

Produce saturation of list of formulas

>saturate1 :: Formulas -> [Formulas]
>saturate1 [] = [[]]
>saturate1 (And phi psi : fs) = saturate1 (phi : psi : fs)
>saturate1 (Or phi psi : fs) = saturate1 (phi : fs) ++ saturate1 (psi : fs)
>saturate1 (Release phi psi : fs) =
>  saturate1 (phi : psi : fs) ++ saturate1 (psi : Next(Release phi psi) : fs)
>saturate1 (phi : fs) = map (add phi) (saturate1 fs)

>saturate :: Formulas -> [Formulas]
>saturate fs = 
>  let fss0 = saturate1 fs
>      fss1 = filter (not . containsFalse) fss0
>      fss2 = foldr add [] fss1
>  in remDominators fss2 [] -- filter (not . dominates fss2) fss2

Does list of formulas contain obvious contradiction?

>containsFalse :: Formulas -> Bool
>containsFalse fs = 
>  let deadlocked = Deadlocked `elem` fs
>      doeses = [a | Does a <- fs]
>  in Falsef `elem` fs || 
>     deadlocked && Live `elem` fs ||
>     length doeses > 1 || 
>     [a | a <- doeses, Doesnot a `elem` fs] /= [] ||
>     deadlocked && doeses /= [] ||
>     deadlocked && [a | Available a <- fs] /= []

Remove formulas that dominate something else in the list.  Note that this must
be done iteratively, rather than using filter: the latter approach would
reduce [[Does a, Available a], [Does a]] to [].

>remDominators :: [Formulas] -> [Formulas] -> [Formulas]
>remDominators [] fs' = fs'
>remDominators (f:fs) fs' = 
>  if dominates (fs++fs') f then remDominators fs fs' 
>  else remDominators fs (f:fs')

Does fs dominate something in fss?

>dominates :: [Formulas] -> Formulas -> Bool
>dominates fss fs = any (superset fs) fss

Is fs2 a proper logical subset of fs1?

>superset :: Formulas -> Formulas -> Bool
>superset fs1 fs2 = fs1/=fs2 && all (implies fs1) fs2

If f implied by fs?

>implies :: Formulas -> Formula -> Bool
>implies fs f = any (`implies1` f) fs -- f `elem` fs

Does f1 imply f2?

>implies1 :: Formula -> Formula -> Bool
>implies1 f1 f2 | f1==f2 || f2==Truef = True
>implies1 (Does _) Live = True
>implies1 Deadlocked (Doesnot _) = True
>implies1 (Does a) (Available a') | a==a' = True
>implies1 _ _ = False
