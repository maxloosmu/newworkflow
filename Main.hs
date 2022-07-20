module Main where

import System (getArgs)
import Directory (getCurrentDirectory)
import Data.List (findIndex)
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Pretty (document)
import XMLToBPMN (xTob)
import BPMNToXML (bTox)
import BPMNToWorkflow (bTowf,bTow)
import WorkflowToBPMN (wTob,justSProcess)
import XMLToWorkflow (xTow)
import WorkflowToXML (wTox)
import Workflow (equalW)
import BPMNToCSP (proprefine,bToc,refine,bTou,urefine)
import CSP (showCSP,showScript)
import Temporal (makeSpecf,makeSpec)

--import CSPData
--import WorkflowData
--import Test.QuickCheck

main :: IO ()
main = do {	args <- getArgs; 
		dir <- getCurrentDirectory;
		checkArgs dir args 
	  }

makeAbsolute :: FilePath -> FilePath -> FilePath 
makeAbsolute cd fp = foldl1 addslash (mkAbs (paths cd) (paths fp))
	where addslash x y = x++['/']++y

mkAbs :: [String] -> [String] -> [String]
mkAbs cs [] = cs
mkAbs [] fs = fs
mkAbs cs (f:fs) 
	| f == ".." = mkAbs (init cs) fs
	| otherwise = cs ++ (f:fs)

paths :: String -> [String]
paths fp = let isSlash s = (s == '/' ||  s =='\\') 
	   in case findIndex isSlash fp of
		Nothing -> [fp]
		Just i -> [take i fp]++paths (drop (i+1) fp)

mkname :: String -> String -> String
mkname y ext = case findIndex (=='.') y of
		Just i -> (fst (splitAt i y))++"."++ext
		Nothing -> y++"."++ext
		
checkArgs :: FilePath -> [String] -> IO()
checkArgs dir (x:y:z) =
	let ny = makeAbsolute dir y
	    nz = if null z 
	         then if x == "XMLToBPMNToUCSPm" || x == "XMLToBPMNToTCSPm" || x == "btu" || x == "btt" || x == "ptu" || x == "ptt" then  mkname ny "csp" 
		      else mkname ny "xml" 
		 else makeAbsolute dir (head z)
	    nzz = if length z == 1 then mkname nz "csp" 
	          else if length z > 1 then makeAbsolute dir (z!!1) else ""  
	in case x of
		"XMLToBPMN" -> xtBtrans ny ""
		"XMLToBPMNToFile" -> xtBtrans ny nz
		"XMLToBPMNToWorkflow" -> xtBtWtrans ny ""
		"XMLToBPMNToWorkflowToFile" -> xtBtWtrans ny nz
		"XMLToBPMNToWorkflowToXML" -> xtBtWtXtrans ny nz
		"btw" -> xtBtWtXtrans ny nz
		"XMLToWorkflowToBPMN" -> xtWtBtXtrans ny ""
		"XMLToWorkflowToBPMNToXML" -> xtWtBtXtrans ny nz
		"wtb" -> xtWtBtXtrans ny nz
		"XMLToWorkflow" -> xtWtrans ny ""
		"XMLToBPMNToUCSPm" -> xtBtTtransu ny (if null nzz then [nz] else [nz,nzz])
		"btu" -> xtBtTtransu ny (if null nzz then [nz] else [nz,nzz])
		"XMLToBPMNToTCSPm" -> xtBtTtranst ny (if null nzz then [nz] else [nz,nzz])
		"btt" -> xtBtTtranst ny (if null nzz then [nz] else [nz,nzz])
		"ptu" -> if null z then pTou y [] else pTou y [nz,nzz]
		"ptt" -> if null z then pTot y [] else pTot y [nz,nzz]
		_ -> putStr "transform OPTION [PL] SOURCE1 [SOURCE2] [DEST]\n\nbtw\n\tFrom BPMN to OWorkflow\n\nwtb\n\tFrom OWorkflow to BPMN\n\nbtu\n\tFrom BPMN to CSPM (purely untimed) (SOURCE1 [F= SOURCE2)\n\nbtt\n\tFrom BPMN to CSPM (SOURCE1 [F= SOURCE2)\n\nptu\n\tFrom PL to CSPM (purely untimed) (SOURCE1 |= PL)\n\nptt\n\tFrom PL to CSPM (SOURCE1 |= PL)\n\n"

--		_ -> putStr "transform OPTION SOURCE [SOURCE2] [DEST]\n\nbtw\n\tFrom XML to BPMN to OWorkflow to XML\n\nwtb\n\tFrom XML to OWorkflow to BPMN to XML\n\nbtc\n\tFrom XML to BPMN abstract syntax to Time Model in CSPm (SOURCE1 [F= SOURCE2)\n\n"
		
--		_ -> putStr "transform OPTION SOURCE1 [SOURCE2] DEST\n\nXMLToBPMNToWorkflowToXML\n\tFrom XML to BPMN to OWorkflow to XML\n\nXMLToWorkflowToBPMNToXML\n\tFrom XML to OWorkflow to BPMN to XML\n\nXMLToBPMNToCSPm\n\tFrom XML to BPMN abstract syntax to Time Model in CSPm (SOURCE1 [F= SOURCE2)\n\n"

checkArgs d _ = putStr "transform OPTION [PL] SOURCE1 [SOURCE2] [DEST]\n\nbtw\n\tFrom BPMN to OWorkflow\n\nwtb\n\tFrom OWorkflow to BPMN\n\nbtu\n\tFrom BPMN to CSPM (purely untimed) (SOURCE1 [F= SOURCE2)\n\nbtt\n\tFrom BPMN to CSPM (SOURCE1 [F= SOURCE2)\n\nptu\n\tFrom PL to CSPM (purely untimed) (SOURCE1 |= PL)\n\nptt\n\tFrom PL to CSPM (SOURCE1 |= PL)\n\n"

--checkArgs d _ = putStr "transform OPTION SOURCE [SOURCE2] [DEST]\n\nbtw\n\tFrom XML to BPMN to OWorkflow to XML\n\nwtb\n\tFrom XML to OWorkflow to BPMN to XML\n\nbtc\n\tFrom XML to BPMN abstract syntax to Time Model in CSPm (SOURCE1 [F= SOURCE2)\n\n"

--checkArgs d _ = putStr "transform OPTION SOURCE [SOURCE2] DEST\n\nXMLToBPMNToWorkflowToXML\n\tFrom XML to BPMN to OWorkflow to XML\n\nXMLToWorkflowToBPMNToXML\n\tFrom XML to OWorkflow to BPMN to XML\n\nXMLToBPMNToCSPm\n\tFrom XML to BPMN abstract syntax to Time Model in CSPm (SOURCE1 [F= SOURCE2)\n\n"
	
--	| otherwise = putStr "transforms [OPTION] SOURCE DEST\n\nXMLToBPMNToWorkflowToXML\n	From XML to BPMN to OWorkflow to XML\n\nXMLToWorkflowToBPMNToXML\n	From XML to OWorkflow to BPMN to XML\n\nXMLToBPMNToTimeModel\n	From XML to BPMN abstract syntax to Time Model\n\n"
-- checkArgs _ = putStr "transforms [OPTION] SOURCE DEST\n\nXMLToBPMNToWorkflowToXML\n	From XML to BPMN to OWorkflow to XML\n\nXMLToWorkflowToBPMNToXML\n	From XML to OWorkflow to BPMN to XML\n\nXMLToBPMNToCSP\n	From XML to BPMN abstract syntax to CSP\n\n"

-- gettestWorkflow = checkArgs ["XMLToBPMNToWorkflowToFile","examples//example_ilog_new_format.ibp","NewDataB.lhs"]
-- getsimpleWorkflow = checkArgs ["XMLToBPMNToWorkflowToFile","examples//example_ilog_new_format_simple.ibp","NewDataB.lhs"]
-- getsimpleWorkflow2 = checkArgs ["XMLToBPMNToWorkflowToFile","examples//example_ilog_new_format_simple02.ibp","NewDataB.lhs"]

xtBtrans :: FilePath -> FilePath -> IO()
xtBtrans i j = do {	xml <- readFile i;
		   	case j of
				"" -> putStr ((show (xTob (xmlParse i xml)))++"\n")  
				str -> writeFile j ("> testBPMN = "++(show (xTob (xmlParse i xml))))  }

xtBtWtrans :: FilePath -> FilePath -> IO()
xtBtWtrans i j = do { xml <- readFile i;
			case j of
		     		"" -> putStr ((ans xml)++"\n")
				str -> writeFile j ("> testWorkflow = "++(ans xml)) }
	where ans x = case ((bTow. xTob) (xmlParse i x)) of
			[] -> "[]"
			a -> (show a)

xtBtWtXtrans :: FilePath -> FilePath -> IO()
xtBtWtXtrans i j = do { xml <- readFile i;
			case j of
		     		"" -> putStr ((ans xml)++"\n")
				str -> writeFile j (ans xml) }
	where ans x = ((show . document . wTox . bTow. xTob) (xmlParse i x))
	
xtWtBtXtrans :: FilePath -> FilePath -> IO()
xtWtBtXtrans i j = do { xml <- readFile i;
			case j of
		     		"" -> putStr (((show . wTob . xTow . (xmlParse i)) xml)++"\n")
				str -> writeFile j ((show . document . bTox . wTob . xTow . (xmlParse i)) xml) }
				
xtWtrans :: FilePath -> FilePath -> IO()
xtWtrans i j = do { xml <- readFile i;
		    case j of
		    	"" -> putStr (((show . xTow . (xmlParse i)) xml)++"\n")
			str -> writeFile j ("> testWorkflow = "++((show . xTow . (xmlParse i)) xml)) }
			
xtBtTtranst :: FilePath -> [FilePath] -> IO()
xtBtTtranst i js = do { xml <- readFile i;
		       if length js == 0 then putStr ((showCSP .  bToc . xTob) (xmlParse i xml))
		       else if length js == 1 then writeFile (head js) ((showCSP .  bToc . xTob) (xmlParse i xml))
		       	    else if length js == 2 
			    	 then do { xml2 <- readFile (js!!0);
				      	  writeFile (js!!1) (showCSP (refine ((bToc . xTob) (xmlParse i xml)) ((bToc . xTob) (xmlParse (js!!0) xml2)))) }
				 else error "invalid argument" }
				 
xtBtTtransu :: FilePath -> [FilePath] -> IO()
xtBtTtransu i js = do { xml <- readFile i;
		       if length js == 0 then putStr ((showCSP .  bTou . xTob) (xmlParse i xml))
		       else if length js == 1 then writeFile (head js) ((showCSP .  bTou . xTob) (xmlParse i xml))
		       	    else if length js == 2 
			    	 then do { xml2 <- readFile (js!!0);
				      	  writeFile (js!!1) (showCSP (urefine ((bTou . xTob) (xmlParse i xml)) ((bTou . xTob) (xmlParse (js!!0) xml2)))) }
				 else error "invalid argument" }
				 
pTou :: String -> [FilePath] -> IO()
pTou s f = do {
		if length f == 0 then (putStr . showScript . makeSpec) s 
		else if length f == 1 then writeFile (f!!0) (makeSpecf s)
	   	     else if length f == 2 
		          then do { xml <- readFile (f!!0);
		     	            writeFile (f!!1) (showCSP (proprefine "u" (makeSpec s) ((bTou . xTob) (xmlParse (f!!0) xml)))) }
		          else error "invalid argument" } 
		     
pTot :: String -> [FilePath] -> IO()
pTot s f = do {
	     if length f == 0 then (putStr . showScript . makeSpec) s
	     else if length f == 1 then writeFile (f!!0) (makeSpecf s)
	     	  else if length f == 2 
		       then do { xml <- readFile (f!!0);
		     	         writeFile (f!!1) (showCSP (proprefine "t" (makeSpec s) ((bToc . xTob) (xmlParse (f!!0) xml)))) }
		       else error "invalid argument" } 
