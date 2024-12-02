import Data.Char (toLower, isSpace)
import Control.Monad (forM_, when)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Process (system, readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import qualified Data.Set as Set
import Data.List (isPrefixOf, isInfixOf, stripPrefix, nub)  -- Added nub
import Data.Maybe (mapMaybe, fromMaybe)  -- Added fromMaybe
import Data.List (nub, findIndex)
import System.IO
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist)



data Proposition

    = P String

    | Obligation Proposition

    | Permission Proposition

    | Prohibition Proposition

    | Not Proposition

    | And Proposition Proposition

    | Or Proposition Proposition

    | Implies Proposition Proposition

    deriving (Show, Eq)



deonticFun :: Proposition -> [Proposition]

deonticFun (Obligation p) = Obligation p : deonticFun p

deonticFun (Permission p) = Permission p : deonticFun p

deonticFun (Prohibition p) = Prohibition p : deonticFun p

deonticFun (And p1 p2) = deonticFun p1 ++ deonticFun p2

deonticFun (Implies p1 p2) = deonticFun p1 ++ deonticFun p2

deonticFun _ = []



counit :: Proposition -> Proposition

counit (Obligation p) = counit p

counit (Permission p) = Or (counit p) (Not (counit p))

counit (Prohibition p) = Not (counit p)

counit (And p1 p2) = And (counit p1) (counit p2)

counit (Implies p1 p2) = Implies (counit p1) (counit p2)

counit p = p



truthAssignment :: Proposition -> Bool

truthAssignment (P _) = True

truthAssignment (Obligation p) = truthAssignment p

truthAssignment (Permission _) = True

truthAssignment (Prohibition p) = not (truthAssignment p)

truthAssignment (Not p) = not (truthAssignment p)

truthAssignment (And p1 p2) = truthAssignment p1 && truthAssignment p2

truthAssignment (Or p1 p2) = truthAssignment p1 || truthAssignment p2

truthAssignment (Implies p1 p2) = not (truthAssignment p1) || (truthAssignment p2)



normativeActions :: Proposition -> [Proposition]

normativeActions = deonticFun



factualActions :: Proposition -> [Proposition]

factualActions p = let deontic = deonticFun p

                       allProps = extractAllProps p

                   in filter (`notElem` deontic) allProps



extractAllProps :: Proposition -> [Proposition]

extractAllProps (And p1 p2) = extractAllProps p1 ++ extractAllProps p2

extractAllProps (Or p1 p2) = extractAllProps p1 ++ extractAllProps p2

extractAllProps (Implies p1 p2) = extractAllProps p1 ++ extractAllProps p2

extractAllProps p = [p]



compliantNormativeActions :: Proposition -> [Proposition]

compliantNormativeActions p = map counit (deonticFun p)



compliantFactualActions :: Proposition -> [Proposition]

compliantFactualActions p = let deontic = deonticFun p

                                allProps = extractAllProps p

                                factual = filter (`notElem` deontic) allProps

                            in filter (\f -> truthAssignment f) factual

                            

compliantActions :: Proposition -> [Proposition]

compliantActions p =    let deontic = map counit (deonticFun p)

                            allProps = extractAllProps p

                            factual = filter (`notElem` (deonticFun p)) allProps

                        in filter (\f -> truthAssignment f) (deontic ++ factual)



data Clause = Clause { condition :: String, consequence :: String, logicalStructure :: Proposition } deriving (Show, Eq)

normalize :: String -> String
normalize = unwords . words . map toLower

parseClausulas :: [String] -> [Clause]
parseClausulas content = 
    let clauseGroups = splitClauses content
    in mapMaybe parseClause clauseGroups

splitClauses :: [String] -> [[String]]
splitClauses = filter (not . null) . split (isPrefixOf "CLÁUSULA")

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile (not . p) xs of
    [] -> []
    (y:ys) -> (y : takeWhile (not . p) ys) : split p (dropWhile (not . p) ys)

parseClause :: [String] -> Maybe Clause
parseClause [] = Nothing
parseClause clauseLines = 
    let clauseText = unwords $ dropWhile (not . isInfixOf "En caso de que") clauseLines
        normalizedText = normalize clauseText
    in case break (isPrefixOf "entonces") (words normalizedText) of
        (condition, _:consequence) -> 
            let cond = unwords condition
                cons = unwords consequence
                logicalStruct = parseLogicalStructure cond cons
            in Just (Clause cond cons logicalStruct)
        _ -> Nothing

parseLogicalStructure :: String -> String -> Proposition
parseLogicalStructure condition consequence =
    let conditionProps = parseCondition condition
        consequenceProp = parseConsequence consequence
    in Implies conditionProps consequenceProp

parseCondition :: String -> Proposition
parseCondition cond =
    let trimmedCond = trim $ dropPrefix "en caso de que " cond
        parts = splitOn " y " trimmedCond
    in case parts of
        [single] -> P (trim single)
        multiple -> foldr1 And (map (P . trim) multiple)


parseConsequence :: String -> Proposition
parseConsequence cons
    | "se tiene derecho de exigir que" `isInfixOf` cons =
        Obligation (P (trim $ drop (length "se tiene derecho de exigir que") cons))
    | "se permite que" `isInfixOf` cons =
        Permission (P (trim $ drop (length "se permite que") cons))
    | "se prohibe que"`isInfixOf` cons =
        Prohibition (P (trim $ drop (length "se prohibe que") cons))
    | otherwise = P (trim cons)

dropPrefix :: String -> String -> String
dropPrefix prefix str
    | prefix `isPrefixOf` str = drop (length prefix) str
    | otherwise = str

trim :: String -> String
trim = unwords . words

splitOn :: String -> String -> [String]
splitOn delimiter = foldr f [""] 
    where f c (x:xs) | isPrefixOf delimiter (c:x) = "":x:xs
                     | otherwise = (c:x):xs

printParsedClauses :: [Clause] -> IO ()
printParsedClauses clausulas = do
    putStrLn $ "Number of clauses found: " ++ show (length clausulas)
    forM_ (zip [1..] clausulas) $ \(i, clause) -> do
        putStrLn $ "Clause " ++ show i ++ ":"
        putStrLn $ "  Condition: " ++ condition clause
        putStrLn $ "  Consequence: " ++ consequence clause
        putStrLn $ "  Logical Structure: " ++ show (logicalStructure clause)

-- Convert proposition to truth matrix form
toMatrix :: Proposition -> ([String], [[Bool]])
toMatrix prop = 
    let atoms = collectAtoms prop
        assignments = generateTruthAssignments (length atoms)
        validAssignments = filter (evaluateAssignment prop atoms) assignments
    in (atoms, validAssignments)

-- Collect all unique atomic propositions
collectAtoms :: Proposition -> [String]
collectAtoms prop = nub $ case prop of
    P s -> [s]
    Obligation p -> collectAtoms p
    Permission p -> collectAtoms p
    Prohibition p -> collectAtoms p
    Not p -> collectAtoms p
    And p1 p2 -> collectAtoms p1 ++ collectAtoms p2
    Or p1 p2 -> collectAtoms p1 ++ collectAtoms p2
    Implies p1 p2 -> collectAtoms p1 ++ collectAtoms p2

-- Generate all possible truth assignments
generateTruthAssignments :: Int -> [[Bool]]
generateTruthAssignments 0 = [[]]
generateTruthAssignments n = 
    let rest = generateTruthAssignments (n-1)
    in map (False:) rest ++ map (True:) rest

-- Evaluate proposition under a given assignment
evaluateAssignment :: Proposition -> [String] -> [Bool] -> Bool
evaluateAssignment prop atoms assignment = 
    let assignmentMap = zip atoms assignment
    in evaluateProp prop assignmentMap

evaluateProp :: Proposition -> [(String, Bool)] -> Bool
evaluateProp (P s) map = fromMaybe False (lookup s map)
evaluateProp (Obligation p) map = evaluateProp p map
evaluateProp (Permission _) map = True  -- Always satisfiable
evaluateProp (Prohibition p) map = not $ evaluateProp p map
evaluateProp (Not p) map = not $ evaluateProp p map
evaluateProp (And p1 p2) map = evaluateProp p1 map && evaluateProp p2 map
evaluateProp (Or p1 p2) map = evaluateProp p1 map || evaluateProp p2 map
evaluateProp (Implies p1 p2) map = not (evaluateProp p1 map) || evaluateProp p2 map


-- Write matrices to files for PyTorch processing
writeMatrixFiles :: [Clause] -> IO ()
writeMatrixFiles clauses = do
    -- First collect and normalize all atoms
    let normalizeAtom :: String -> String
        normalizeAtom s = 
            let s1 = filter (/= ',') . filter (/= '.') $ s  -- Remove punctuation
                s2 = if "y " `isPrefixOf` s1              -- Remove initial "y "
                     then drop 2 s1
                     else s1
            in s2
            
    let allAtoms = nub $ concatMap (map normalizeAtom . collectAtomsFromProp . logicalStructure) clauses
    
    -- Create atom mapping
    let atomMap = Map.fromList $ zip allAtoms [0..]
    
    -- Debug: Print atom mapping
    putStrLn "\nAtom Mapping:"
    forM_ (Map.toList atomMap) $ \(atom, idx) -> do
        putStrLn $ show idx ++ ": " ++ atom
        
    -- Write atom mapping file
    writeFile "atom_mapping.txt" $ unlines 
        [show idx ++ " " ++ atom | (atom, idx) <- Map.toList atomMap]
    
    -- Process each clause
    forM_ (zip [1..] clauses) $ \(i, clause) -> do
        let origAtoms = collectAtomsFromProp (logicalStructure clause)
        let normalizedAtoms = map normalizeAtom origAtoms
        let atomIndices = map (\atom -> Map.findWithDefault (-1) atom atomMap) normalizedAtoms
        let (_, matrix) = toMatrix (logicalStructure clause)
        let prefix = "clause_" ++ show i
        
        -- Debug: Print clause atoms
        putStrLn $ "\nClause " ++ show i ++ " atoms:"
        forM_ (zip origAtoms atomIndices) $ \(atom, idx) -> do
            putStrLn $ "Original: " ++ atom ++ " -> Normalized: " ++ normalizeAtom atom ++ " -> ID: " ++ show idx
        
        -- Write matrix (1s and 0s)
        writeFile (prefix ++ "_matrix.txt") $ 
            unlines [unwords [if b then "1" else "0" | b <- row] | row <- matrix]
        
        -- Write normalized atom indices
        writeFile (prefix ++ "_atoms.txt") $ 
            unlines $ map show atomIndices
        
        putStrLn $ "\nGenerated matrix files for clause " ++ show i ++ ":"
        putStrLn $ "  Matrix size: " ++ show (length matrix) ++ " x " ++ show (length atomIndices)
        putStrLn $ "  Files: " ++ prefix ++ "_matrix.txt, " ++ prefix ++ "_atoms.txt"
  where
    collectAtomsFromProp :: Proposition -> [String]
    collectAtomsFromProp (P s) = [s]
    collectAtomsFromProp (Obligation p) = collectAtomsFromProp p
    collectAtomsFromProp (Permission p) = collectAtomsFromProp p
    collectAtomsFromProp (Prohibition p) = collectAtomsFromProp p
    collectAtomsFromProp (Not p) = collectAtomsFromProp p
    collectAtomsFromProp (And p1 p2) = collectAtomsFromProp p1 ++ collectAtomsFromProp p2
    collectAtomsFromProp (Or p1 p2) = collectAtomsFromProp p1 ++ collectAtomsFromProp p2
    collectAtomsFromProp (Implies p1 p2) = collectAtomsFromProp p1 ++ collectAtomsFromProp p2

type AtomMap = Map.Map String Int


normalizeAtom :: String -> String
normalizeAtom s = 
    let s1 = filter (/= ',') . filter (/= '.') $ s  -- Remove punctuation
        s2 = if "y " `isPrefixOf` s1              -- Remove initial "y "
             then drop 2 s1
             else s1
    in s2

collectAtomsFromProp :: Proposition -> [String]
collectAtomsFromProp (P s) = [s]
collectAtomsFromProp (Obligation p) = collectAtomsFromProp p
collectAtomsFromProp (Permission p) = collectAtomsFromProp p
collectAtomsFromProp (Prohibition p) = collectAtomsFromProp p
collectAtomsFromProp (Not p) = collectAtomsFromProp p
collectAtomsFromProp (And p1 p2) = collectAtomsFromProp p1 ++ collectAtomsFromProp p2
collectAtomsFromProp (Or p1 p2) = collectAtomsFromProp p1 ++ collectAtomsFromProp p2
collectAtomsFromProp (Implies p1 p2) = collectAtomsFromProp p1 ++ collectAtomsFromProp p2


promptForActions :: [Clause] -> AtomMap -> IO ()
promptForActions clauses atomMap = do
    putStrLn "\nThe contract is satisfiable. Would you like to explore:"
    putStrLn "1. Normative actions"
    putStrLn "2. Factual actions"
    putStrLn "3. Compliant normative actions"
    putStrLn "4. Compliant factual actions"
    putStrLn "5. All compliant actions"
    putStrLn "6. Probability analysis"
    putStrLn "Enter choice (1-6) or any other key to exit:"
        
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "\nNormative Actions:"
            forM_ clauses $ \clause -> do
                putStrLn $ "\nFor clause: " ++ condition clause
                let actions = normativeActions (logicalStructure clause)
                forM_ actions $ \action -> do
                    let atomId = case action of
                            P s -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            Obligation (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            Permission (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            Prohibition (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            _ -> -1
                    putStrLn $ "ID " ++ show atomId ++ ": " ++ show action
                
        "2" -> do
            putStrLn "\nFactual Actions:"
            forM_ clauses $ \clause -> do
                putStrLn $ "\nFor clause: " ++ condition clause
                let actions = factualActions (logicalStructure clause)
                forM_ actions $ \action -> do
                    let atomId = case action of
                            P s -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            _ -> -1
                    putStrLn $ "ID " ++ show atomId ++ ": " ++ show action
                
        "3" -> do
            putStrLn "\nCompliant Normative Actions:"
            forM_ clauses $ \clause -> do
                putStrLn $ "\nFor clause: " ++ condition clause
                let actions = compliantNormativeActions (logicalStructure clause)
                forM_ actions $ \action -> do
                    let atomId = case action of
                            P s -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            Obligation (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            Permission (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            Prohibition (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            _ -> -1
                    putStrLn $ "ID " ++ show atomId ++ ": " ++ show action
                
        "4" -> do
            putStrLn "\nCompliant Factual Actions:"
            forM_ clauses $ \clause -> do
                putStrLn $ "\nFor clause: " ++ condition clause
                let actions = compliantFactualActions (logicalStructure clause)
                forM_ actions $ \action -> do
                    let atomId = case action of
                            P s -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            _ -> -1
                    putStrLn $ "ID " ++ show atomId ++ ": " ++ show action
                
        "5" -> do
            putStrLn "\nAll Compliant Actions:"
            forM_ clauses $ \clause -> do
                putStrLn $ "\nFor clause: " ++ condition clause
                let actions = compliantActions (logicalStructure clause)
                forM_ actions $ \action -> do
                    let atomId = case action of
                            P s -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            Obligation (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            Permission (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            Prohibition (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
                            _ -> -1
                    putStrLn $ "ID " ++ show atomId ++ ": " ++ show action
                putStrLn ""
                
        "6" -> do
            -- First check if analysis file exists
            analysisExists <- doesFileExist "analysis_results.txt"
            if not analysisExists
                then do
                    putStrLn "Running statistical analysis..."
                    (exitCode, stdout, stderr) <- readProcessWithExitCode "python" ["advanced_probability_analyzer.py"] ""
                    case exitCode of
                        ExitSuccess -> showAnalysisResults
                        ExitFailure code -> do
                            putStrLn $ "Error running analysis (exit code " ++ show code ++ ")"
                            putStrLn stderr
                else showAnalysisResults
            
            -- Ask for specific analysis
            putStrLn "\nWould you like to:"
            putStrLn "1. See overall statistics"
            putStrLn "2. Analyze specific variable"
            putStrLn "3. View dependencies"
            putStrLn "4. See mandatory conditions"
            putStrLn "Enter choice (1-4):"
            
            analysisChoice <- getLine
            case analysisChoice of
                "1" -> showSection "Overall Statistics"
                "2" -> do
                    putStrLn "\nAvailable variables:"
                    forM_ (Map.toList atomMap) $ \(atom, idx) -> do
                        putStrLn $ show idx ++ ": " ++ atom
                    putStrLn "Enter variable ID:"
                    varId <- getLine
                    case reads varId of
                        [(id, "")] -> showVariableAnalysis id
                        _ -> putStrLn "Invalid variable ID"
                "3" -> showSection "Dependencies"
                "4" -> showSection "Mandatory Conditions"
                _ -> return ()
        
        _ -> return ()
  where
    showAnalysisResults :: IO ()
    showAnalysisResults = do
        exists <- doesFileExist "analysis_results.txt"
        if exists
            then do
                content <- readFile "analysis_results.txt"
                putStrLn "\nAnalysis Results:"
                putStrLn content
            else putStrLn "Analysis results not found"
    
    showSection :: String -> IO ()
    showSection section = do
        exists <- doesFileExist "analysis_results.txt"
        if exists
            then do
                content <- readFile "analysis_results.txt"
                let sections = lines content
                    sectionStart = findIndex (isPrefixOf section) sections
                case sectionStart of
                    Just idx -> do
                        let relevantLines = drop idx sections
                            sectionContent = takeWhile (not . null) relevantLines
                        putStrLn $ unlines sectionContent
                    Nothing -> putStrLn $ "Section '" ++ section ++ "' not found"
            else putStrLn "Analysis results not found"
    
    showVariableAnalysis :: Int -> IO ()
    showVariableAnalysis varId = do
        case Map.lookup varId (Map.fromList [(v,k) | (k,v) <- Map.toList atomMap]) of
            Just varName -> do
                exists <- doesFileExist "analysis_results.txt"
                if exists
                    then do
                        content <- readFile "analysis_results.txt"
                        let sections = lines content
                            relevant = filter (isInfixOf varName) sections
                        putStrLn $ "\nAnalysis for " ++ varName ++ ":"
                        putStrLn $ unlines relevant
                    else putStrLn "Analysis results not found"
            Nothing -> putStrLn "Variable not found"

-- Helper function to extract atom ID from any proposition type
getAtomId :: Proposition -> AtomMap -> Int
getAtomId prop atomMap = case prop of
    P s -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
    Obligation (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
    Permission (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
    Prohibition (P s) -> Map.findWithDefault (-1) (normalizeAtom s) atomMap
    _ -> -1

main :: IO ()
main = do
    content <- readFile "generated_contract.txt"
    let linesContent = lines content
    putStrLn "\nParsing clauses..."
    let clausulas = parseClausulas linesContent
    printParsedClauses clausulas
    
    -- Create atom mapping and write matrices
    let allAtoms = nub $ concatMap (map normalizeAtom . collectAtomsFromProp . logicalStructure) clausulas
    let atomMap = Map.fromList $ zip allAtoms [0..]
    
    writeMatrixFiles clausulas
    
    -- Call Python script for Sesma analysis
    putStrLn "\nRunning Sesma product analysis..."
    let pythonScript = "sesma_runner.py"
    let pythonPath = "python3"
    
    pythonExists <- doesFileExist pythonScript
    if not pythonExists
        then putStrLn $ "Error: Cannot find " ++ pythonScript ++ " in current directory"
        else do
            putStrLn $ "Found " ++ pythonScript
            (exitCode, stdout, stderr) <- readProcessWithExitCode pythonPath [pythonScript] ""
            putStrLn "Python Output:"
            putStrLn stdout
            case exitCode of
                ExitSuccess -> do
                    fileExists <- doesFileExist "final_matrix.txt"
                    if fileExists
                        then do
                            -- Directly run probability analysis without prompting
                            putStrLn "\nRunning probability analysis..."
                            (probExitCode, probStdout, probStderr) <- readProcessWithExitCode "python" ["advanced_probability_analyzer.py"] ""
                            case probExitCode of
                                ExitSuccess -> do
                                    analysisExists <- doesFileExist "analysis_results.txt"
                                    when analysisExists $ do
                                        analysisContent <- readFile "analysis_results.txt"
                                        putStrLn "\nAnalysis Results:"
                                        putStrLn analysisContent
                                ExitFailure code -> do
                                    putStrLn $ "Error running probability analysis (exit code " ++ show code ++ "):"
                                    putStrLn probStderr
                        else putStrLn "No satisfying assignments found."
                ExitFailure code -> do
                    putStrLn $ "Error running Python script (exit code " ++ show code ++ ")"
                    putStrLn $ "Error message: " ++ stderr
