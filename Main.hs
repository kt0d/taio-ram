module Main where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

type Symbol = Char
type Register = [Symbol]
type Addr = [Symbol]
data AddrOperand = Direct Addr | Indirect Addr deriving (Show)
type Memory = Map Addr Register
type Label = String
data Instruction =
        Add Symbol AddrOperand
    |   Del AddrOperand
    |   Clr AddrOperand
    |   Cpy AddrOperand AddrOperand
    |   Jmp Label
    |   JmpIf Symbol AddrOperand Label
    deriving (Show)
data Statement = L Label | Instr Instruction deriving (Show)
type Source = [Statement]
type Program = [Instruction]
type JumpMap = Map Label Program
data Machine = Machine Program JumpMap

mkMachine :: Source -> Machine
mkMachine = foldr addJumps (Machine [] M.empty)
addJumps :: Statement -> Machine -> Machine
addJumps (Instr i) (Machine p j) = Machine (i:p) j
addJumps (L l) (Machine p j) = Machine p (M.insert l p j)

type MemoryInstr = Maybe Register -> Maybe Register

execAdd :: Symbol -> MemoryInstr
execAdd s (Just l) = Just $ l ++ [s]
execAdd s _ = Just [s]

execDel :: MemoryInstr
execDel (Just (_:xs)) = Just xs
execDel _ = Nothing

execClr :: MemoryInstr
execClr _ = Just []

execCpy :: Register -> MemoryInstr
execCpy r _ = Just r

type ControlInstr = Machine -> Machine

nextInstr :: ControlInstr
nextInstr (Machine (_:xs) j) = Machine xs j

execJmp :: Label -> ControlInstr
execJmp l (Machine _ j) = case M.lookup l j of 
    Just p -> Machine p j
    Nothing -> errorWithoutStackTrace $ "Unknown label: " ++ l

execJmpIf :: Symbol -> Register -> Label -> ControlInstr
execJmpIf s (x:_) l m
    | x == s = execJmp l m
    | otherwise = nextInstr m
execJmpIf _ _ _ m = nextInstr m

loadRegister :: Memory -> AddrOperand -> Register
loadRegister mem (Direct a) = M.findWithDefault [] a mem
loadRegister mem (Indirect a) = loadRegister mem $ Direct $ M.findWithDefault [] a mem

runMachine :: Memory -> Machine -> Memory
runMachine mem m@(Machine (x:_) _) = runMachine newMem newMachine
    where 
        alter (Direct a) f = M.alter f a mem
        alter (Indirect a) f = M.alter f (loadRegister mem $ Direct a) mem
        newMem = case x of
            Add s a -> alter a $ execAdd s
            Del a -> alter a $ execDel
            Clr a -> alter a $ execClr
            Cpy dst src -> alter dst $ execCpy $ loadRegister mem src
            _ -> mem
        newMachine = case x of 
            Jmp l -> execJmp l m
            JmpIf s a l -> execJmpIf s (loadRegister mem a) l m
            _ -> nextInstr m
runMachine mem (Machine [] _) = mem

parseAddr :: ReadP Addr
parseAddr = munch1 isAlphaNum

parseDirAddr :: ReadP AddrOperand
parseDirAddr = Direct <$> parseAddr

parseIndirAddr :: ReadP AddrOperand
parseIndirAddr = do
    char '('
    a <- parseAddr
    char ')'
    return $ Indirect a

parseAddrOp :: ReadP AddrOperand
parseAddrOp = parseDirAddr <++ parseIndirAddr

parseAdd :: ReadP Instruction
parseAdd = do
    string "add"
    skipSpaces
    s <- get
    skipSpaces
    Add s <$> parseAddrOp

parseDel :: ReadP Instruction
parseDel = do
    string "del"
    skipSpaces
    Del <$> parseAddrOp

parseClr :: ReadP Instruction
parseClr = do
    string "clr"
    skipSpaces
    Clr <$> parseAddrOp

parseCpy :: ReadP Instruction
parseCpy = do
    string "cpy"
    skipSpaces
    dst <- parseAddrOp
    skipSpaces
    Cpy dst <$> parseAddrOp

parseLabel :: ReadP Label
parseLabel = munch1 isAlphaNum

parseLabelStmnt :: ReadP Statement
parseLabelStmnt = do
    l <- parseLabel
    char ':'
    return $ L l

parseJmp :: ReadP Instruction
parseJmp = do
    string "jmp"
    skipSpaces
    Jmp <$> parseLabel

parseJmpIf :: ReadP Instruction
parseJmpIf = do
    string "jmpif"
    skipSpaces
    s <- get
    skipSpaces
    a <- parseAddrOp
    skipSpaces
    l <- parseLabel
    return $ JmpIf s a l

parseStmnt :: ReadP Statement
parseStmnt = choice $ parseLabelStmnt:(map (Instr <$>) [parseAdd, parseDel, parseClr, parseCpy, parseJmp, parseJmpIf])

parseSource :: ReadP Source
parseSource = do
    skipSpaces
    src <- sepBy1 parseStmnt skipSpaces
    skipSpaces
    eof
    return src

main :: IO ()
main = do
    input <- getContents
    let parsed = readP_to_S parseSource input
    case parsed of
        [] -> errorWithoutStackTrace "Could not parse source"
        _:_:_ -> errorWithoutStackTrace "Ambigous parse"
        (_,str@(_:_)):_ -> errorWithoutStackTrace $ "Could not parse whole source, stopped at: " ++ str
        _ -> return ()
    let src = fst $ head parsed
    let machine = mkMachine src
    let mem = runMachine M.empty machine
    forM_ (M.toAscList mem) $
        \(r,v) -> putStrLn $ "[" ++ r ++ "]: " ++ v
