module Main where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Symbol = Char
type Register = [Symbol]
type Addr = [Symbol]
type Memory = Map Addr Register
type Label = String
data Instruction =
        Add Symbol Addr
    |   Del Addr
    |   Clr Addr
    |   Cpy Addr Addr
    |   Jmp Label
    |   JmpIf Symbol Addr Label
data Statement = L Label | Instr Instruction
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
execAdd s (Just l) = Just (s:l)
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
execJmp l (Machine _ j) = Machine (j M.! l) j

execJmpIf :: Symbol -> Register -> Label -> ControlInstr
execJmpIf s (x:_) l m
    | x == s = execJmp l m
    | otherwise = nextInstr m
execJmpIf _ _ _ m = nextInstr m

loadRegister :: Memory -> Addr -> Register
loadRegister mem a = M.findWithDefault [] a mem

runMachine :: Memory -> Machine -> Memory
runMachine mem m@(Machine (x:_) _) = runMachine newMem newMachine
    where 
        alter a f = M.alter f a mem
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

main :: IO ()
main = do
    let src = [
                Instr (Add 'x' "0")
            ,   Instr (Add 'y' "0")
            ,   Instr (Cpy "1" "0")
            ,   Instr (JmpIf 'y' "1" "test")
            ,   Instr (Add 'z' "3")
            ,   L "test"]
    let machine = mkMachine src
    let mem = runMachine M.empty machine
    print mem
