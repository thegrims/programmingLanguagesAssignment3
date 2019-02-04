-- Name: Aidan Grimshaw ONID: 932 681 399
module HW3 where
    
import Data.List
-- >>> steps 0
-- pen up;
-- move(0, 0);
-- >>> steps 1
-- pen up;
-- move(0, 0);
-- pen down;
-- move(0, 1);
-- move(1, 1);
-- >> steps 2
-- pen up;
-- move(0, 0);
-- pen down;
-- move(0, 1);
-- move(1, 1);
-- move(1, 2);
-- move(2, 2);

-- 1. Define the abstract syntax of MiniLogo as a set of Haskell data types.

-- num	::=	(any natural number)	
-- var	::=	(any variable name)	
-- macro	::=	(any macro name)	

-- prog	::=	ε   |   cmd ; prog	sequence of commands

-- mode	::=	down   |   up	pen status

-- expr	::=	var     variable reference
-- |	num	        literal number
-- |	expr + expr	addition expression

-- cmd	::=	pen mode	change pen mode
-- |	move ( expr , expr )	move pen to a new position
-- |	define macro ( var* ) { prog }  	define a macro
-- |	call macro ( expr* )	invoke a macro

data Mode = Down | Up
    deriving (Eq,Show)

type Prog = [Cmd]

data Expr 
    = Ref String
    | Lit Int
    | Add Expr Expr
    deriving (Eq,Show)

type Macro = String
type Var = String

data Cmd 
    = Pen Mode
    | Move (Expr, Expr)
    | Define Macro [Var] Prog
    | Call Macro [Expr]
    deriving (Eq,Show)

ex1 :: Prog
ex1 = [Pen Up, Move (Lit 0,Lit 0), Pen Down, Move (Lit 2, Lit 2), nix, line]

-- 2. Part 1, Concrete Syntax

-- define line (x1,y1,x2,y2) {
--   pen up; move (x1,y1);
--   pen down; move (x2,y2); 
--   pen up;
-- }

-- 2. Part 2, Abstract Syntax

line = Define "line" ["x1","y1","x2","y2"] [Pen Up, Move (Ref "x1",Ref "y1"), Pen Down, Move (Ref "x2", Ref "x2"), Pen Up]

-- 3. Part 1, Concrete Syntax

-- define nix (x,y,w,h) {
-- call line (x, y, w + x, h + y);
-- call line (x + w, y, x, h);
-- }

-- 3. Part 2, Abstract Syntax

nix = Define "nix" ["x","y","w","h"] [ 
    Call "line" [Ref "x", Ref "y", Add (Ref "w") (Ref "x"), Add (Ref "h") (Ref "y")], 
    Call "line" [Add (Ref "x") (Ref "w"),  Ref "y", Ref "x", Ref "h"]
    ]

-- 4. Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo program that draws a staircase of n steps starting from (0,0).

steps :: Int -> Prog
steps 0 = []
steps stepCount = steps (stepCount - 1) ++ [Pen Up, Move (Lit (stepCount-1),Lit (stepCount-1)), Pen Down, Move (Lit (stepCount-1),Lit stepCount), Move (Lit stepCount,Lit stepCount),Pen Up]

-- 5. Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names of all of the macros that are defined anywhere in a given MiniLogo program. Don’t worry about duplicates—
-- if a macro is defined more than once, the resulting list may include multiple copies of its name.

macros :: Prog -> [Macro]
macros [] = []
macros ((Define macroName _ _):b) = macros b ++ [macroName]
macros (a:b) = macros b

-- 6. Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo program. That is, it transforms the abstract syntax (a Haskell value) into nicely formatted concrete syntax 
-- (a string of characters). Your pretty-printed program should look similar to the example programs given above; however, for simplicity you will probably want to print just one command per line.

-- exprToString (Add (Lit 2) (Lit 5))
-- exprToString (Add (Ref "test") (Ref "test2"))
-- exprToString (Lit 3)
-- exprToString (Ref "test")

exprToString :: Expr -> String
exprToString (Ref word) = word
exprToString (Lit num) = show num
exprToString (Add expr1 expr2) = (exprToString expr1) ++ "+" ++ (exprToString expr2)

cmdToString :: Cmd -> String
cmdToString (Move(expr1,expr2)) = "Move (" ++ exprToString expr1 ++ "," ++ exprToString expr2 ++ ");"
cmdToString (Define mName params commands) = "Define " ++ mName ++ " (" ++ (intercalate "," params) ++ ") {\n" ++ (pretty commands) ++ "}\n"
cmdToString (Call cName params) = "Call " ++ cName ++ " (" ++ intercalate "," (map exprToString params) ++ ");"
cmdToString myCmd = show myCmd ++ ";"

-- putStrLn (pretty ex1)
pretty :: Prog -> String
pretty [] = []
pretty (a:b) = (cmdToString a) ++ "\n" ++ (pretty b) 

-- 7. Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions by replacing any additions of literals with the result. 
-- For example, given the expression (2+3)+x, optE should return the expression 5+x.

ex2 :: Expr
ex2 = Add (Add (Lit 2) (Lit 3)) (Ref "x")

b :: Expr
b = Add (Add (Lit 2) (Lit 3)) (Add (Lit 3) (Ref "x2"))

optE :: Expr -> Expr
optE (Add (Lit num) (Lit num2)) = Lit (num + num2)
optE (Add arg1 arg2) = Add (optE arg1) (optE arg2)
optE (Ref myStr) = Ref myStr
optE (Lit num) = Lit num

-- Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions contained in a given program using optE.
-- optP :: Prog -> Prog
-- optP ()