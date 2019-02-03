-- Name: Aidan Grimshaw ONID: 932 681 399
module HW3 where

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
ex1 = [Pen Up, Move (Lit 0,Lit 0), Pen Down, Move (Lit 2, Lit 2)]

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
-- line (x, y, w + x, h + y)
-- line (x + w, y, x, h)
-- }

-- 3. Part 2, Abstract Syntax

nix = Define "nix" ["x","y","w","h"] [ 
    Call "line" [Ref "x", Ref "y", Add (Ref "w") (Ref "x"), Add (Ref "h") (Ref "y")], 
    Call "line" [Add (Ref "x") (Ref "w"),  Ref "y", Ref "x", Ref "h"]
    ]