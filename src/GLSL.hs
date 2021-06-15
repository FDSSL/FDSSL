module GLSL where
import Syntax (OpaqueType)

data GSize = Two
           | Three
           | Four
        deriving (Eq, Ord)

data GType = Void
           | GBool
           | GInt
           | GUInt
           | GFloat
           | GDouble
           | GVec GSize GType
           | GMat GSize GSize

data GOp = GArr
         | GField
         | GMult
         | GMod
         | GDiv
         | GAdd
         | GSub
         | GRShift
         | GLShift
         | GLt
         | GLte
         | GGt
         | GGte
         | GEq
         | GNEq
         | GAnd
         | GOr
         | GXOr
         | GBAnd
         | GBOr
         | GBXOr

data GUOp = GFLip
          | GNot
          | GNeg
          | GPos

data GConst = GR String
            | GI Int
            | GB Bool
            | GF Float
            | GD Double
            | GV [GConst]
            | GM [[GConst]]

data GExpr = GCall String [GExpr]
           | GParen GExpr
           | GAssign String GExpr
           | Imm GConst
           | GUn GUOp GExpr
           | GBin GOp GExpr GExpr
           | GTern GExpr GExpr GExpr

data GStmt = GFor GExpr GExpr GExpr [GStmt]
           | GWhile GExpr [GStmt]
           | GDoWhile GExpr [GStmt]
           | GIf [(Maybe GExpr, [GStmt])] -- This is so that we can have if/else if/else chains
           | GSwitch GExpr [(GConst,[GStmt])]
           | GStmt GExpr
           | GRet GExpr
           | GBreak
           | GContinue

data GTop = GFunc String [(String,GType)] GType [GStmt]
          | GGlob GExpr                     -- This is for global variables
          | GOpaque String OpaqueType GType -- This is for varying and Attribute

type GShader = [GTop]
