module Examples where

--
-- Example programs written in FDSSL abstract syntax
--

import Syntax

-- | Example function, that add increments it's arg by 1
fInc :: Func
fInc = Func "inc" [("x",TI)] TI [(BinOp Add (Ref "x") (I 1))]

-- | Example attribute passed into vertex shader
aAttr :: Opaque
aAttr = attribute TV4 "aAttr"

-- | Example varying, passed from vertex -> fragment
vColor :: Opaque
vColor = varying TV4 "vColor"

-- | Simple expr, corresponding to vec2(1,2) (a vector of ints)
expr1 :: Expr
expr1 = V2 (I 1, I 2)

-- | Attribute corresponding to a vertex pos passed in at the start of a vertex shader, w/ default vals
aVertPos :: Opaque
aVertPos = attribute TV4 "aVertPos"

-- | Varying x and Y vector that will be passed to fragment shader, w/ default 0 vals
vXY :: Opaque
vXY = varying TV2 "vXY"

-- | Varying x,y,z vector that passes to the fragment shader
vXYZ :: Opaque
vXYZ = varying TV3 "vXYZ"

-- | Uniform Projection matrix (used for model-view-projection)
uProjectionMatrix :: Opaque
uProjectionMatrix = uniform TMat4 "uProjectionMatrix"

-- | Uniform Model View matrix (used for model-view-projection)
uModelViewMatrix :: Opaque
uModelViewMatrix = uniform TMat4 "uModelViewMatrix"

-- | Time passed into the shader
uTime :: Opaque
uTime = uniform TF "uTime"

--
-- Vertex Shaders
--

-- Basic vertex shader, just passes along the coordinates without applying any matrix transformations
basicVert :: Shader
basicVert = Shader VertShader [] [vXYZ] [
  Update "gl_Position" (Ref "aVertPos"),
  Out "vXYZ" (V3 (AccessN "aVertPos" "x", AccessN "aVertPos" "y", AccessN "aVertPos" "z"))
  ]

-- Vertex shader that applies a model view projection matrix to the position
mvpVert :: Shader
mvpVert = Shader VertShader [aVertPos] [] [
  Update "gl_Position" (BinOp Mul (Ref "uProjectionMatrix") (BinOp Mul (Ref "uModelViewMatrix") (Ref "aVertPos")))
  ]

-- Alter position over time, also does MVP projections
timeVert :: Shader
timeVert = Shader VertShader [aVertPos] [vXYZ] [
    Mut TV4 "pos" (V4
                   (BinOp Mul (AccessN "aVertPos" "x") (App "sin" [Ref "uTime"]),
                    BinOp Mul (AccessN "aVertPos" "y") (App "cos" [Ref "uTime"]),
                    AccessN "aVertPos" "z",
                    AccessN "aVertPos" "w")),
    Update "gl_Position" (BinOp Mul (Ref "uProjectionMatrix") (BinOp Mul (Ref "uModelViewMatrix") (Ref "pos")))
 ]

--
-- Fragment Shaders
--

-- Fragment shader that produces a gray coloring everywhere
defaultFrag :: Shader
defaultFrag = Shader FragShader [] [] [Update "gl_FragColor" (V4 (F 0.5, F 0.5, F 0.5, F 1.0))]

-- Fragment shader that changes color based on position
posFrag :: Shader
posFrag = Shader FragShader [vXYZ] [] [
  Mut TF "r" (AccessN "vXYZ" "x"),
  Mut TF "g" (AccessN "vXYZ" "y"),
  Mut TF "b" (AccessN "vXYZ" "z"),
  Update "gl_FragColor" (V4 (Ref "r", Ref "g", Ref "b", F 1.0))
  ]

timeFrag :: Shader
timeFrag = Shader FragShader [vXYZ] [] [
  Mut TF "r" (BinOp Add (AccessN "vXYZ" "x") (App "cos" [Ref "uTime"])),
  Mut TF "g" (BinOp Add (AccessN "vXYZ" "y") (App "sin" [Ref "uTime"])),
  Mut TF "b" (AccessN "vXYZ" "z"),
  Update "gl_FragColor" (V4 (Ref "r", Ref "g", Ref "b", F 1.0))
  ]
--
-- Programs
--

prog :: Prog
prog = Prog [uProjectionMatrix,uModelViewMatrix] [(Nothing, fInc)] mvpVert posFrag

-- | Simple shader program that shows RGB color values on a surface
prog1 :: Maybe Prog
prog1 = comp basicVert mvpVert >>= \v -> Just $ Prog [uProjectionMatrix,uModelViewMatrix] [(Nothing, fInc)] v posFrag

-- | More complex shader, color & shape changes over time
prog2 :: Maybe Prog
prog2 = comp basicVert timeVert >>= \v -> Just $ Prog [uProjectionMatrix,uModelViewMatrix,uTime] [(Nothing, fInc)] v timeFrag
