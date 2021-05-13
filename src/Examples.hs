module Examples where

import Syntax

{-
Examples that should not type check later on

-- | Example program w/ ints (shouldn't type-check later)
e1 :: Prog
e1 = Prog [fInc] [] (VertShader [] (Let TI "x" (I 5) (Return U))) (FragShader [] (Return $ I 9))

-- | Example program with bools (shouldn't type-check later)
e2 :: Prog
e2 = Prog [fInc] [] (VertShader [] (Return $ B False)) (FragShader [] (Return $ B True))

-- | Shouldn't type check later
e3 :: Prog
e3 = Prog [fInc] [] (VertShader [] (Return $ I 5)) (FragShader [] (Return $ B True))

-- | Program w/ branching, also shouldn't type check later
e4 :: Prog
e4 = Prog [fInc] [aAttr] (VertShader [vColor] (Let TB "x" (B False) (Branch ((Ref "x")) (Return $ I 2) (Return U)))) (FragShader [] (Return $ B True))

-}

-- | Example function, that add increments it's arg by 1
fInc :: Func
fInc = Func "inc" [("x",TI)] TI (Body (BinOp Add (Ref "x") (I 1)) "")

-- | Example attribute passed into vertex shader
aAttr :: Func
aAttr = attribute TV4 "aAttr"

-- | Example varying, passed from vertex -> fragment
vColor :: Func
vColor = varying TV4 "vColor"

-- | Simple expr, corresponding to vec2(1,2) (a vector of ints)
expr1 :: Expr
expr1 = V2 (I 1, I 2)

-- | Attribute corresponding to a vertex pos passed in at the start of a vertex shader, w/ default vals
aVertPos :: Func
aVertPos = attribute TV4 "aVertPos"

-- | Varying x and Y vector that will be passed to fragment shader, w/ default 0 vals
vXY :: Func
vXY = varying TV2 "vXY"

-- | Varying x,y,z vector that passes to the fragment shader
vXYZ :: Func
vXYZ = varying TV3 "vXYZ"

-- | Uniform Projection matrix (used for model-view-projection)
uProjectionMatrix :: Func
uProjectionMatrix = uniform TMat4 "uProjectionMatrix"

-- | Uniform Model View matrix (used for model-view-projection)
uModelViewMatrix :: Func
uModelViewMatrix = uniform TMat4 "uModelViewMatrix"

-- | Time passed into the shader
uTime :: Func
uTime = uniform TF "uTime"

--
-- Vertex Shaders
--

-- Basic vertex shader, just passes along the coordinates without applying any matrix transformations
basicVert :: Shader
basicVert = Shader VertShader [vXYZ] (Update "gl_Position" (Ref "aVertPos") (Just $ Update "vXYZ" (V3 (AccessN "aVertPos" "x",AccessN "aVertPos" "y",AccessN "aVertPos" "z")) Nothing))

-- Vertex shader that applies a model view projection matrix to the position
mvpVert :: Shader
mvpVert = Shader VertShader [] (Update "gl_Position" (BinOp Mul (Ref "uProjectionMatrix") (BinOp Mul (Ref "uModelViewMatrix") (Ref "aVertPos"))) Nothing)

-- Alter position over time, also does MVP projections
timeVert :: Shader
timeVert = Shader VertShader []
  (Let TV4 "pos" (V4 (BinOp Mul (AccessN "aVertPos" "x") (App "sin" $ [Ref "uTime"]), BinOp Mul (AccessN "aVertPos" "y") (App "cos" $ [Ref "uTime"]), AccessN "aVertPos" "z", AccessN "aVertPos" "w"))
  (Update "gl_Position" (BinOp Mul (Ref "uProjectionMatrix") (BinOp Mul (Ref "uModelViewMatrix") (Ref "pos"))) Nothing))

--
-- Fragment Shaders
--

-- Fragment shader that produces a gray coloring everywhere
defaultFrag :: Shader
defaultFrag = Shader FragShader [] (Update "gl_FragColor" (V4 (F 0.5, F 0.5, F 0.5, F 1.0)) Nothing)

-- Fragment shader that changes color based on position
posFrag :: Shader
posFrag = Shader FragShader []
  (Let TF "r" (AccessN "vXYZ" "x")
  (Let TF "g" (AccessN "vXYZ" "y")
  (Let TF "b" (AccessN "vXYZ" "z")
  (Update "gl_FragColor" (V4 (Ref "r", Ref "g", Ref "b", F 1.0)) Nothing))))

timeFrag :: Shader
timeFrag = Shader FragShader []
  (Let TF "r" (BinOp Add (AccessN "vXYZ" "x") (App "cos" [Ref "uTime"]))
  (Let TF "g" (BinOp Add (AccessN "vXYZ" "y") (App "sin" [Ref "uTime"]))
  (Let TF "b" (AccessN "vXYZ" "z")
  (Update "gl_FragColor" (V4 (Ref "r", Ref "g", Ref "b", F 1.0)) Nothing))))

--
-- Programs
--

-- | Simple shader program that shows RGB color values on a surface
prog1 :: Maybe Prog
prog1 = comp basicVert mvpVert >>= \v -> Just $ Prog [uProjectionMatrix,uModelViewMatrix,fInc] [aVertPos] v posFrag

-- | More complex shader, color & shape changes over time
prog2 :: Maybe Prog
prog2 = comp basicVert timeVert >>= \v -> Just $ Prog [uProjectionMatrix,uModelViewMatrix,fInc,uTime] [aVertPos] v timeFrag
