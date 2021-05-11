module Examples where

import Syntax

-- | Example function, that add increments it's arg by 1
fInc :: Func
fInc = Func "inc" [("x",TI)] TI (Return (BinOp Add (Ref "x") (I 1)))

-- | Example attribute passed into vertex shader
aAttr :: Func
aAttr = attribute TV4 "aAttr" (wrap (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int))

-- | Example varying, passed from vertex -> fragment
vColor :: Func
vColor = varying TV4 "vColor" (wrap (0.2 :: Float, 0.5 :: Float, 0.6 :: Float, 1.0 :: Float))

-- | Simple expr, corresponding to vec2(1,2) (a vector of ints)
expr1 :: Expr
expr1 = V2 (I 1, I 2)

-- | Example program w/ ints (shouldn't type-check later)
--e1 :: Prog
--e1 = Prog [fInc] [] (VertShader [] (Let TI "x" (I 5) (Return U))) (FragShader [] (Return $ I 9))

-- Example program with bools (shouldn't type-check later)
--e2 :: Prog
--e2 = Prog [fInc] [] (VertShader [] (Return $ B False)) (FragShader [] (Return $ B True))

-- | Shouldn't type check later
--e3 :: Prog
--e3 = Prog [fInc] [] (VertShader [] (Return $ I 5)) (FragShader [] (Return $ B True))

-- | Program w/ branching, also shouldn't type check later
--e4 :: Prog
--e4 = Prog [fInc] [aAttr] (VertShader [vColor] (Let TB "x" (B False) (Branch ((Ref "x")) (Return $ I 2) (Return U)))) (FragShader [] (Return $ B True))

-- | Attribute corresponding to a vertex pos passed in at the start of a vertex shader, w/ default vals
aVertPos :: Func
aVertPos = varying TV4 "aVertPos" (wrap (0.2 :: Float, 0.5 :: Float, 0.6 :: Float, 1.0 :: Float))

-- | Varying x and Y vector that will be passed to fragment shader, w/ default 0 vals
vXY :: Func
vXY = varying TV2 "vXY" (wrap (0.0 :: Float, 0.0 :: Float))

-- | Uniform Projection matrix (used for model-view-projection)
uProjectionMatrix :: Func
uProjectionMatrix = uniform TMat4 "uProjectionMatrix" U

-- | Uniform Model View matrix (used for model-view-projection)
uModelViewMatrix :: Func
uModelViewMatrix = uniform TMat4 "uModelViewMatrix" U

-- | Vertex shader that just passes along a position
vert1 :: VertShader
vert1 = VertShader [vXY] (Update "gl_Position" (BinOp Mul (Ref "uProjectionMatrix") (BinOp Mul (Ref "uModelViewMatrix") (Ref "aVertPos"))) Nothing)

-- | Vertex shader that extracts and passes along the X and Y position of a given vertex
vertXY :: VertShader
vertXY = VertShader [vXY] (Update "vXY" (V2 (AccessN "aVertPos" "x", AccessN "aVertPos" "y")) Nothing)

-- | Fragment shader that colors by X and Y...if given before
frag1 :: FragShader
frag1 = FragShader [] (Update "gl_FragColor" (V4 (AccessN "vXY" "x", D 0.3, AccessN "vXY" "y", D 1.0)) Nothing)

-- | This is a working shader program that should be type correct
e5 :: Prog
e5 = Prog [uProjectionMatrix,uModelViewMatrix,fInc] [aVertPos] (comp vert1 vertXY) frag1
