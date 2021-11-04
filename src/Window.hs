--
-- Window module for rendering via OpenGL
--
-- primarily based off of material from https://lokathor.gitbooks.io/using-haskell/content/opengl/hello-window.html

module Window(render) where

import Graphics.UI.GLUT
import Data.IORef
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B

-- Example for loading shaders here
-- https://github.com/haskell-opengl/GLUT/blob/master/examples/RedBook8/Chapter01/Triangles.hs#L44
--

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
   where n' = fromIntegral n

-- | Start the render
render :: IO ()
render = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode    $= [WithDepthBuffer, DoubleBuffered, RGBAMode]
  initialWindowSize     $= Size 512 512
  _window <- createWindow "FDSSL"
  depthFunc $= Just Less -- the comparison function for depth the buffer
  angle <- newIORef 0.0
  delta <- newIORef 0.1
  pos   <- newIORef (0,0)
  displayCallback       $= (display angle pos)
  reshapeCallback       $= Just onReshape
  keyboardMouseCallback $= Just (onKeyboardMouse delta pos)
  idleCallback          $= Just (idle angle delta)

  program <- loadShaders [
     ShaderInfo VertexShader (FileSource "e1.vert"),
     ShaderInfo FragmentShader (FileSource "e1.frag")]
  currentProgram $= Just program

  mainLoop


-- | Display callback
display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  --clear [ColorBuffer]
  loadIdentity
  (x',y') <- get pos
  a <- get angle
  rotate a $ Vector3 1 1 0.3
  translate $ Vector3 x' y' 0
  --scale 0.7 0.7 (0.7::GLfloat), for reference
  color $ Color3 1 1 (1::GLfloat)
  cube 0.4
  -- flush
  swapBuffers

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing


-- | Window reshape callback
onReshape :: ReshapeCallback
onReshape size = do
  viewport $= (Position 0 0, size)
  --postRedisplay Nothing

-- | Keyboard & mouse handler
onKeyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
onKeyboardMouse a p key Down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char '=') -> a $~! (* 2)
  (Char '-') -> a $~! (/ 2)
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y-0.1)
  _ -> return ()
onKeyboardMouse _ _ _ _ _ _ = return ()

cube :: GLfloat -> IO ()
cube w = do
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) w (-w)




--------------------------------------------------------------------------------
-- |
-- Module      :  LoadShaders
-- Copyright   :  (c) Sven Panne 2018
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Utilities for shader handling, adapted from LoadShaders.cpp which is (c) The
-- Red Book Authors.
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The source of the shader source code.

data ShaderSource =
     ByteStringSource B.ByteString
     -- ^ The shader source code is directly given as a 'B.ByteString'.
   | StringSource String
     -- ^ The shader source code is directly given as a 'String'.
   | FileSource FilePath
     -- ^ The shader source code is located in the file at the given 'FilePath'.
   deriving ( Eq, Ord, Show )

getSource :: ShaderSource -> IO B.ByteString
getSource (ByteStringSource bs) = return bs
getSource (StringSource str) = return $ packUtf8 str
getSource (FileSource path) = B.readFile path

--------------------------------------------------------------------------------

-- | A description of a shader: The type of the shader plus its source code.

data ShaderInfo = ShaderInfo ShaderType ShaderSource
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | Create a new program object from the given shaders, throwing an
-- 'IOException' if something goes wrong.

loadShaders :: [ShaderInfo] -> IO Program
loadShaders infos =
   createProgram `bracketOnError` deleteObjectName $ \program -> do
      loadCompileAttach program infos
      linkAndCheck program
      return program

linkAndCheck :: Program -> IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

loadCompileAttach :: Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) =
   createShader shType `bracketOnError` deleteObjectName $ \shader -> do
      src <- getSource source
      shaderSourceBS shader $= src
      compileAndCheck shader
      attachShader program shader
      loadCompileAttach program infos

compileAndCheck :: Shader -> IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   ok <- get (getStatus object)
   unless ok $ do
      infoLog <- get (getInfoLog object)
      fail (message ++ " log: " ++ infoLog)
