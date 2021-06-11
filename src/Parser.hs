module Parser where

--
-- FDSSL Parser
--

import Syntax
import Pretty

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import GHC.Float
import Data.List

import Data.Functor.Identity(Identity)

-- | Parser state from a program
data PState = PState {
  uniforms :: Env, -- uniforms
  funcs    :: [Func], -- user-defined functions
  shaders  :: [(String,Shader)], -- shaders declared along the way
  progs    :: [(String,Prog)]  -- programs parsed along the way
}

-- | Consumes input & returns the original parser
consume :: (String, a)-> Parser a
consume (s,a) = reserved s *> return a

-- | Allows checkig over a list of literal sequences to parse & their corresponding results
listTrans :: [(String, a)] -> Parser a
listTrans = choice . map consume

-- | Initial program state
initialState :: PState
initialState = PState [] [] [] []

-- | Find a shader in the state
lookupShader :: String -> Parser (Maybe Shader)
lookupShader s = do
  (PState _ _ ss _) <- getState
  return $ lookup s ss

-- | Add uniform variable to the state
addUni :: Opaque -> Parser ()
addUni u' = do
  (PState u f s p) <- getState
  putState $ PState (u':u) f s p

-- | Retrieves uniform (global) variables from the state
getUnis :: Parser (Env)
getUnis = getState >>= return . uniforms

-- | Retrieves functions in the state
getFuncs :: Parser [Func]
getFuncs = getState >>= return . funcs

-- | Adds a function into the state
addFunc :: Func -> Parser ()
addFunc f' = do
  (PState u f s p) <- getState
  putState $ PState u (f':f) s p

-- | Adds a shader into the state
addShader :: (String,Shader) -> Parser ()
addShader s' = do
  (PState u f s p) <- getState
  putState $ PState u f (s':s) p

-- | Adds a program into the state
addProg :: (String,Prog) -> Parser ()
addProg p' = do
  (PState u f s p) <- getState
  putState $ PState u f s (p':p)

-- | Parser helper
type Parser = Parsec String (PState)

-- | The FDSSL language definition
defFDSSL :: P.GenLanguageDef String u Identity
defFDSSL = (haskellStyle
  {
    P.reservedNames = [
                        "uniform",
                        "frag","vert",
                        "Int","Float","Bool","Vec2","Vec3","Vec4","Mat4","Prog",
                        "true","false",
                        "mut","const",
                        "if","then","else","for","do","set","out"],
    P.reservedOpNames = [
                          "=","*","<","<=",">",">=","==","!=","-","+","/",":",".","->"
                        ],
    P.commentStart = "/*",
    P.commentEnd = "*/",
    P.commentLine = "//"
  })

-- | Helper function for handling operators
op :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
op s f assoc = Infix (reservedOp s *> pure f) assoc

-- BOps we have to worry about
operators :: [[Operator String u Identity (Expr)]]
operators = [
   [op "*" (BinOp Mul) AssocLeft, op "/" (BinOp Div) AssocLeft, op "%" (BinOp Mod) AssocLeft],
   [op "+" (BinOp Add) AssocLeft, op "-" (BinOp Sub) AssocLeft],
   [op "<" (BinOp Lt) AssocLeft],
   [op "<=" (BinOp Lte) AssocLeft],
   [op "==" (BinOp Eq) AssocLeft],
   [op "!=" (BinOp Neq) AssocLeft],
   [op ">=" (BinOp Gte) AssocLeft],
   [op ">" (BinOp Gt) AssocLeft]]

-- | Parse for Exprs
parseExpr :: Parser (Expr)
parseExpr = buildExpressionParser operators parseExpr'

-- | lexer w/ reserved keywods & op names
lexer :: P.GenTokenParser String u Data.Functor.Identity.Identity
lexer = P.makeTokenParser defFDSSL

-- | Identifier recognizer (lower or upper case a-z start)
lowIdentifier :: ParsecT String u Identity String
lowIdentifier = P.identifier lexer

-- | parser for whitespace
whitespace :: ParsecT String u Identity ()
whitespace = P.whiteSpace lexer

-- | Comma separated values, 2 or more
commaSep2 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep2 p = (:) <$> (lexeme p <* lexeme comma) <*> commaSep1 p

-- | Comma separated values, 1 or more
commaSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep1 = P.commaSep1 lexer

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = P.commaSep lexer

-- | Comma separator
comma :: ParsecT String u Identity String
comma = P.comma lexer

-- | int parser
int :: ParsecT String u Identity Int
int = fromInteger <$> P.integer lexer

float :: ParsecT String u Identity Float
float = double2Float <$> P.float lexer

double :: ParsecT String u Identity Double
double = P.float lexer

lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = P.lexeme lexer

-- | Parse reserved tokens
reserved :: String -> ParsecT String u Identity ()
reserved = P.reserved lexer

-- | Parse reserved op
reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp lexer

-- | Parentheses recognizer
parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = P.braces lexer

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = P.brackets lexer

-- | Parse a type
parseType :: Parser (Type)
parseType = listTrans [
    ("Int", TI),
    ("Bool", TB),
    ("Float", TF),
    ("Vec2", TV2),
    ("Vec3", TV3),
    ("Vec4", TV4),
    ("Mat4", TMat4),
    ("Array", TArray)
    ]

-- | Parse a single param, used for function input
parseParam :: Parser (String,Type)
parseParam =
  do
    t <- parseType
    n <- lowIdentifier
    return (n,t)

-- | Parse several params
parseParams :: Parser ([(String,Type)])
parseParams = parens $ commaSep $ parseParam

-- | Parse a uniform
parseUniform :: Parser ()
parseUniform =
  do
    whitespace
    reserved "uniform"
    typ <- parseType
    name <- lowIdentifier
    let u = Opaque Uniform typ name
    -- add uniform to state
    addUni u
    return ()

-- | Parse a function signature
parseFuncSignature :: Parser ([(String,Type)],Type)
parseFuncSignature =
  -- (t,t,) -> t
  do
    params <- parseParams
    reservedOp "->"
    t <- parseType
    return (params,t)
  <|>
  -- t
  do
    t <- parseType
    return ([],t)

parseShaderSignature :: Parser ([(String,Type)],[(String,Type)])
parseShaderSignature =
  do
    t1 <- parseParams
    whitespace
    reservedOp "->"
    whitespace
    t2 <- parseParams
    return (t1,t2)


parseBOp :: Parser (BOp)
parseBOp = listTrans [
  ("+", Add),
  ("-", Sub),
  ("*", Mul),
  ("/", Div),
  ("%", Mod),
  ("&&", And),
  ("||", Or),
  ("==", Eq),
  ("!=", Neq),
  (">=", Gte),
  (">", Gt),
  ("<=", Lte),
  ("<", Lt)
  ]

  -- TODO no bitwise ops in here yet

parseBlock :: Parser [Expr]
parseBlock = braces $ many1 parseExpr

-- | Parse an expr
parseExpr' :: Parser (Expr)
parseExpr' =
  Mut <$> (reserved "mut" *> parseType) <*> lowIdentifier <*> (reserved "=" *> parseExpr)
  <|>
  Const <$> (reserved "const" *> parseType) <*> lowIdentifier <*> (reserved "=" *> parseExpr)
  <|>
  Update <$> (reserved "set" *> lowIdentifier) <*> parseExpr
  <|>
  Out <$> (reserved "out" *> lowIdentifier) <*> parseExpr
  <|>
  Branch <$> (reserved "if" *> parseExpr) <*> (reserved "then" *> parseBlock) <*> (reserved "else" *> parseBlock)
  <|>
  -- For with named var
  try(do
    reserved "for"
    e1 <- parseExpr
    reserved "do"
    e2 <- lowIdentifier
    blk <- parseBlock
    return $ For e1 (Just e2) blk)
  <|>
  For <$> (reserved "for" *> parseExpr) <*> (consume ("do", Nothing)) <*> parseBlock
  -- | TODO originally imagined to be incorporated into the syntax, but Parsec likes to eat comments before we can use them
  -- marked for future work...
  -- <|>
  -- SComment <$> (string "//" *> manyTill anyChar newline) <*> parseExpr
  -- <|>
  -- BComment <$> (string "/*" *> manyTill anyChar (try (string "*/"))) <*> parseExpr
  -- -- sequence will never be used in concrete syntax
  <|>
  F <$> try float
  <|>
  D <$> try double
  <|>
  I <$> int
  <|>
  B <$> listTrans [("true", True), ("false", False)]
  <|>
  do
    reserved "vec4"
    e1 <- parseExpr
    whitespace
    e2 <- parseExpr
    whitespace
    e3 <- parseExpr
    whitespace
    e4 <- parseExpr
    return $ V4 (e1,e2,e3,e4)
  <|>
  do
    reserved "vec3"
    e1 <- parseExpr
    whitespace
    e2 <- parseExpr
    whitespace
    e3 <- parseExpr
    return $ V3 (e1,e2,e3)
  <|>
  do
    reserved "vec2"
    e1 <- parseExpr
    whitespace
    e2 <- parseExpr
    return $ V2 (e1,e2)
  -- -- TODO add Mat4, but realizing some names may need to be looked up to get their types...
  <|>
  Array <$> (brackets (commaSep1 parseExpr))
  <|>
  -- application
  try (
    do
      i <- lowIdentifier
      es <- parens (many1 parseExpr)
      return $ App i es
  )
  <|>
  try (AccessI <$> lowIdentifier <*> (brackets int))
  <|>
  try (AccessN <$> lowIdentifier <*> (brackets lowIdentifier))
  <|>
  (try (Ref <$> (lowIdentifier <* notFollowedBy (reserved "[" <|> reserved "("))))
  <|>
  parens (parseExpr <* notFollowedBy comma) -- expr wrapped in parens, which is ok, but not part of list of sorts...
  -- if followed by a comma

-- | Parse a function
parseFunc :: Parser ()
parseFunc =
  do
    whitespace
    name <- lowIdentifier
    reservedOp ":"
    (params,typ) <- parseFuncSignature
    reservedOp "="
    expr <- parseBlock
    let f = Func name params typ expr
    -- update the state with this function
    addFunc f
    return ()

-- | Convert a pair of name & type (a param), to a func for internal rep
toVarying :: (String,Type) -> Opaque
toVarying (s,t) = Opaque Varying t s

-- | Convert a pair of name & type (a param) into an attribute passed into the vert shader
toAttribute :: (String,Type) -> Opaque
toAttribute (s,t) = Opaque Attribute t s

parseShader :: Parser ()
parseShader = try $ do
  t <- listTrans [("vert", VertShader), ("frag", FragShader)]
  n <- lowIdentifier
  reservedOp ":"
  (e1,e2) <- parseShaderSignature
  whitespace
  reservedOp "="
  e <- parseBlock
  let shader = Shader t (case t of
                          VertShader -> map toAttribute e1
                          FragShader -> map toVarying e1) (map toVarying e2) e
  -- add this shader to the env
  addShader (n,shader)
  return ()

-- | special composition parsing
parseCompShader :: Parser ()
parseCompShader = try $ do
  t <- listTrans [("vert", VertShader), ("frag", FragShader)]
  n <- lowIdentifier
  reservedOp ":"
  (e1,e2) <- parseShaderSignature
  whitespace
  reservedOp "="
  s1 <- lowIdentifier
  reservedOp "."
  s2 <- lowIdentifier
  --let shader = Shader t (map toVarying e1) (map toVarying e2) e
  -- add this shader to the env
  (Just s1') <- lookupShader s1
  (Just s2') <- lookupShader s2
  case comp s2' s1' of
    (Just q) -> do
      addShader (n,q)
      return ()
    Nothing  -> unexpected "Bad shader composition"

-- | Get the inputs to this shader (attributes or varyings)
getInputs :: Shader -> Env
getInputs (Shader _ e _ _) = e

-- parse a single program
parseProgram :: Parser ()
parseProgram = try $ do
  n <- lowIdentifier
  reservedOp ":"
  reserved "Prog"
  reservedOp "="
  reserved "mkProg"
  -- get the names of the vertex & fragment shaders
  v <- lowIdentifier
  f <- lowIdentifier
  -- try to lookup these shaders, and build a program if possible
  (Just vs) <- lookupShader v
  (Just fs) <- lookupShader f
  unis <- getUnis
  progFuncs <- getFuncs
  let funs = zip (repeat Nothing) progFuncs :: Funcs
  let p = Prog unis funs vs fs
  -- add a named program to the env
  addProg (n,p)
  return ()
  --  (_,_) -> unexpected $ "Could not find vertex or fragment shader for program " ++ n

-- parse an entire FDSSL script, and return the programs it produced
parseFDSSL :: Parser ([(String,Prog)])
parseFDSSL =
  many (choice [parseProgram,parseUniform,parseFunc,parseShader,parseCompShader]) >> getState >>= return . progs

parseFDSSLFile :: String -> IO (Either ParseError ([(String,Prog)]))
parseFDSSLFile f = do
  c <- readFile f
  return $ runParser (parseFDSSL <* eof) initialState f c

parseFDSSLFiles :: [String] -> IO (Either ParseError ([(String,Prog)]))
parseFDSSLFiles fs = do
  c <- mapM readFile fs
  return $ runParser (parseFDSSL <* eof) initialState "FDSSL Programs" (intercalate "\n" c)
