{-# LANGUAGE StandaloneDeriving #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Sql.Oracle.Parser  where

import Database.Sql.Oracle.Type

import Data.Int
import Data.Char
import Data.Data hiding (DataType)
import Data.List
import Data.Functor
import Data.Foldable
import Data.Maybe
import GHC.Generics
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)

import Data.These
import Control.Lens ()

import Data.String.Conversions (cs)
import Data.ByteString.Lazy (ByteString)
import qualified Data.List.NonEmpty as NE

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

import qualified Data.Set as S

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (State, runState, state, gets, put, modify)

import Database.Sql.Info (HasInfo(..))
import Database.Sql.Position
  (Position(..), Range(..), advance, advanceHorizontal, advanceVertical)
import Database.Sql.Type
  ( RawNames(..), QSchemaName(..), ConstrainSNames(..)
  , mkNormalSchema)

import Data.Conduit (ConduitT, runConduitRes, runConduit, bracketP, (.|))
import qualified Data.Conduit.Combinators as C

data Oracle
dialectProxy :: Proxy Oracle
dialectProxy = Proxy
                                                    

data Token = TokWord !Bool !T.Text
           | TokString !ByteString
           | TokNumber !T.Text
           | TokSymbol !T.Text
           | TokVariable !T.Text VariableName
           | TokError !String
           deriving (Show, Eq)

data VariableName = StaticName !T.Text
                  | DynamicName Token
                  deriving (Show, Eq)

type ScopeTableRef = T.Text
data ParserScope = ParserScope
    { selectTableAliases :: Maybe (S.Set ScopeTableRef) }
    deriving (Eq, Ord, Show)

type Parser = P.ParsecT [(Token, Position, Position)] Integer (Reader ParserScope)

tokenize :: T.Text -> [(Token, Position, Position)]
tokenize = go (Position 1 0 0)
  where
    go :: Position -> T.Text -> [(Token, Position, Position)]
    go _ "" = []
    go p t = case T.head t of
      c | isAlpha c || c == '_' || c == '"' ->
          case tokName p t of
            Left token -> [token]
            Right (name, quoted, rest, p') -> (TokWord quoted name, p, p') : go p' rest
      c | isDigit c ->
          let ((token, len), rest) = parseNumber t
              p' = advanceHorizontal len p
            in (token, p, p') : go p' rest
      '$' | "${" `T.isPrefixOf` t ->
          let ((token, len), rest) = parseVariable t
              p' = advanceHorizontal len p
            in (token, p, p') : go p' rest
      '`' ->
        case tokQuotedWord p t of
          Left p' -> [(TokError "end of input inside name", p, p')]
          Right (name, rest, p') -> (TokWord True name, p, p') : go p' rest
      c | (== '\n') c -> let (newlines, rest) = T.span (== '\n') t
                             p' = advanceVertical (T.length newlines) p
                             in go p' rest
      c | (liftA2 (&&) isSpace (/= '\n')) c ->
            let (spaces, rest) = T.span (liftA2 (&&) isSpace (/= '\n')) t
                p' = advanceHorizontal (T.length spaces) p
             in go p' rest
      '-' | "--" `T.isPrefixOf` t ->
          let (comment, rest) = T.span (/= '\n') t
              p' = advanceVertical 1 (advanceHorizontal (T.length comment) p)
           in go p' (T.drop 1 rest)
      '/' | "/*" `T.isPrefixOf` t ->
          case T.breakOn "*/" t of
              (comment, "") ->
                let p' = advance comment p
                 in [(TokError "unterminated join hint", p, p')]
              (comment, rest) ->
                let p' = advance (T.append comment "*/") p
                 in go p' $ T.drop 2 rest
      c | c == '\'' ->
          case tokExtString c p t of
            Left (tok, p') -> [(tok, p, p')]
            Right (string, rest, p') -> (TokString string, p, p') : go p' rest
      '.' ->
          let p' = advanceHorizontal 1 p
           in (TokSymbol ".", p, p') : go p' (T.tail t)
      c | isOperator c -> case readOperator t of
          Just (sym, rest) -> let p' = advanceHorizontal (T.length sym) p
                               in (TokSymbol sym, p, p') : go p' rest
          Nothing ->
              let opchars = T.take 5 t
                  p' = advance opchars p
                  message = unwords
                      [ "unrecognized operator starting with"
                      , show opchars
                      ]
               in [(TokError message, p, p')]
      c ->
          let message = unwords
                  [ "unmatched character ('" ++ show c ++ "') at position"
                  , show p
                  ]
           in [(TokError message, p, advanceHorizontal 1 p)]

isWordBody :: Char -> Bool
isWordBody = liftA3 (\x y z -> x || y || z) isAlphaNum (== '_') (== '$')

operators :: [T.Text]
operators = sortBy (flip compare)
  [ "+", "-", "*", "/", "%"
  , "||"
  , "&", "|", "^", "~"
  , "!"
  , ":", ":="
  , "!=", "<>", ">", "<", ">=", "<=", "<=>", "=", "=="
  , "(", ")", "[", "]", ",", ";"
  ]
isOperator :: Char -> Bool
isOperator c = elem c $ map T.head operators

readOperator t = asum $ map (\ op -> (op,) <$> T.stripPrefix op t) operators

tokName :: Position -> T.Text -> Either (Token, Position, Position) (T.Text, Bool, T.Text, Position)
tokName pos = go pos [] False
  where
    go :: Position -> [T.Text] -> Bool -> T.Text -> Either (Token, Position, Position) (T.Text, Bool, T.Text, Position)
    go p [] _ "" = error $ "parse error at " ++ show p
    go p ts seen_quotes "" = Right (T.concat $ reverse ts, seen_quotes, "", p)
    go p ts seen_quotes input = case T.head input of
      c | isWordBody c ->
        let (word, rest) = T.span isWordBody input
            p' = advanceHorizontal (T.length word) p
         in go p' (T.toLower word:ts) seen_quotes rest
      c | c == '"' ->
        case tokString p '"' input of
            Left p' -> Left (TokError "end of input inside string", p, p')
            Right (quoted, rest, p') -> Right (quoted, True, rest, p')
      _ -> case ts of
        [] -> error "empty token"
        _ -> Right (T.concat $ reverse ts, seen_quotes, input, p)

parseVariable :: T.Text -> ((Token, Int64), T.Text)
parseVariable = runState $ do
  let endOfInput = "end of input inside variable substitution"
      missingNamespaceOrName = "variable substitutions must have a namespace and a name"
  modify $ T.drop 2
  namespace <- state (T.break (== ':'))

  gets (T.take 1) >>= \case
    "" ->
      let varLen = 2 + T.length namespace
       in if (not $ T.null namespace) && (T.last namespace == '}')
           then pure (TokError missingNamespaceOrName, varLen)
           else pure (TokError endOfInput, varLen)
    _ -> do
      modify $ T.drop 1
      gets (T.take 2) >>= \case
        "${" -> do
          ((subName, subLen), rest) <- gets parseVariable
          _ <- put rest
          gets (T.take 1) >>= \case
              "}" -> do
                modify $ T.drop 1
                let varLen = 2 + T.length namespace + 1 + subLen + 1
                    varName = TokVariable namespace $ DynamicName subName
                pure $ liftInnerErrors $ enforceNamespaces $ (varName, varLen)
              _ -> do
                let varLen = 2 + T.length namespace + 1 + subLen
                pure (TokError endOfInput, varLen)
                
        _ -> do
             name <- state (T.break (== '}'))
             gets (T.take 1) >>= \case
               "" ->
                 let varLen = 2 + T.length namespace + 1 + T.length name
                   in pure (TokError endOfInput, varLen)
               _ -> do
                 modify $ T.drop 1
                 let varLen = 2 + T.length namespace + 1 + T.length name + 1
                 if T.null name
                 then pure (TokError missingNamespaceOrName, varLen)
                 else pure (TokError endOfInput, varLen)
  where
    enforceNamespaces tok@(TokVariable ns _, len) =
      let allowedNamespaces = ["hiveconf", "system", "env", "define", "hivevar"]
          permitted = (`elem` allowedNamespaces)
       in if permitted ns
          then tok
          else (TokError $ "bad namespace in variable substitution: " ++ show ns, len)
    enforceNamespaces x = x
    liftInnerErrors (TokVariable _ (DynamicName (TokError msg)), len) = (TokError msg, len)
    liftInnerErrors x = x

parseNumber :: T.Text -> ((Token, Int64), T.Text)
parseNumber = runState $ do
  ipart <- state $ T.span isDigit
  gets (T.take 1) >>= \case
    "" -> pure (TokNumber ipart, T.length ipart)
    "." -> do
      modify $ T.drop 1
      fpart <- state $ T.span isDigit
      gets (T.take 1) >>= \case
        "e" -> do
          modify $ T.drop 1
          sign <- gets (T.take 1) >>= \case
            s | elem s ["+", "-"] -> modify (T.drop 1) >> pure s
            _ -> pure ""
          state (T.span isDigit) >>= \ epart ->
            let number = T.concat [ipart, ".", fpart, "e", sign, epart]
              in pure (TokNumber number, T.length number)
        _ ->
          let number = T.concat [ipart, ".", fpart]
            in pure (TokNumber number, T.length number)
    "e" -> do
      modify $ T.drop 1
      sign <- gets (T.take 1) >>= \case
        s | elem s ["+", "-"] -> modify (T.drop 1) >> pure s
        _ -> pure ""
      epart <- state $ T.span isDigit
      gets (T.take 1) >>= \case
        c | liftA2 (&&) (not . T.null) (isWordBody . T.head) c || T.null epart -> do
              rest <- state $ T.span isWordBody
              let word = T.concat [ipart, "e", sign, epart, rest]
              pure (TokWord False word, T.length word)
          | otherwise ->
            let number = T.concat [ipart, "e", sign, epart]
             in pure (TokNumber number, T.length number)
    c | liftA2 (||) isAlpha (== '_') (T.head c) -> do
          rest <- state $ T.span (liftA2 (||) isAlpha (== '_'))
          let word = T.concat [ipart, rest]
          pure (TokWord False word, T.length word)
    _ -> pure (TokNumber ipart, T.length ipart)

tokUnquotedWord :: Position -> T.Text -> (T.Text, T.Text, Position)
tokUnquotedWord pos input =
  case T.span (liftA2 (||) isAlphaNum (== '_')) input of
    (word, rest) -> (T.toLower word, rest, advanceHorizontal (T.length word) pos)

tokQuotedWord :: Position -> T.Text -> Either Position (T.Text, T.Text, Position)
tokQuotedWord pos = go (advanceHorizontal 1 pos) [] . T.tail
  where
    go p _ "" = Left p
    go p ts input = case T.head input of
      c | c == '`' ->
        let (quotes, rest) = T.span (== '`') input
            len = T.length quotes
         in if len `mod` 2 == 0
             then go (advanceHorizontal len p)
                     (T.take (len `div` 2) quotes : ts)
                     rest
             else Right (T.concat $ reverse (T.take (len `div` 2) quotes : ts)
                        , rest
                        , advanceHorizontal len p)
      _ -> let (t, rest) = T.span (/= '`') input
            in go (advance t p) (t:ts) rest

halve txt = T.take (T.length txt `div` 2) txt

tokString :: Position -> Char -> T.Text -> Either Position (T.Text, T.Text, Position)
tokString pos d = go (advanceHorizontal 1 pos) [] . T.tail
  where
    go p _ "" = Left p
    go p ts input = case T.head input of
        c | c == d ->
            let (quotes, rest) = T.span (== d) input
                len = T.length quotes
                t = T.take (len `div` 2) quotes
             in if len `mod` 2 == 0
                 then go (advanceHorizontal len p) (t:ts) rest
                 else let str = T.concat $ reverse $ t:ts
                          p' = advanceHorizontal len p
                       in Right (str, rest, p')
        _ -> let (t, rest) = T.span (/= d) input
              in go (advance t p) (t:ts) rest

tokExtString :: Char -> Position -> T.Text -> Either (Token, Position) (ByteString, T.Text, Position)
tokExtString quote pos = go (advanceHorizontal 1 pos) [] . T.drop 1
  where
    go p ts input = case T.span (not . (`elem` [quote, '\\']))  input of
      (cs, "") -> Left (TokError "end of input inside string", advance cs p)
      ("", rest) -> handleSlashes p ts rest
      (cs, rest) -> handleSlashes (advance cs p) (cs:ts) rest
    handleSlashes p ts input = case T.span (== '\\') input of
      (cs, "") -> Left (TokError "end of input inside string", advance cs p)
      ("", _) -> handleQuote p ts input
      (slashes, rest) ->
        let len = T.length slashes
         in if len `mod` 2 == 0
             then go (advanceHorizontal len p) (halve slashes:ts) rest
             else case T.splitAt 1 rest of
               (c, rest')
                 | c == "a" -> go (advanceHorizontal (len + 1) p) ("\a": halve slashes :ts) rest'
                 | c == "b" -> go (advanceHorizontal (len + 1) p) ("\BS": halve slashes :ts) rest'
                 | c == "f" -> go (advanceHorizontal (len + 1) p) ("\FF": halve slashes :ts) rest'
                 | c == "n" -> go (advanceHorizontal (len + 1) p) ("\n": halve slashes :ts) rest'
                 | c == "r" -> go (advanceHorizontal (len + 1) p) ("\r": halve slashes :ts) rest'
                 | c == "t" -> go (advanceHorizontal (len + 1) p) ("\t": halve slashes :ts) rest'
                 | c == "v" -> go (advanceHorizontal (len + 1) p) ("\v": halve slashes :ts) rest'
                 | c == "'" -> go (advanceHorizontal (len + 1) p) ("'": halve slashes :ts) rest'
                 | c == "\"" -> go (advanceHorizontal (len + 1) p) ("\"": halve slashes :ts) rest'
                 | otherwise -> go (advanceHorizontal (len + 1) p) (c:"\\":halve slashes :ts) rest'

    handleQuote p ts input = case T.splitAt 1 input of
      (c, rest) | c == T.singleton quote ->
        Right ( T.encodeUtf8 $ T.concat $ reverse ts
              , rest
              , advanceHorizontal 1 p
              )
      x -> error $ "this shouldn't happend: handleQuote splitInput got " ++ show x

showTok :: (Token, Position, Position) -> String
showTok (t, _, _) = show t

posFromTok :: P.SourcePos -> (Token, Position, Position) -> [(Token, Position, pOSITION)] -> P.SourcePos
posFromTok _ (_, pos, _) _ = flip P.setSourceLine (fromEnum $ positionLine pos)
                           $ flip P.setSourceColumn (fromEnum $ positionColumn pos)
                           $ P.initialPos "-"

keywordP :: T.Text -> Parser Range
keywordP keyword = P.tokenPrim showTok posFromTok testTok
  where
    testTok (tok, s, e) = case tok of
        TokWord False name | name == keyword -> Just (Range s e)
        _ -> Nothing

nameP :: (T.Text -> Bool) -> Parser (T.Text, Range)
nameP pred = P.tokenPrim showTok posFromTok testTok
  where
    testTok (tok, s, e) = case tok of
      TokWord True name -> Just (name, Range s e)
      TokWord False name -> Just (name, Range s e)
      _ -> Nothing

symbolP :: T.Text -> Parser Range
symbolP t =  P.tokenPrim showTok posFromTok testTok
  where
    testTok (tok, s, e) = case tok of
      TokSymbol t' | t == t' -> Just (Range s e)
      _ -> Nothing

createProcedureP :: Parser (CreateProcedure RawNames Range)
createProcedureP = do
  kCreate <- keywordP "create"
  orReplace <- P.optionMaybe $ (<>) <$> (keywordP "or") <*> (keywordP "replace")
  keywordP "procedure"
  procedureName <- procedureNameP
  createProcedureParams <- P.option [] $ symbolP "(" *> flip P.sepBy1 (symbolP ",") parameterDeclarationP <* symbolP ")"
  P.choice [keywordP "is", keywordP "as"]
  (e, createProcedureImpl) <- createProcedureImplP
  pure $ CreateProcedure
      { createProcedureInfo = kCreate <> e
      , createProcedureOrReplace = orReplace
      , createProcedureName = procedureName
      , createProcedureParams = createProcedureParams
      , createProcedureInvokerRights = Nothing
      , createProcedureImpl = createProcedureImpl
      }

procedureNameP :: Parser (QProcedureName Maybe Range)
procedureNameP = P.choice
  [ P.try $ do
      (s, r) <- nameP (const True)
      symbolP "."
      (t, r') <- nameP (const True)
      return $ QProcedureName (r <> r') (Just (mkNormalSchema s r )) t
  , do
      (t, r)  <- nameP (const True)
      return $ QProcedureName r Nothing t
  ]

dataTypeP :: Parser (DataType RawNames Range)
dataTypeP = P.choice
  [ do
      (s, r) <- scalarTypeP
      return $ DataTypeScalar r s
  ]
  where
    scalarTypeP :: Parser (ScalarType, Range)
    scalarTypeP = P.choice
      [ (VARCHAR2,) <$> keywordP "varchar2" 
      ]

longDataTypeP :: Parser (DataType RawNames Range)
longDataTypeP = P.choice
  [ do
      (s, r) <- scalarTypeP
      return $ DataTypeScalar r s
  ]
  where
    scalarTypeP :: Parser (ScalarType, Range)
    scalarTypeP = P.choice
      [ (VARCHAR2,) <$> keywordP "varchar2" 
      ]


expressionP :: Parser (Expression r a)
expressionP = undefined

parameterDeclarationP :: Parser (ParameterDeclaration RawNames Range)
parameterDeclarationP = do
  (s, r) <- nameP (const True)
  let parameter = Parameter s r
  parameterBody <- P.optionMaybe $ P.choice
    [ P.try $ do
        xIn <- P.optionMaybe (keywordP "in")
        xDataType <- dataTypeP
        xExpressionM <- P.optionMaybe $ do
          P.choice [ symbolP ":=", keywordP "default" ]
          expressionP
        return $ ParameterBodyIn xDataType xExpressionM
    , P.try $ do
        xIn <- P.optionMaybe (keywordP "in")
        xOut <- keywordP "out"
        xNocopy <- isJust <$> P.optionMaybe (keywordP "nocopy")
        xDataType <- dataTypeP
        return $ ParameterBodyInOut xNocopy xDataType
    ]
  return $ ParameterDeclaration (Parameter s r) parameterBody

procedureItemList1P :: Parser (Range, [ProcedureItemList1Item r a])
procedureItemList1P = do
  xs <- flip P.endBy1 (symbolP ";") $ P.choice
    [ -- typeDefinitionP
      -- cursorDefinitionP
      itemDeclarationP
      -- functionDeclarationP
      -- procedureDeclarationP
    ]
  undefined
  where
    typeDefinitionP = undefined
    cursorDefinitionP = undefined
    itemDeclarationP = P.choice
        [ collectionVariableDeclarationP
        , constantDeclarationP
        , cursorVariableDeclarationP
        , exceptionDeclarationP
        , recordVariableDeclarationP
        , variableDeclarationP
        ]
    functionDeclarationP = undefined
    procedureDeclarationP = undefined

    collectionVariableDeclarationP = undefined
    constantDeclarationP = undefined
    cursorVariableDeclarationP = undefined
    exceptionDeclarationP = undefined
    recordVariableDeclarationP = undefined
    variableDeclarationP = do
      (variable, r) <- nameP (const True)
      dataTypeP
      P.optionMaybe $ do
        P.optionMaybe $ keywordP "not" >> keywordP "null"
        P.choice [ symbolP ":=", keywordP "default" ]
        expressionP
        undefined

procedureItemList2P :: Parser (Range, [ProcedureItemList2Item r a])
procedureItemList2P = undefined

maybeThese :: Maybe a -> Maybe b -> Maybe (These a b)
maybeThese (Just a) (Just b) = Just (These a b)
maybeThese (Just a) _ = Just (This a)
maybeThese _ (Just b) = Just (That b)
maybeThese _ _ = Nothing

procedureDeclareSectionP :: Parser (Range, (ProcedureDeclareSection RawNames Range))
procedureDeclareSectionP = P.try $ do
  (r0, itemList1) <- NE.unzip <$> P.optionMaybe procedureItemList1P
  (r1, itemList2) <- NE.unzip <$> P.optionMaybe procedureItemList2P
  let Just itemThese = maybeThese itemList1 itemList2
  let Just r' = (r0 <> r1)
  return (r', ProcedureDeclareSection $ itemThese)

procedureBodyP :: Parser (Range, (ProcedureBody RawNames Range))
procedureBodyP = undefined

createProcedureImplP :: Parser (Range, (CreateProcedureImpl RawNames Range))
createProcedureImplP = P.choice
  [ do
      (r0, procedureDeclare) <- NE.unzip <$> P.optionMaybe procedureDeclareSectionP
      (r1, procedureBody) <- procedureBodyP
      let r' = (fromMaybe r1 r0) <>  r1
      return $ (r', ProcedureDeclareImpl r' procedureDeclare procedureBody)
  ]

statementParser :: Parser (OracleTotalStatement RawNames Range)
statementParser = P.choice
  [ OracleCreateProcedureStatement <$> createProcedureP
  ]

emptyParserScope :: ParserScope
emptyParserScope = ParserScope { selectTableAliases = Nothing }

parse :: T.Text -> Either P.ParseError (OracleTotalStatement RawNames Range)
parse = flip runReader emptyParserScope . P.runParserT statementParser 0 "-" . tokenize

main :: IO ()
main = do
  putStrLn "hello world"
  runConduit
--     $ (liftIO (T.getContents <&> T.lines) >>= C.yieldMany)
    $ C.yieldMany ["/Users/larluo/my-work/my-sample/metadata_local/procedure/oracle/ADM.PR_MSTR2_KPI_INDUSTRY_MONTH.prc"::String]
    .| C.iterM print
    .| C.mapM (cs >>> T.readFile)
--    .| (C.map tokenize .| C.concatMap id)
    .| C.map parse
    .| C.mapM print
    .| C.sinkNull
