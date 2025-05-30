{-# OPTIONS -XFlexibleInstances -XMultiParamTypeClasses#-}

module Parser (Parser, parseSome, parseAll,
               eof, matchAny, matching, match, matches,
               option, lookahead, failif,
               numParsed, ParseError, errorLocation, errorNames,
               ($=)) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State

-------------------- Module interface --------------------

-- Invocation

-- Run a parser on a [t], returning an a.  Success returns (result,
-- number of list items parsed, remainder of list).
parseSome :: Parser t a -> [t] -> Either ParseError (a, Int, [t])
-- Run (parser >> eof) and return only the result.
parseAll  :: Parser t a -> [t] -> Either ParseError a

-- Basic parsers

eof      :: Parser t ()                   -- End of input
matchAny :: Parser t t                    -- Any single t
matching :: (t -> Bool)   -> Parser t t   -- A t matching a property
match    :: (Eq t) => t   -> Parser t t   -- A specific t
matches  :: (Eq t) => [t] -> Parser t [t] -- A sequence of specific t's
-- The plural of match is matches

-- How to build parsers (PEG functionality)

-- Sequencing is monadic
-- Either-or alternatives are via <|> or mplus
-- empty or mzero is a parser that always fails

-- Zero-or-one (aka Optional)
option :: Parser t a -> Parser t (Maybe a)

-- Look ahead, match p, do not consume input
lookahead :: Parser t a -> Parser t a

-- Look ahead, match if p does not match, do not consume input
failif :: Parser t a -> Parser t ()

-- Error handling

-- Assign a name to a parser, which is returned if the parser fails
infixr 4 $=
($=) :: String -> Parser t a -> Parser t a

-- Names are returned in the errorNames field of a ParserError,
-- if the parse failed within that parser.
data ParseError = MakeParseError { errorLocation :: Int,
                                   errorNames ::  [(Int, String)] }
                deriving (Eq, Show)

-------------------- Innards --------------------

-- Everything that sees the inside of parsers goes here.  This should
-- all be unit tested heavily and thoroughly.

-- Types

data ParseState t = ParseState Int [t] ParseError

makeError :: [(Int, String)] -> ParseState t -> ParseError
makeError names st = bestError pastError currentError where
  ParseState loc _ pastError = st
  currentError = MakeParseError loc names

bestError :: ParseError -> ParseError -> ParseError
bestError e f = let m = errorLocation e
                    n = errorLocation f
                in if m > n then e else f

addError :: ParseState t -> ParseError -> ParseState t
addError s e = ParseState num rest err where
  ParseState num rest pastErr = s
  err = bestError pastErr e

newtype Parser t a = MakeParser { getParser :: [(Int, String)] -> StateT
                                  (ParseState t) (Either ParseError) a }

-- Invocation

parseSome parser input =
  case runStateT (getParser parser [])
       (ParseState 0 input (MakeParseError 0 [])) of
    Left err -> Left err
    Right (val, ParseState num rest _) -> Right (val,num,rest)

-- Basic parsers

matching p = MakeParser $ \names ->
  do st <- get
     let ParseState num rest err = st
     case rest of
       (x:xs) | p x ->
         do put $ ParseState (num+1) xs err
            return x
       _ -> throwError $ makeError names st

numParsed :: Parser t Int
numParsed = MakeParser $ \_ ->
  do st <- get
     let ParseState num _ _ = st
     return num

-- PEG functionality

instance Functor (Parser t) where
  fmap f p = p >>= return . f

instance Applicative (Parser t) where
  p <*> q = p >>= \f -> q >>= \x -> return (f x)
  pure = MakeParser . const . return

instance Monad (Parser t) where
  p >>= f = MakeParser $ \names ->
            getParser p names >>= flip getParser names . f
  return  = pure

instance Alternative (Parser t) where
  (<|>) = mplus
  empty = mzero

instance MonadFail (Parser t) where
  fail _  = mzero

instance MonadPlus (Parser t) where
  mplus p q = MakeParser $ \names ->
              getParser p names
              `catchError` \e ->
              do s <- get
                 put $ addError s e
                 getParser q names
  mzero = MakeParser $ \names ->
    do st <- get
       throwError $ makeError names st

lookahead p = MakeParser $ \names ->
  do st <- get
     x <- getParser p names
     put st
     return x

a $= p = MakeParser $ \names ->
  do ParseState n _ _ <- get
     getParser p ((n,a):names)

-------------------- Additional functionality --------------------

-- This is all stuff the user could have defined externally to this
-- module.  Its correctness should follow from the above, so it needs
-- only cursory testing.

-- Invocation

parseAll parser input =
  case parseSome (do x <- parser
                     eof
                     return x) input of
    Left err -> Left err
    Right (out,_,_) -> Right out

-- Basic parsers

eof = failif matchAny

matchAny = matching (const True)

match c = matching (== c)

matches w = sequence (map match w)

-- PEG functionality

option p = (p >>= (return . Just)) <|> return Nothing

failif p = join $ lookahead ((p >> return mzero) <|> return (return ()))
