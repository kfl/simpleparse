{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-
  Code from Advanced Programming course at DIKU.

  Small monadic parser combinator library. Intended to be (mostly) API
  compatible with ReadP.

  Last updated: Sep 2015
  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module SimpleParse where

import Control.Monad ( MonadPlus(..), liftM )
import Control.Applicative ( Alternative((<|>), empty, many) )
import Data.Char ( isSpace )

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser t -> String -> [(t, String)]
parse (Parser p) = p

parse' :: Parser t -> String -> [t]
parse' p s = [ result | (result,rest) <- parse p s, null rest ]


get :: Parser Char     -- String -> [(Char,String)]
get = Parser get'
  where get' "" = [ ]
        get' (x : xs) = [(x,xs)]

item :: Parser Char
item = get

look :: Parser String
look = Parser look'
  where look' s = [(s, s)]

reject :: Parser a
reject = Parser $ \ _ -> []

pfail :: Parser a
pfail = reject

eof :: Parser ()
eof = Parser  eof'
  where eof' "" = [((),[])]
        eof' _  = []

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \s -> case parse p s of
  []  -> [((), s)]
  _:_ -> []

parseEof :: Parser t -> String -> [(t, String)]
parseEof p = parse $ liftM fst $ p >>> eof

(>>>) :: Parser a -> Parser b -> Parser (a,b)
p >>> q = Parser $ \ s -> [ ((a,b), cs) | (a, cs1) <- parse p s
                                        , (b, cs)  <- parse q cs1]

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  fm <*> xm = do f <- fm
                 c <- xm
                 return $ f c
  pure = return

instance Monad Parser where
   p >>= q  = Parser$ \cs -> [(v2, cs2) |
                              (v1, cs1) <- parse p cs,
                              (v2, cs2) <- parse (q v1) cs1]

   return v = Parser$ \cs -> [(v, cs)]

instance Alternative Parser where
  p <|> q = Parser$ \cs -> parse p cs ++ parse q cs
  many p = many1 p <|> return []
  empty = reject

(<++) :: Parser a -> Parser a -> Parser a
p <++ q = Parser (\cs -> case parse p cs of
                     []  -> parse q cs
                     res -> res)



char :: Char -> Parser Char
char e = do c <- get
            if e == c
              then return c
              else reject

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- get
               if p c
                 then return c
                 else reject

string :: String -> Parser String
string "" = return ""
string (c:cs) = do char c
                   string cs
                   return (c:cs)

instance MonadPlus Parser where
  p `mplus` q = p <|> q
  mzero       = reject

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

munch :: (Char -> Bool) -> Parser String
munch p =
  do s <- look
     scan s
 where
  scan (c:cs) | p c = do _ <- get; s <- scan cs; return (c:s)
  scan _            = do return ""

munch1 :: (Char -> Bool) -> Parser String
munch1 p =
  do c <- get
     if p c then do s <- munch p; return (c:s)
            else pfail

sepBy           :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep    = (p `sepBy1` sep) <|> return []

sepBy1          :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep   = do {a <- p; as <- many (do {sep; p}); return (a:as)}

chainl          :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a    = (p `chainl1` op) <|> return a

chainl1         :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op   = do a <- p
                      rest a
                   where
                      rest a = do f <- op
                                  b <- p
                                  rest (f a b)
                               <|> return a

option :: Parser a -> Parser (Maybe a)
option p = do v <- p
              return (Just v)
           <|> return Nothing


-- Lexical combinators: ----------------------------------------------

space           :: Parser Char
space            = satisfy isSpace

spaces          :: Parser String
spaces           = many space

token           :: Parser a -> Parser a
token p          = spaces >> p

symbol           :: String -> Parser String
symbol           = token . string

schar            :: Char -> Parser Char
schar            = token . char
