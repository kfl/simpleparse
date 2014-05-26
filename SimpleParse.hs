{-
  Example code from Advanced Programming lecture.

  Small monadic parser combinator library.

  Date: Sep 20, 2012
  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module SimpleParse where

import Control.Monad(MonadPlus(..))
import Data.Char (isSpace)

newtype Parser a = Parser (String -> [(a, String)])
parse (Parser p) = p

parse' p s = [ result | (result,rest) <- parse p s, null rest ]



item :: Parser Char     -- String -> [(Char,String)]
item = Parser item'
  where item' "" = [ ]
        item' (x : xs) = [(x,xs)]

reject :: Parser a
reject = Parser $ \ _ -> []

eof :: Parser ()
eof = Parser  eof'
  where eof' "" = [((),[])]
        eof' _  = []

parseEof p = parse $ p >>> eof >>= return . fst


(>>>) :: Parser a -> Parser b -> Parser (a,b)
p >>> q = Parser $ \ s -> [ ((a,b), cs) | (a, cs1) <- parse p s
                                        , (b, cs)  <- parse q cs1]

instance Monad Parser where
   p >>= q  = Parser$ \cs -> [(v2, cs2) |
                              (v1, cs1) <- parse p cs,
                              (v2, cs2) <- parse (q v1) cs1]

   return v = Parser$ \cs -> [(v, cs)]


(<++) :: Parser a -> Parser a -> Parser a
p <++ q = Parser (\cs -> case parse p cs of
                     []	 -> parse q cs
                     res -> res)



char :: Char -> Parser Char
char e = do c <- item
            if e == c
              then return c
              else reject

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- item
               if p c
                 then return c
                 else reject

string :: String -> Parser String
string "" = return ""
string (c:cs) = do char c
                   string cs
                   return (c:cs)

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser$ \cs -> parse p cs ++ parse q cs

instance MonadPlus Parser where
  p `mplus` q = p <|> q
  mzero       = reject


many :: Parser a -> Parser [a]
many p = do v <- p
            vs <- many p
            return (v:vs)
         <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

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

symbol           = token . string
schar            = token . char
