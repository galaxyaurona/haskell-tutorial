import Data.Char
import Control.Applicative
newtype Parser a =  P(String -> [(a, String)])

-- must defines instance monad parsers
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> parse (f v) out)
instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)
-- define Option monads
instance Alternative Parser where
    --empty :: Parser a
    empty = P (\inp -> [])
   
    -- (+++) :: Parser a -> Parser a -> Parser a
    p <|> q = P(\inp -> case parse p inp of 
                        []          -> parse q inp
                        [(v,out)]   -> [(v,out)] )

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item  = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])
    --item (x:xs) = [(x,xs)]


return' :: a -> Parser a
return' v  = P(\inp -> [(v,inp)]) 

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return' x else empty

digit :: Parser Char 
digit = sat isDigit


isSpecificChar :: Char -> Parser Char
isSpecificChar x = sat (x ==)

many' :: Parser a -> Parser [a]
many' p = many1' p <|> return' []

many1' :: Parser a -> Parser [a]
many1' p = do   x  <- p
                xs     <- many' p
                return' (x:xs)
-- recognize a string
string :: String -> Parser String
string []       = return' []
string (x:xs)   = do isSpecificChar x
                     string xs
                     return' (x:xs)

main = do
    print(" Lecture 8")