-----------------------------------------------------------------------------
-- |
-- Module      :  Harp.Match
-- Copyright   :  (c) Niklas Broberg 2004,
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions that simulate the behavior of regular patterns
-- using a Match monad for parsing lists.
-----------------------------------------------------------------------------

module Harp.Match (
        Match           -- Match e a
        , runMatch      -- Match e a -> [e] -> Maybe a
        , baseMatch     -- (a -> Maybe b) -> Match a (a, b)
        , manyMatch     -- Match e a -> Match e [a]
        , gManyMatch    -- Match e a -> Match e [a]
        , foldComp      -- [[a] -> [a]] -> ([a] -> [a])
        , unzip0, unzip1, unzip2, unzip3, unzip4, unzip5, unzip6, unzip7
        , (+++)
        ) where

import Data.List (unzip3, unzip4, unzip5, unzip6, unzip7)

--------------------------------------------------------------
-- | The Match monad

newtype Match e a = Match ([e] -> [(a, [e])])

(+++) :: Match e a -> Match e a -> Match e a
(Match f) +++ (Match g) = Match (\es -> let aes1 = f es
                                            aes2 = g es
                                         in aes1 ++ aes2)

instance Monad (Match e) where
  return x = Match (\es -> [(x, es)])

  (Match f) >>= k = Match (\es -> let aes = f es
                                   in concatMap help aes)
    where help (a, es) = let Match g = k a
                          in g es


mfail :: Match e a
mfail = Match $ \_ -> []

runM :: Match e a -> [e] -> [a]
runM (Match f) es = let aes = f es
                     in map fst $ filter (null . snd) aes

getElement :: Match e e
getElement = Match $ \es -> case es of
                             [] -> []
                             (x:xs) -> [(x,x:xs)]

discard :: Match e ()
discard = Match $ \es -> case es of
                           [] -> []
                           (_:xs) -> [((), xs)]


runMatch :: Match e a -> [e] -> Maybe a
runMatch m es = case runM m es of
                 []     -> Nothing
                 (a:_) -> Just a

baseMatch :: (a -> Maybe b) -> Match a (a, b)
baseMatch f = do e <- getElement
                 case f e of
                  Nothing -> mfail
                  Just b -> do discard
                               return (e, b)

gManyMatch :: Match e a -> Match e [a]
gManyMatch m = (do a <- m
                   as <- gManyMatch m
                   return (a:as))
              +++ (return [])

manyMatch :: Match e a -> Match e [a]
manyMatch m = (return []) +++
                (do a <- m
                    as <- manyMatch m
                    return (a:as))


foldComp :: [[a] -> [a]] -> ([a] -> [a])
foldComp = foldl (.) id


unzip0 :: [()] -> ()
unzip0 = const ()

unzip1 :: [a] -> [a]
unzip1 = id

unzip2 :: [(a,b)] -> ([a],[b])
unzip2 = unzip

{-
data M e a = Element (e -> M e a)
           | Fail
           | Return a (M e a)

instance Monad (M e) where
  return x = Return x Fail

  (Element f)  >>= k = Element (\e -> f e >>= k)
  Fail         >>= k = Fail
  (Return x m) >>= k = k x ++++ (m >>= k)


infix 5 ++++

(++++) :: M e a -> M e a -> M e a
Fail       ++++ n          = n
m          ++++ Fail       = m
Return x m ++++ n          = Return x (m ++++ n)
m          ++++ Return x n = Return x (m ++++ n)
Element f  ++++ Element g  = Element (\e -> f e ++++ g e)


runM :: M e a -> [e] -> [a]
runM (Element f)  (e:es) = runM (f e) es
runM (Element _)  []     = []
runM Fail         _      = []
runM (Return x m) []     = x : runM m []
runM (Return _ m) es     = runM m es

-- the continuation trick
newtype Match e a = Match ()

instance Monad (Match e) where
  return x = Match (\k -> k x)

  (Match f) >>= k = Match (\h -> f (\a -> let Match g = k a
                                           in g h))

runMatch :: Match e a -> [e] -> [a]
runMatch (Match f) = runM (f return)

mfail :: Match e a
mfail = Match $ \_ -> Fail
-}
