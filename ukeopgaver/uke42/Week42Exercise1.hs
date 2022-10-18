{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Week42Exercise1 where
import Data.Either

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' _ bc (Right b) = bc b
either' ac _ (Left a) = ac a

toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd f = (fst . f, snd . f)

pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f d a = (f a, d a)

fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (\a -> f (Left a), \ b -> f (Right b))

-- . :: forall b c a. (b -> c) -> (a -> b) -> a -> c
--                       f          left
--                    either ab -> c||a -> Either a b   -> a -> c 
-- a -> Either a b