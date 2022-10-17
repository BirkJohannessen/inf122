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
fromLeftAndRight f = ((.) f fromLeft,(.) f fromRight) 

-- a -> Either a b