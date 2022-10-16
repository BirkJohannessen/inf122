module Week42Exercise1 where
import Data.Either

--fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
--fromLeftAndRight (Right b) = uncurry b


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' _ bc (Right b) = bc b
either' ac _ (Left a) = ac a

toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd (a -> (b,c)) = (a -> b, a -> c) 