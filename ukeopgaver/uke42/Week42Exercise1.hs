module Week42Exercise1 where
import Data.Either 


fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight = (\y -> fromLeft',\y -> fromRight')

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' _ bc (Right b) = bc b
either' ac _ (Left a) = ac a

toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd f = (fst . f, snd . f)

pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f d a = (f a, d a)


fromRight' :: Either a b -> b
fromRight' (Left _)  = error "Data.Either.Combinators.fromRight' encountered a value of form 'Left _', consider using Data.Either.Combinators.fromRight with a default value." -- yuck
fromRight' (Right x) = x

fromLeft' :: Either a b -> a
fromLeft' (Right _) = error "Data.Either.Combinators.fromLeft' encountered a value of form 'Right _', consider using Data.Either.Combinators.fromLeft with a default value." -- yuck
fromLeft' (Left x)  = x
