import Data.Monoid (Sum(..))

data NL a = Atom a | List [NL a]

instance Foldable NL where
    -- foldMap :: Monoid m => (a -> m) -> NL a -> m
    foldMap f nl = case nl of
        (Atom a) -> f a
        (List ns) -> foldMap (foldMap f) ns

atomcount :: NL a -> Int
atomcount nl = getSum (foldMap (\_ -> Sum 1) nl)

flatten :: NL a -> [a]
flatten nl = foldMap (\x -> [x]) nl

ggf :: Sheep -> Maybe Sheep 
ggf sh = (return sh) >>= (\s -> 
            mother s >>= (\m -> 
            father m >>= (\gf -> 
            father gf)))

lenProgram :: IO Int
lenProgram = let x = length [1, 2, 3] in
    (putStrLn ("Length is " ++ show x) >> return x)

fancyProgram :: IO ()
fancyProgram = putStrLn "What's your name?" >>
                getLine >>= (\inpStr ->
                if inpStr == "Haskell"
                    then putStrLn "You rock!" >>
                         return () >>
                         putStrLn "Really!!"
                    else putStrLn ("Hello " ++ inpStr) >>
                    putStrLn "That's all!")
