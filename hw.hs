import Data.List
import Data.Char
import System.IO
import System.Environment (getArgs)
import Data.Typeable




data Ninja = Ninja {name :: String, 
                    country :: Char,
                    status :: String, 
                    exam1 :: Float,
                    exam2 :: Float, 
                    ability1 :: String,
                    ability2 :: String,
                    r :: Int,
                    score :: Float}
                    deriving (Ord, Eq)



instance Show Ninja where
        show (Ninja name _ status _ _ _ _ round score ) = show name ++ ", Score: " ++ show score ++ ", Status: " ++ show status ++ ", Round: " ++ show round ++ "\n"


abilityScore :: String -> Float
abilityScore ability = case ability of 
                "Clone"     -> 20.0
                "Hit"       -> 10.0
                "Lightning" -> 50.0
                "Vision"    -> 30.0
                "Sand"      -> 50.0
                "Fire"      -> 40.0
                "Water"     -> 30.0
                "Blade"     -> 20.0
                "Summon"    -> 50.0
                "Storm"     -> 10.0
                "Rock"      -> 20.0
                _           -> error "No such ability"


calculateScore :: Float -> Float -> String -> String -> Float
calculateScore e1 e2 a1 a2 =  0.5 * e1 + 0.3 * e2 + abi1 + abi2
    where 
        abi1 = abilityScore a1
        abi2 = abilityScore a2


                
initNinja :: [String] -> Float -> Float -> Ninja
initNinja params s1 s2 = Ninja (params !! 0) countryChar "junior" s1 s2 (params !! 4) (params !! 5) 0 score
    where
        score = calculateScore s1 s2  (params !! 4) (params !! 5)
        countryChar = case (params !! 1) of
                "Fire"      -> 'f'
                "Lightning" -> 'l'
                "Water"     -> 'w'
                "Wind"      -> 'n'
                "Earth"     -> 'e'
                _           -> error "No such country"
                


readNinjas :: Handle -> [Ninja] ->  IO [Ninja]
readNinjas file ninjas = do
        end <- hIsEOF file
        if not end then do
                line <- hGetLine file
                let params = words line
                let score1 = read(params !! 2) :: Float
                let score2 = read(params !! 3) :: Float 

                let ninja = initNinja params score1 score2

                readNinjas file (ninja:ninjas)
        else do
                return ninjas
     
prompt :: String -> IO String
prompt prmpt = do
        putStrLn "a) Wiew a Country's Ninja Information"
        putStrLn "b) Wiew All Countries' Ninja Information"
        putStrLn "c) Make a Round Between Ninjas"
        putStrLn "d) Make a Round Between Countries"
        putStrLn "e) Exit"
        putStrLn prmpt
        action <- getLine
        return action

input :: String -> IO String
input prompt = do
        hSetBuffering stdout NoBuffering -- maybe replace with a hFlush stdout?
        putStr prompt
        r <- getLine
        return r



countryNinjaInfo :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
countryNinjaInfo f l w n e = do
        choice <- input "Enter the country code: "
        let lowered_choice = map toLower choice
        case lowered_choice of
                "e" -> print e
                "f" -> print f
                "l" -> print l
                "w" -> print w
                "n" -> print n
                ""  -> error "enter a country"
                _   -> error "No such country"


main :: IO ()
main = do
        args <- getArgs 
        file <- openFile (head args) ReadMode
        all_ninjas <- readNinjas file []
        -- print $ country $ head all_ninjas
        let sortedNinjas = sortBy (\n1 n2 -> compare (country n1) (country n2)) all_ninjas
        let [earth, fire, lightning, wind, water] = groupBy (\n1 n2 -> (country n1) == (country n2)) sortedNinjas
        countryNinjaInfo fire lightning water wind earth
        -- print wind
        print "end"


        