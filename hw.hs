import Data.List
import Data.Char
import System.IO
import qualified Data.Text as T
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

fire :: [Ninja] 
fire = [
        Ninja {name = "Naruto", country='F', status = "Junior", exam1 = 40, exam2 = 45, ability1 = "Clone", ability2 = "Summon" , r = 0, score = 10.0},
        Ninja {name = "Sasuke", country='F', status = "Junior", exam1 = 50, exam2 = 60, ability1 = "Lightning", ability2 = "Fire", r = 0 , score = 10.0},
        Ninja {name = "Neiji", country='F', status = "Junior", exam1 = 40, exam2 = 75, ability1 = "Vision", ability2 = "Hit", r = 0 , score = 10.0}]
lightning :: [Ninja] 
lightning = [
        Ninja {name = "Sana", country='L', status = "Junior", exam1 = 55, exam2 = 65, ability1 = "Lightning", ability2 = "Hit", r = 0 , score = 10.0},
        Ninja {name = "Aimi", country='L', status = "Junior", exam1 = 60, exam2 = 65, ability1 = "Blade", ability2 = "Rock", r = 0 , score = 10.0},
        Ninja {name = "Kira", country='L', status = "Junior", exam1 = 40, exam2 = 60, ability1 = "Storm", ability2 = "Rock", r = 0 , score = 10.0}]

water :: [Ninja] 
water = [Ninja {name = "Midare", country='W', status = "Junior", exam1 = 35, exam2 = 45, ability1 = "Hit", ability2 = "Water", r = 0 , score = 10.0},
        Ninja {name = "Suiu", country='W', status = "Junior", exam1 = 45, exam2 = 55, ability1 = "Water", ability2 = "Blade", r = 0 , score = 10.0},
        Ninja {name = "Samidare", country='W', status = "Junior", exam1 = 30, exam2 = 55, ability1 = "Water", ability2 = "Hit", r = 0, score = 10.0 }]

wind :: [Ninja] 
wind = [Ninja {name = "Gaara", country='N', status = "Junior", exam1 = 55, exam2 = 80, ability1 = "Vision", ability2 = "Sand", r = 0, score = 10.0},
        Ninja {name = "Temari", country='N', status = "Junior", exam1 = 40, exam2 = 60, ability1 = "Hit", ability2 = "Blade" , r = 0, score = 10.0},
        Ninja {name = "Kankuro", country='N', status = "Junior", exam1 = 30, exam2 = 50, ability1 = "Hit", ability2 = "Storm" , r = 0, score = 10.0}]

earth :: [Ninja] 
earth = [Ninja {name = "Haruki", country='E', status = "Junior", exam1 = 50, exam2 = 64, ability1 = "Blade", ability2 = "Rock", r = 0 , score = 10.0},
        Ninja {name = "Miyazaki", country='E', status = "Junior", exam1 = 45, exam2 = 55, ability1 = "Rock", ability2 = "Hit" , r = 0, score = 10.0},
        Ninja {name = "Hiroshi", country='E', status = "Junior", exam1 = 40, exam2 = 60, ability1 = "Storm", ability2 = "Rock", r = 0 , score = 10.0}]

fireNinjaInit :: [String] -> Float -> Float -> Ninja
fireNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='F', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5), r = 0, score = (calculateScore s1 s2 (ninja !! 4) (ninja !! 5))} in nin

lightningNinjaInit :: [String] -> Float -> Float -> Ninja
lightningNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='L', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5), r = 0, score = (calculateScore s1 s2 (ninja !! 4) (ninja !! 5))  } in nin

waterNinjaInit :: [String] -> Float -> Float -> Ninja
waterNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='W', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5), r = 0 , score = (calculateScore s1 s2 (ninja !! 4) (ninja !! 5)) } in nin

windNinjaInit :: [String] -> Float -> Float -> Ninja
windNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='N', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5), r = 0 , score = (calculateScore s1 s2 (ninja !! 4) (ninja !! 5)) } in nin

earthNinjaInit :: [String] -> Float -> Float -> Ninja
earthNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='E', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5), r = 0 , score = (calculateScore s1 s2 (ninja !! 4) (ninja !! 5))} in nin


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
                _           -> error "error"


calculateScore :: Float -> Float -> String -> String -> Float
calculateScore e1 e2 a1 a2 = let
        abi1 = abilityScore a1
        abi2 = abilityScore a2
        scr = 0.5 * e1 + 0.3 * e2 + abi1 + abi2
        in scr





























placeToCountry :: [String] -> Float -> Float -> Ninja
placeToCountry ninja s1 s2 = case (ninja !! 1) of 
                "Fire"      -> fireNinjaInit ninja s1 s2
                "Lightning" -> fireNinjaInit ninja s1 s2
                "Water"     -> fireNinjaInit ninja s1 s2
                "Wind"      -> fireNinjaInit ninja s1 s2
                "Earth"     -> fireNinjaInit ninja s1 s2
                _           -> error "no ninja"
                 


readNinja :: Handle -> IO() --FIXIT: bunun void func olması lazım ama void yazpınca file okuması hata veriyor
readNinja file = do
        end <- hIsEOF file
        if not end then do
                line <- hGetLine file
                let placeholder = T.splitOn (T.pack " ") (T.pack line) --okunan satırı kelimelerine ayırıyor hangi countryden geldiğini bulmamız için
                let ninja = map T.unpack placeholder --kelimelerine ayırdığımız zaman tipi T.text tarzı bir şey olmuştu burda tipini stringe çevirdim
                let score1 = read(ninja !! 2) :: Float
                let score2 = read(ninja !! 3) :: Float 
                let ph = placeToCountry ninja score1 score2
                readNinja file
                else do
                        print ""
     
prompt :: String -> IO String
prompt prmt = do
        putStrLn "a) Wiew a Country's Ninja Information"
        putStrLn "b) Wiew All Countries' Ninja Information"
        putStrLn "c) Make a Round Between Ninjas"
        putStrLn "d) Make a Round Between Countries"
        putStrLn "e) Exit"
        putStrLn prmt
        action <- getLine
        return action

        
main :: IO ()
main = do
        args <- getArgs 
        file <- openFile (head args) ReadMode
        readNinja file
        --showUIList "correct"
        print "end"
        

        