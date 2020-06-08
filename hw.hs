import Data.Char
import System.IO
import System.Environment (getArgs)



data Ninja = Ninja {name :: String, 
                    country :: Char,
                    status :: String, 
                    exam1 :: Float,
                    exam2 :: Float, 
                    ability1 :: String,
                    ability2 :: String,
                    r :: Int,
                    score :: Float}
                    deriving (Eq)

instance Show Ninja where
        show (Ninja name _ status _ _ _ _ round score ) = name ++ ", Score: " ++ show score ++ ", Status: "
         ++ status ++ ", Round: " ++ show round

instance Ord Ninja where
        compare (Ninja _ _ _ _ _ _ _ r1 s1) (Ninja _ _ _ _ _ _ _ r2 s2) = if r1 == r2 then compare s2 s1 else compare r1 r2



input :: String -> IO String
input prompt = do
        putStr prompt
        hFlush stdout
        r <- getLine
        return r

inputUntilValid :: String -> [String] -> IO String
inputUntilValid prompt validInputs = do
        result <- input prompt
        let lowered_result = map toLower result
        if lowered_result `notElem` validInputs then do
                putStrLn "Input is not valid. Try again."
                inputUntilValid prompt validInputs
        else
                return lowered_result

when :: (Applicative f) => Bool -> f () -> f ()
when p s  = if p then s else pure ()

getItem ::  String -> [a] -> a
getItem countryCode [e, f, l, n, w] = case countryCode of
         "e" -> e 
         "f" -> f 
         "l" -> l 
         "n" -> n 
         "w" -> w 
         _ -> error "No such item"

hasAJourneyman :: String -> [Bool] -> Bool
hasAJourneyman = getItem

getCountry :: String -> [[Ninja]] -> [Ninja]
getCountry = getItem

getCountryName :: String -> String
getCountryName i = getItem i ["Earth", "Fire", "Lightning", "Wind", "Water"]


getithNinja :: Int -> [[Ninja]] -> [Bool] -> IO Ninja
getithNinja i ninjas journeymanL = do
        let ordinalString = if i == 1 then "first" else "second"
        name_ <- input ("Enter the name of the " ++ ordinalString ++ " ninja: ")
        country <- inputUntilValid  ("Enter the country code of the " ++ ordinalString ++  " ninja: ") ["e","f","l","n","w"]
        let countryHasJourneyman = hasAJourneyman country journeymanL
        let ninja = filter (\ninja -> (name ninja) == name_) (getCountry country ninjas)
        if null ninja then do
                putStrLn "Please enter a valid name-country pair."
                getithNinja i ninjas journeymanL
        else
                if countryHasJourneyman then do
                        putStrLn $ getCountryName country ++ " country already has a journeyman. Please try another country."
                        getithNinja i ninjas journeymanL
                else
                        return $ head ninja

getithCountrysNinja :: Int -> [[Ninja]] -> [Bool] -> IO Ninja
getithCountrysNinja i ninjas journeymanL = do
        let ordinalString = if i == 1 then "first" else "second"
        countryCode <- inputUntilValid  ("Enter the " ++ ordinalString ++  " country code: ") ["e","f","l","n","w"]
        let countryHasJourneyman = hasAJourneyman countryCode journeymanL
        let country = getCountry countryCode ninjas
        if countryHasJourneyman then do
                putStrLn $ getCountryName countryCode ++ " country already has a journeyman. Please try another country."
                getithCountrysNinja i ninjas journeymanL
        else
                if null country then do
                        putStrLn $ getCountryName countryCode ++ " country's ninjas are eliminated. Please try another country."
                        getithCountrysNinja i ninjas journeymanL
                else
                        return $ head country

remove :: Ninja -> [Ninja] -> [Ninja]
remove removed = filter (/= removed)

add :: Ninja -> [Ninja] -> [Ninja]
add added []     = [added]
add added all@(n:ns) = if added < n then added:all else n: add added ns
                
update :: Ninja -> [Ninja] -> [Ninja]
update updated li = updatedList
        where
                listWithoutUpdated = remove updated li
                status = if (r updated) < 2 then "Junior" else "Journeyman"
                toBeAdded = updated {status = status, r = succ (r updated), score = (score updated)+10 }
                updatedList = add toBeAdded listWithoutUpdated
                

updateNinja :: (Ninja -> [Ninja] -> [Ninja]) -> Ninja -> [[Ninja]] -> [[Ninja]]
updateNinja func nin [e, f, l, n, w] = case (country nin) of
        'e' -> [(func nin e),f,l,n,w]
        'f' -> [e,(func nin f),l,n,w]
        'l' -> [e,f,(func nin l),n,w]
        'n' -> [e,f,l,(func nin n),w]
        'w' -> [e,f,l,n,(func nin w)]
        _   -> error ""

fight :: Ninja -> Ninja -> (Ninja, Ninja)
fight n1 n2 = if totalScore1 < totalScore2 then (n1, n2) else (n2, n1)
        where
                totalScore1 = score n1 + (impact (ability1 n1) + impact (ability2 n1)) * fromIntegral (fromEnum (score n1 == score n2))
                totalScore2 = score n2 + (impact (ability1 n2) + impact (ability2 n2)) * fromIntegral (fromEnum (score n1 == score n2))

updateJourneymanList :: [Bool] -> Int -> [Bool]
updateJourneymanList journeymanL n
        | n < 0     = journeymanL
        | otherwise = take n journeymanL ++ [True] ++ drop (n + 1) journeymanL

makeARound :: Ninja -> Ninja -> [[Ninja]] -> [Bool] -> IO ()
makeARound ninja1 ninja2 ninjas journeymanL = do
        let (looser, winner) = fight ninja1 ninja2
        let status = if (r winner) < 2 then "Junior" else "Journeyman"
        let updatedWinner = winner {status = status, r = succ (r winner), score = (score winner)+10 }
        putStrLn $ "Winner: " ++ show updatedWinner
        let countryCode = [country winner]
        let updateIndex = if status == "Journeyman" then getItem countryCode [0..4] else -1
        let updatedJourneymanL = updateJourneymanList journeymanL updateIndex
        let ninjas' = updateNinja remove (looser) ninjas                                                                                                              
        let ninjas'' = updateNinja update (winner) ninjas'
        showUIList False ninjas'' updatedJourneymanL
        

countryNinjaInfo :: [[Ninja]] -> [Bool] -> IO()
countryNinjaInfo ninjas journeymanL = do
        countryCode <- inputUntilValid "Enter the country code: " ["e", "f", "l", "n", "w"]
        let countryHasjourneyman = hasAJourneyman countryCode journeymanL
        let countryName = getCountryName countryCode
        let country = getCountry countryCode ninjas
        if null country then do
                putStrLn $ countryName ++ " country's ninjas are eliminated."
        else 
                mapM_ print country
        when (countryHasjourneyman)(
                putStrLn $ countryName ++ " country cannot be included in a fight.")
        showUIList False ninjas journeymanL


allNinjaInfo :: [[Ninja]] -> [Bool] -> IO()
allNinjaInfo ninjas journeymanL = do
        let allCountries = (foldl (++) [] ninjas)
        mapM_ print (sort allCountries)
        showUIList False ninjas journeymanL



ninjaRound :: [[Ninja]] -> [Bool] -> IO()
ninjaRound ninjas journeymanL = do
        ninja1 <- getithNinja 1 ninjas journeymanL
        ninja2 <- getithNinja 2 ninjas journeymanL
        if country ninja1 == country ninja2 then do
                if ninja1 == ninja2 then do
                        putStrLn $ name ninja1 ++ " refuses to punch itself. Try again."
                else do
                        putStrLn "Please select ninjas from different countries."
                ninjaRound ninjas journeymanL
        else do
                makeARound ninja1 ninja2 ninjas journeymanL
        

countryRound :: [[Ninja]] -> [Bool] -> IO()
countryRound ninjas journeymanL = do
        ninja1 <- getithCountrysNinja 1 ninjas journeymanL
        ninja2 <- getithCountrysNinja 2 ninjas journeymanL
        if country ninja1 == country ninja2 then do
                if country ninja1 == 'f' then do
                        putStrLn "You can't fight fire with fire :)"
                else
                        putStrLn "Please select two distinct countries."
                countryRound ninjas journeymanL
        else do
                makeARound ninja1 ninja2 ninjas journeymanL

journeymanList :: [[Ninja]] -> [Bool] -> IO()
journeymanList ninjas journeymanL = do
        let unsortedjourney = filter(\ninja -> (status ninja) == "Journeyman") (foldl (++) [] ninjas)
        print (sort unsortedjourney)
                
getAction :: Bool -> IO String
getAction show_help = do
        when show_help (
                putStrLn "a) View a Country's Ninja Information\n\
                         \b) View All Countries' Ninja Information\n\
                         \c) Make a Round Between Ninjas\n\
                         \d) Make a Round Between Countries\n\
                         \e) Exit\n")

        action <- input "Enter the action: "
        return action


showUIList :: Bool -> [[Ninja]] -> [Bool] -> IO()
showUIList show_help ninjas hasAJourneyMan = do 
        
        action <- getAction show_help
        let lowered_action = map toLower action
        case lowered_action of
                "a" -> countryNinjaInfo ninjas  hasAJourneyMan
                "b" -> allNinjaInfo ninjas hasAJourneyMan
                "c" -> ninjaRound ninjas hasAJourneyMan
                "d" -> countryRound ninjas hasAJourneyMan
                "e" -> journeymanList ninjas hasAJourneyMan
                _   ->  do 
                        putStrLn "Action is not on the list. Try again."
                        showUIList True ninjas hasAJourneyMan
        
filterNinjas :: Char -> [Ninja] -> [Ninja]
filterNinjas countryCode = filter (\ninja -> country ninja == countryCode)

sort :: [Ninja] -> [Ninja]
sort xs = foldr ins [] xs
        where 
                ins :: Ninja -> [Ninja] -> [Ninja]
                ins n [] = [n]
                ins n xs@(x':xs')
                        | n <= x'   = n : xs
                        | otherwise = x' : ins n xs'




impact :: String -> Float
impact ability = case ability of 
        "Clone"     -> 20
        "Hit"       -> 10
        "Lightning" -> 50
        "Vision"    -> 30
        "Sand"      -> 50
        "Fire"      -> 40
        "Water"     -> 30
        "Blade"     -> 20
        "Summon"    -> 50
        "Storm"     -> 10
        "Rock"      -> 20
        _           -> error $ "No such ability: " ++ ability

calculateScore :: Float -> Float -> String -> String -> Float
calculateScore e1 e2 a1 a2 =  0.5 * e1 + 0.3 * e2 + abi1 + abi2
        where 
                abi1 = impact a1
                abi2 = impact a2


initNinja :: [String] -> Float -> Float -> Ninja
initNinja params s1 s2 = Ninja (params !! 0) countryChar "Junior" s1 s2 (params !! 4) (params !! 5) 0 score
        where
                score = calculateScore s1 s2  (params !! 4) (params !! 5)
                countryChar = case (params !! 1) of
                        "Fire"      -> 'f'
                        "Lightning" -> 'l'
                        "Water"     -> 'w'
                        "Wind"      -> 'n'
                        "Earth"     -> 'e'
                        _           -> error $ "No such country: " ++  (params !! 1)


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

main :: IO ()
main = do
        args <- getArgs 
        file <- openFile (head args) ReadMode
        all_ninjas <- readNinjas file []
        -- maybe we can strictly evaluate here to catch the errors early
        -- such as invalid country names or abilities.
        let groupedNinjas = zipWith filterNinjas ['e', 'f', 'l', 'n', 'w'] (repeat all_ninjas)
        let sortedNinjas = map sort groupedNinjas
        let hasAJourneyman = [False, False, False, False, False]
        showUIList True sortedNinjas hasAJourneyman
        