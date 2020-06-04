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
        show (Ninja name _ status _ _ _ _ round score ) = show name ++ ", Score: " ++ show score ++ ", Status: "
         ++ show status ++ ", Round: " ++ show round ++ "\n"


abilityScore :: String -> Float
abilityScore ability = case ability of 
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
                _           -> error "No such ability"


calculateScore :: Float -> Float -> String -> String -> Float
calculateScore e1 e2 a1 a2 =  0.5 * e1 + 0.3 * e2 + abi1 + abi2
    where 
        abi1 = abilityScore a1
        abi2 = abilityScore a2


initNinja :: [String] -> Float -> Float -> Ninja
initNinja params s1 s2 = Ninja (params !! 0) countryChar "Junior" s1 s2 (params !! 4) (params !! 5) 0 scr
    where
        scr = calculateScore s1 s2  (params !! 4) (params !! 5)
        countryChar = case (params !! 1) of
                "Fire"      -> 'f'
                "Lightning" -> 'l'
                "Water"     -> 'w'
                "Wind"      -> 'n'
                "Earth"     -> 'e'
                _           -> error "No such country"  -- maybe display the country here?
                


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
     
prompt :: Bool -> IO String
prompt valid = do
        putStrLn "a) View a Country's Ninja Information"
        putStrLn "b) View All Countries' Ninja Information"
        putStrLn "c) Make a Round Between Ninjas"
        putStrLn "d) Make a Round Between Countries"
        putStrLn "e) Exit"
        hSetBuffering stdout NoBuffering
        if valid then do 
                putStr "Enter the action: "
        else 
                putStr "Action is not on the list. Please enter a valid action: "
        action <- getLine
        return action


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


ninjaInfoSort :: [Ninja] -> [Ninja]
ninjaInfoSort array = sortBy (\n1 n2 -> compare (r n1) (r n2)) $ sortBy (\n1 n2 -> compare (score n2) (score n1)) array
                                


countryNinjaInfo :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
countryNinjaInfo f l w n e = do
        countryCode <- inputUntilValid "Enter the country code: " ["e", "f", "l", "w", "n"]
        print $ ninjaInfoSort $ convertCountry countryCode f l w n e
        showUIList True f l w n e


allNinjaInfo :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
allNinjaInfo f l w n e = do
        let allCountries = f ++ l ++ w ++ n ++ e
        print (ninjaInfoSort allCountries)
        showUIList True f l w n e



convertCountry ::  String -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja]
convertCountry countryCode f l w n e  = 
        case countryCode of
        "f" -> f
        "l" -> l
        "w" -> w
        "n" -> n
        "e" -> e
        _ -> error " "


listDelete :: Ninja -> [Ninja] -> [Ninja]
listDelete deletedNinja = filter (\ninja -> ninja /= deletedNinja)

                
listUpdate :: Ninja -> [Ninja] -> [Ninja]
listUpdate updatedNinja c = updatedList
        where
                placeholder = filter (\ninja -> ninja /= updatedNinja) c
                stat = if (r updatedNinja) < 2 then "Junior" else "Journeyman"
                updatedList = placeholder ++ [
                        Ninja {name = (name updatedNinja), country = (country updatedNinja), status = stat,
                                        exam1 = (exam1 updatedNinja), exam2 = (exam2 updatedNinja), 
                                        ability1 = (ability1 updatedNinja), ability2 = (ability2 updatedNinja), 
                                        r = (r updatedNinja)+1, score = (score updatedNinja)+10 }]
                

updateNinja :: (Ninja -> [Ninja] -> [Ninja]) -> Ninja -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [[[Ninja]]]
updateNinja func nin f l w n e = case (country nin) of
        'f' -> return [(func nin f),l,w,n,e]
        'l' -> return [f,(func nin l),w,n,e]
        'w' -> return [f,l,(func nin w),n,e]
        'n' -> return [f,l,w,(func nin n),e]
        'e' -> return [f,l,w,n,(func nin e)]
        _   -> error ""


getithNinja :: Int -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO Ninja
getithNinja i f l w n e = do
        let ordinalString = if i == 1 then "first" else "second"
        name_ <- input ("Enter the name of the " ++ ordinalString ++ " ninja: ")
        country <- inputUntilValid  ("Enter the country code of the " ++ ordinalString ++  " ninja: ") ["f","l","w","n","e"]
        let ninja = filter (\ninja -> (name ninja) == name_) (convertCountry country f l w n e)
        if null ninja then do
                putStrLn "Please enter a valid name-country pair."
                getithNinja i f l w n e
        else
                return $ head ninja


makeARound :: Ninja -> Ninja -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO ()
makeARound ninja1 ninja2 f l w n e = do
        let [looser, winner] = sortBy(\n1 n2 -> compare (score n1) (score n2)) [ninja1, ninja2]
        putStrLn $ "Winner: " ++ show winner

        let [[f', l', w', n', e']] = updateNinja listDelete (looser) f l w n e                                                                                                              
        let [[f'', l'', w'', n'', e'']] = updateNinja listUpdate (winner) f' l' w' n' e'
        showUIList True f'' l'' w'' n'' e''


ninjaRound :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
ninjaRound f l w n e = do
        ninja1 <- getithNinja 1 f l w n e
        ninja2 <- getithNinja 2 f l w n e
        makeARound ninja1 ninja2 f l w n e
        

journeymanList :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
journeymanList f l w n e = do
        let unsortedjourney = filter(\ninja -> (status ninja) == "Journeyman") (f++l++w++n++e)
        print (ninjaInfoSort unsortedjourney)


countryRound :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
countryRound f l w n e = do
        country1 <- inputUntilValid "Enter the first country code: " ["f","l","w","n","e"]
        country2 <- inputUntilValid "Enter the second country code: " ["f","l","w","n","e"]
        if country1 == country2 then do
                putStrLn "Please select two distinct countries."
                countryRound f l w n e
        else do
        
                let ninja1 = head $ ninjaInfoSort (convertCountry country1 f l w n e)
                let ninja2 = head $ ninjaInfoSort (convertCountry country2 f l w n e)
                makeARound ninja1 ninja2 f l w n e
                

showUIList :: Bool -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
showUIList state f l w n e = do 
        
        action <- prompt state
        
        let lowered_action = map toLower action
        case lowered_action of
                "a" -> countryNinjaInfo f l w n e 
                "b" -> allNinjaInfo f l w n e
                "c" -> ninjaRound f l w n e
                "d" -> countryRound f l w n e
                "e" -> journeymanList f l w n e
                _   -> showUIList False f l w n e


main :: IO ()
main = do
        args <- getArgs 
        file <- openFile (head args) ReadMode
        all_ninjas <- readNinjas file []
        let sortedNinjas = sortBy (\n1 n2 -> compare (country n1) (country n2)) all_ninjas
        let [earth, fire, lightning, water, wind] = groupBy (\n1 n2 -> (country n1) == (country n2)) sortedNinjas
        showUIList True fire lightning wind water earth
        

        
