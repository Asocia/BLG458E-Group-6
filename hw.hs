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
        show (Ninja name _ status _ _ _ _ round score ) = name ++ ", Score: " ++ show score ++ ", Status: "
         ++ status ++ ", Round: " ++ show round


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
              


-- Actually, the ordering of the ninjas in their country list will be also the same, meaning that making 
-- some rounds will affect the ordering of the ninjas in the country lists. Therefore, you will not need 
-- to reorder the ninjas in the country list for the purpose of viewing the ninjas. You will just iterate
-- over the related country list.

countryNinjaInfo :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
countryNinjaInfo e f l n w = do
        countryCode <- inputUntilValid "Enter the country code: " ["e", "f", "l", "n", "w"]
        mapM_ print $ ninjaInfoSort $ convertCountry countryCode e f l n w
        showUIList True e f l n w


allNinjaInfo :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
allNinjaInfo e f l n w = do
        let allCountries = e ++ f ++ l ++ n ++ w
        mapM_ print (ninjaInfoSort allCountries)
        showUIList True e f l n w



convertCountry ::  String -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja]
convertCountry countryCode e f l n w  = 
        case countryCode of
        "e" -> e 
        "f" -> f 
        "l" -> l 
        "n" -> n 
        "w" -> w 
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
updateNinja func nin e f l n w = case (country nin) of
        'e' -> return [(func nin e),f,l,n,w]
        'f' -> return [e,(func nin f),l,n,w]
        'l' -> return [e,f,(func nin l),n,w]
        'n' -> return [e,f,l,(func nin n),w]
        'w' -> return [e,f,l,n,(func nin w)]
        _   -> error ""


getithNinja :: Int -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO Ninja
getithNinja i e f l n w = do
        let ordinalString = if i == 1 then "first" else "second"
        name_ <- input ("Enter the name of the " ++ ordinalString ++ " ninja: ")
        country <- inputUntilValid  ("Enter the country code of the " ++ ordinalString ++  " ninja: ") ["e","f","l","n","w"]
        let ninja = filter (\ninja -> (name ninja) == name_) (convertCountry country e f l n w)
        if null ninja then do
                putStrLn "Please enter a valid name-country pair."
                getithNinja i e f l n w
        else
                return $ head ninja

-- If both of them have the same score, the one with the higher Ability1 + Ability2
-- score will pass. If again the scores are equal, 1 of them will pass the round randomly. 10 points will
-- be added to the winner’s score.

makeARound :: Ninja -> Ninja -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO ()
makeARound ninja1 ninja2 e f l n w = do
        let [looser, winner] = sortBy(\n1 n2 -> compare (score n1) (score n2)) [ninja1, ninja2]
        putStrLn $ "Winner: " ++ show winner -- MUST SHOW UPDATED WINNER HERE

        let [[e', f', l', n', w']] = updateNinja listDelete (looser) e f l n w                                                                                                              
        let [[e'', f'', l'', n'', w'']] = updateNinja listUpdate (winner) e' f' l' n' w'

        -- his/her position in the country list will also change according to the rules described 
        -- in View a Country’sNinja Information,

        showUIList True e'' f'' l'' n'' w''


ninjaRound :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
ninjaRound e f l n w = do

        -- Each country will promote only 1 ninja to journeyman. Therefore, if this country has already 1
        -- promoted ninja, then a warning will be given to the user and any ninja from this country cannot be
        -- included to the fights anymore even if they are not disqualified. You can use a Boolean flag for each
        -- country in order to give this warning.

        ninja1 <- getithNinja 1 e f l n w
        ninja2 <- getithNinja 2 e f l n w
        makeARound ninja1 ninja2 e f l n w
        

journeymanList :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
journeymanList e f l n w = do
        let unsortedjourney = filter(\ninja -> (status ninja) == "Journeyman") (f++l++w++n++e)
        print (ninjaInfoSort unsortedjourney)


countryRound :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
countryRound e f l n w = do
        country1 <- inputUntilValid "Enter the first country code: " ["e","f","l","n","w"]
        country2 <- inputUntilValid "Enter the second country code: " ["e","f","l","n","w"]
        if country1 == country2 then do
                putStrLn "Please select two distinct countries."
                countryRound e f l n w
        else do
                -- Then, the first ninjas of each country list will make a round.
                let ninja1 = head $ ninjaInfoSort (convertCountry country1 e f l n w)
                let ninja2 = head $ ninjaInfoSort (convertCountry country2 e f l n w)
                makeARound ninja1 ninja2 e f l n w
                

showUIList :: Bool -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO()
showUIList state e f l n w = do 
        
        action <- prompt state
        let lowered_action = map toLower action
        case lowered_action of
                "a" -> countryNinjaInfo e f l n w
                "b" -> allNinjaInfo e f l n w
                "c" -> ninjaRound e f l n w
                "d" -> countryRound e f l n w
                "e" -> journeymanList e f l n w
                _   -> showUIList False e f l n w


main :: IO ()
main = do
        args <- getArgs 
        file <- openFile (head args) ReadMode
        all_ninjas <- readNinjas file []
        let sortedNinjas = sortBy (\n1 n2 -> compare (country n1) (country n2)) all_ninjas
        let [earth, fire, lightning, wind, water] = groupBy (\n1 n2 -> (country n1) == (country n2)) sortedNinjas
        showUIList True earth fire lightning wind water
        

        
