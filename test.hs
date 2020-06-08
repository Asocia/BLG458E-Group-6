import System.IO


getAction :: IO String
getAction = do 
    putStrLn "a) View a Country’s Ninja Information"
    putStrLn "b) View All Countries’ Ninja Information"
    putStrLn "c) Make a Round Between Ninjas"
    putStrLn "d) Make a Round Between Countries"
    putStrLn "e) Exit"
    putStrLn "Enter the action: "

    action <- getLine
    return action


main :: IO ()
main = do
    action <- getAction
 
    case action of 
        "a" -> putStrLn "Country’s Ninja Information"
        "b" -> putStrLn "View All Countries’ Ninja Information"
        "c" -> putStrLn "Make a Round Between Ninjas"
        "d" -> putStrLn "Make a Round Between Countries"
        "e" -> return ()
        _ -> putStrLn "Invalid action. Try again."
    if not (action == "e")
        then main
    else return ()

updateJourneymanList :: [Bool] -> Int -> [Bool]
updateJourneymanList journeymanL n
        | n < 0     = journeymanL
        | otherwise = take n journeymanL ++ [True] ++ drop (n + 1) journeymanL

makeARound :: Ninja -> Ninja -> [[Ninja]] -> [Bool] -> IO ()
makeARound ninja1 ninja2 ninjas journaymanL = do
        let (looser, winner) = fight ninja1 ninja2
        -- putStrLn $ "Winner: " ++ show winner -- MUST SHOW UPDATED WINNER HERE
        let status = if (r winner) < 2 then "Junior" else "Journeyman"
        let updatedWinner = winner {status = status, r = succ (r winner), score = (score winner)+10 }
        putStrLn $ "Winner: " ++ show updatedWinner
        let countryCode = [country winner]
        let updateIndex = if status == "Journeyman" then getItem countryCode [0..4] else -1
        let updatedJourneymanL = updateJourneymanList journaymanL updateIndex
        let ninjas' = updateNinja remove (looser) ninjas                                                                                                              
        let ninjas'' = updateNinja update (winner) ninjas'
        
        showUIList False ninjas'' updatedJourneymanL