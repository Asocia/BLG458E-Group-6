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