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
                    ability2 :: String }

fire :: [Ninja] 
fire = []
lightning :: [Ninja] 
lightning = []
water :: [Ninja] 
water = []
wind :: [Ninja] 
wind = []
earth :: [Ninja] 
earth = []

fireNinjaInit :: [String] -> Float -> Float -> Ninja
fireNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='F', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5) } in nin

lightningNinjaInit :: [String] -> Float -> Float -> Ninja
lightningNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='L', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5) } in nin

waterNinjaInit :: [String] -> Float -> Float -> Ninja
waterNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='W', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5) } in nin

windNinjaInit :: [String] -> Float -> Float -> Ninja
windNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='A', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5) } in nin

earthNinjaInit :: [String] -> Float -> Float -> Ninja
earthNinjaInit ninja s1 s2 =
        let nin = Ninja {name = (ninja !! 0), country='E', status = "dunno", exam1 = s1, exam2 = s2, ability1 = (ninja !! 4), ability2 = (ninja !! 5) } in nin



placeToCountry :: [String] -> Float -> Float -> Ninja
placeToCountry ninja s1 s2= case (ninja !! 1) of 
                "Fire" -> fireNinjaInit ninja s1 s2
                "Lightning" -> fireNinjaInit ninja s1 s2
                "Water" -> fireNinjaInit ninja s1 s2
                "Wind" -> fireNinjaInit ninja s1 s2
                "Earth" -> fireNinjaInit ninja s1 s2
                _ -> error "no ninja"
                 


readNinja :: Handle -> IO()
readNinja file = do
        end <- hIsEOF file
        print end
        if not end then do
                line <- hGetLine file
                let placeholder = T.splitOn (T.pack " ") (T.pack line) --okunan satırı kelimelerine ayırıyor hangi countryden geldiğini bulmamız için
                let ninja = map T.unpack placeholder --kelimelerine ayırdığımız zaman tipi T.text tarzı bir şey olmuştu burda tipini stringe çevirdim
                let score1 = read(ninja !! 2) :: Float
                let score2 = read(ninja !! 3) :: Float 
                let ph = placeToCountry ninja score1 score2
                readNinja file
                else do
                        print "end of file" 
     



main :: IO ()
main = do
        args <- getArgs --txt dosyasını argüman olarak alıyor
        file <- openFile (head args) ReadMode
        --file dosyasını açıyor --------  ama burada denediğim zaman file değişkeninin tipini yazdırmadı o yüzden direk bunu fonksiyona gönderemedim
        readNinja file
        print "end"
        --ninjak = placeToCountry ninja score1 score2

        