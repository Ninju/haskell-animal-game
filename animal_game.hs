module AnimalGame where
import System.IO

data QuestionTree = Animal String | Question String QuestionTree QuestionTree -- nodes are questions, and leaves are Animals
data Answer = Yes | No

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          play (Animal "Dog")
          return ()

play :: QuestionTree -> IO QuestionTree
play root = do putStrLn "Think of an animal, I will try to guess what it is..."
               newRoot <- play' root
               playAgain <- ask "Do you want to play again?"
               case playAgain of
                   Yes -> play newRoot
                   No  -> do putStrLn "Thanks for playing.."
                             return newRoot

play' :: QuestionTree -> IO QuestionTree
play' animal@(Animal _) = do ans <- ask $ "Are you thinking of " ++ show animal ++ "?"
                             case ans of
                                Yes -> do putStrLn "I win this time."  
                                          return animal
                                No  -> do putStrLn "I give up, you win!"
                                          getNewAnimal animal -- returns a new question
play' question@(Question s y n) = do ans <- ask s
                                     case ans of
                                        Yes -> do newYes <- play' y
                                                  return $ Question s newYes n
                                        No  -> do newNo <- play' n
                                                  return $ Question s y newNo

getNewAnimal :: QuestionTree -> IO QuestionTree
getNewAnimal animal = do putStrLn "Please help me improve my guesses!"
                         putStrLn "What is the name of the animal you were thinking of?"
                         name <- getLine
                         let newAnimal = Animal name
                         putStrLn $ "Now please enter a question that answers yes for " ++ show newAnimal ++ " and no for " ++ show animal
                         question <- getLine
                         return $ Question question newAnimal animal

ask :: String -> IO Answer
ask s = do putStrLn $ s ++ " (y/n)"
           getAnswer

getAnswer :: IO Answer
getAnswer = do ans <- getChar
               putStrLn ""
               case ans of
                   'y' -> return Yes
                   'n' -> return No
                   _   -> putStrLn "That is not a valid response, please enter 'y' or 'n'..." >> getAnswer

instance Show QuestionTree where
    show (Animal name) = (if elem (head name) "AEIOUaeiou" then "an " else "a ") ++ name
    show (Question s _ _) = s
