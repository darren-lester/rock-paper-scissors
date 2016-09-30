import System.Random
data Move = Rock | Paper | Scissors deriving (Show, Read, Eq, Enum)
data Winner = Player | Computer | Tie

beats :: Move -> Move -> Bool
Rock `beats` Scissors = True
Paper `beats` Rock = True
Scissors `beats` Paper = True
_ `beats` _ = False

play :: Move -> Move -> Winner
play playerMove computerMove
 | playerMove == computerMove = Tie
 | playerMove `beats` computerMove = Player
 | otherwise = Computer

result :: Winner -> String
result Player = "You win!"
result Computer = "You lose!"
result Tie = "It's a tie!"

main :: IO ()
main = do
       putStrLn "Enter 'Rock', 'Paper' or 'Scissors':"
       playerMove <- getLine
       g <- newStdGen
       let computerMove = toEnum ( fst $ randomR (0, 2) g) :: Move
           winner = play (read playerMove :: Move) computerMove
       putStrLn $ "Computer plays " ++ show computerMove ++ "..."
       putStrLn $ result winner
