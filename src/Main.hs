module Main where
import Data.List

votos::[String]
votos = ["Vermelho","Azul","Verde","Azul","Azul","Vermelho"]

votosRanks::[[String]]
votosRanks = [["Vermelho","Verde"],["Azul"],["Verde","Vermelho","Azul"],["Azul","Verde","Vermelho"],["Verde"]]


main :: IO ()
main = do
  print $ "vencedor votos"
  --print $ vencedor' votosRanks

conta::Eq a => a -> [a] -> Int
conta candidato (x:xs) = conta' candidato (x:xs) 0
  where
    conta' _ [] i = i
    conta' cd (y:ys) i = if cd == y then conta' cd ys i+1 else conta' cd ys i

unicos::Eq a => [a] -> [a]
unicos xs = unicos' [] xs
    where
      unicos' unq [] = unq
      unicos' x:xs = if not(x `elem` unq) then unicos' x:unq xs else unicos' unq 



