-- stack --resolver ghc-8.6.3 script
main = print =<< readFile "input.txt"
