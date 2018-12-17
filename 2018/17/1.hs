-- stack --resolver lts-12.0 script

main
  = print
  . take 5
  . lines
  =<< readFile "input.txt"
