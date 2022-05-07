import Parse ( Parser, Result(Good, Bad), parse )


check (Good "") = "correct syntax"
check _ = "incorrect syntax"


main = do
  input <- readFile "./file.html"
  putStrLn input
  putStrLn (check (parse (Good input)))
