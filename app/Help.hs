module Help (showHelp) where

showHelp :: String -> String
showHelp "" =
  "This is where the main help text goes."
showHelp _ =
  "There is no help on this topic."
