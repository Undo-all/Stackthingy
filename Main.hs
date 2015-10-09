import Compile
import System.Environment

main = do
    args <- getArgs
    case args of
      [fileName] -> do
        xs <- readFile fileName
        case compile fileName xs of
          Right asm -> writeFile (takeWhile (/='.') fileName ++ ".s") asm
          Left err  -> print err

