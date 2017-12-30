import Control.Arrow ((***))
import Text.Parsec
main = interact $ show . parse (group 1) "source"
group d = char '{' *> groupElement d <* char '}'
groupElement d = ((d+) . sum *** sum) . unzip <$>
                 sepBy (group (d+1) <|> garbage) (char ',')
garbage = char '<' *> garbageElement <* char '>'
garbageElement = (,) 0 . sum <$>
                 many ( (char '!' *> anyChar *> pure 0) <|>
                        (noneOf ">" *> pure 1) )
