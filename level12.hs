import Data.Char     (isDigit)
import Data.List     (groupBy)
import Data.Function (on)
import Data.Graph    (buildG,reachable,scc)
import Control.Arrow ((&&&))
-- (I'm not proud of this.)
main = interact $ show . (length . flip reachable 0 &&& length . scc) .
                  uncurry buildG . (((,) 0) . pred . length &&& concat) .
                  map ( uncurry map . ((,) . head &&& tail) . map read .
                        filter (isDigit . head) . groupBy ((==) `on` isDigit) ) .
                  lines
