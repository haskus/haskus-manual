:set -XDataKinds

import Haskus.Utils.Flow
import Haskus.Utils.Variant.Excepts
import Excepts

runE (foo "d10")
runE (foo "x10")
runE (foo "u10")
runE (foo "")
runE (foo "d10X")
runE (foo "" |> catchE (\ListWasEmpty -> successE 42) :: Excepts '[ParseError,LookupError Char] IO Integer)
