:set -XDataKinds
import Haskus.Utils.Variant.VEither
import Haskus.Utils.Flow

VRight True :: VEither '[String,Int] Bool
VLeft (V "failed" :: V '[String,Int]) :: VEither '[String,Int] Bool

let x = VRight True :: VEither '[Int,Float] Bool
fmap (\b -> if b then "Success" else "Failure") x

let x = VRight True  :: VEither '[Int,Float] Bool
let y = VRight False :: VEither '[Int,Float] Bool
(&&) <$> x <*> y

let x   = VRight True    :: VEither '[Int,Float] Bool
let f v = VRight (not v) :: VEither '[Int,Float] Bool
x >>= f

let x   = VRight True    :: VEither '[Int,Float] Bool
let y   = VLeft (V "failed" :: V '[String,Int]) :: VEither '[String,Int] Bool
forM_ x print

forM_ y print
