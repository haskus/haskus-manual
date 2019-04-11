#!/bin/sh
echo "=============================="
echo "Excepts"
echo "=============================="
stack exec -- ghc -e ":script scripts/ExceptsScript.hs"

echo "=============================="
echo "VEither"
echo "=============================="
stack exec -- ghc -e ":script scripts/VEither.hs"
