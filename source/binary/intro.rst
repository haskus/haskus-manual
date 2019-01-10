==============================================================================
Intro
==============================================================================

Some packages (e.g. ``haskus-system``) use these binary modules to provide
bindings to C libraries. We don't want to rely on external tools such as C2HS to
provide bindings to C libraries because:

* We don't want to depend on .h files;
* .h files often contain pecularities that are difficult to handle
  automatically;
* Documentation and code of the resulting Haskell files are often very bad:

    * No haddock
    * Very low-level (e.g. #define are not transformed into ADTs with Enum
      instances)

Instead ``haskus-binary`` lets you write bindings in pure Haskell code and
provides many useful things to make this process easy.


