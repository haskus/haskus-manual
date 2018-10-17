Building systems
----------------

Suppose we have the following ``HelloWorld`` system code and that we
want to build it.

.. code:: haskell

   import Haskus.System
   
   main :: IO ()
   main = runSys' <| do
      term <- defaultTerminal
      writeStrLn term "Hello World!"
      waitForKey term
      powerOff

You can use ``Cabal`` as for any other Haskell program. However, we want to
build a ``static`` executable, hence the ``.cabal`` file must contain an
``executable`` section such as:

.. code::

   executable HelloWorld
      main-is: HelloWorld.hs
      build-depends:
         base,
         haskus-system
      default-language: Haskell2010
      ghc-options:      -Wall -static -threaded
      cc-options:       -static
      ld-options:       -static -pthread
      #extra-lib-dirs: /path/to/static/libs

If static versions of the ``libgmp``, ``libffi`` and ``glibc`` libraries (used
by GHC's runtime system) are not available on your system, you have to compile
them and to indicate to the linker where to find them: uncomment the last line
in the previous extract of the ``.cabal`` file (the ``extra-lib-dirs`` entry)
and modify it so that the given path points to a directory containing the static
libraries.

Finally, we recommend using ``stack`` to ensure that you are using appropriate
versions of GHC and of ``haskus-system``. Example of ``stack.yaml`` contents:

.. code:: yaml

   resolver: lts-8.15

   packages:
   - '.'
   - location:
       git: git@github.com:haskus/haskus-system
       commit: 33ba0413f2adae33f66f78e51f7e9e52e63758f1
     extra-dep: true
   
   ghc-options:
      "haskus-system": -freduction-depth=0 -fobject-code

   extra-deps:
   - haskus-binary-0.6.0.0
   - haskus-utils-0.6.0.0

Finally use ``stack build`` to compile the program.

**Examples**


The `haskus-system-examples
<http://www.github.com/haskus/haskus-system-examples>`_ repository contains
several examples of such systems (including this ``HelloWorld`` program).

Refer to its `.cabal file
<http://github.com/haskus/haskus-system-examples/tree/master/haskus-system-examples.cabal>`_
and to its `stack.yaml
<https://github.com/haskus/haskus-system-examples/tree/master/stack.yaml>`_
file.
