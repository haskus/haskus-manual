==============================================================================
Hello World
==============================================================================

The following program is the "Hello World" of Haskus system:

.. code:: haskell

   import Haskus.System
   
   main :: IO ()
   main = runSys <| do
   
      -- Initialize the default terminal
      term <- defaultTerminal
   
      -- print a string on the standard output
      writeStrLn term "Hello World!"
   
      -- wait for a key to be pressed
      waitForKey term

      -- shutdown the computer
      powerOff_

It writes the "Hello World" string on the default terminal, then wait for a key
to be pressed, and finally it power off the computer.

Note that unlike traditional Unix programs, we don't use STDIN and STDOUT
explicitly: we have to query a terminal and to use it explicitly.
