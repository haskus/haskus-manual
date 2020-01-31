==============================================================================
Use the logging mechanism
==============================================================================

Many high-level interfaces of the ``haskus-system`` use the ``Sys`` monad. It is
basically a wrapper for the ``IO`` monad that adds a logging mechanism.

The following code prints the system log that is implicitly maintained in the
``Sys`` monad on the kernel console.

.. code:: haskell

   {-# LANGUAGE BlockArguments #-}

   import Haskus.System
   
   main :: IO ()
   main = runSys do
      term <- defaultTerminal
      writeStrLn term "Hello World!"
      waitForKey term
      sysLogPrint -- print system log
      powerOff

Hence, the output of this program is something like:

.. code:: text

   Hello World!
   
   ---- Log root
   --*- FORK: Terminal input handler
     |---- Read bytes from Handle 0 (succeeded with 1)
     |---- readBytes /= 0 (success)
   --*- FORK: Terminal output handler

   [    1.818814] reboot: Power down

You can see that the log is hierarchical and that it supports thread forks:
``defaultTerminal`` forks 2 threads to handle asynchronous terminal
input/output; the input thread indicates in the log that it has read 1 byte from
the terminal input (when I have pressed the ``enter`` key).

Note that the log entries produced by the framework functions may change in the
future, hence the contents of the log may change and you may not get exactly the
same output if you try to execute this code.
