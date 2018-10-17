The Sys monad
-------------

Many high-level interfaces of the ``haskus-system`` use the ``Sys`` monad. It is
basically a wrapper for the ``IO`` monad that adds a logging mechanism.

For instance, consider the following ``HelloWorld`` code where ``runSys :: Sys
a -> IO a``:

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

      -- print system log
      sysLogPrint
   
      -- shutdown the computer
      void powerOff

This code prints the string "Hello World!" in the Linux terminal and waits for a
char to be entered in the terminal. Then it prints the system log that is
implicitly maintained in the ``Sys`` monad. Hence, the output of this program is
something like:

.. code::

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
