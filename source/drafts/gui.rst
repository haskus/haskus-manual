==============================================================================
GUI programming
==============================================================================

Graphics User Interfaces (GUI) reflect the state of the system and let the user
interact with it. There are two things:

   * Frames: pictures that are displayed on screen

   * Interaction: given the currently displayed frame and the state of the
     system (i.e. the currently displayed frame is part of the state in fact),
     how to react to events (mouse click, key press, etc.)

Basic game loop
---------------

A basic game loop acts like this:

.. code:: haskell

   gameLoop :: IO ()
   gameLoop = do

      -- upate the state
      whileM (currentTime - lastSimulationTime >= timeStep) do
         -- process input events and other system events that have happened
         -- during [lastSimulationTime, lastSimulationTime+timeStep]
         processEvents

         updateSimulation timeStep  -- update physics in constant meaningful steps

         lastSimulationTime += timeStep

      -- perform frame rendering
      -- Parameter is used for interpolation (1 frame lag) or extrapolation
      -- (potential glitches) of the remaining time
      tmpState <- simulateUpdate (currentTime - lastSimulationTime)
      render tmpState

      unlessM quitTheGame
         gameLoop


Modifying the state
-------------------

With MonadFlow M we track access to values from the Monad M. Basically we build
a tree of commands but if the values we read from M are the same as those in
cache, we know that the tree is exactly the same (pure function with the same
inputs).

We build a pure tree representing the new state or we indicate if no change
occurred: then we don't have to redraw at all.

M must be able to give a coherent view of the system at a given point in time.

References
----------

* http://gameprogrammingpatterns.com/game-loop.html
* https://dewitters.com/dewitters-gameloop/
* https://webcache.googleusercontent.com/search?q=cache:5cH3UfBvb2YJ:vodacek.zvb.cz/archiv/681.html&hl=en&gl=us&strip=1&vwsrc=0
