===================
Linux display model
===================

Linux provides an API named *kernel mode setting* (KMS) that allows us to
control video displays. The idea is to build a pipeline that describes where the
video displays get their pixel colors from.

The left side of the following picture describes the relations between pipeline entities and the right side shows a more visual representation of how an image is formed on a video display surface using the same entities.


.. image:: /_static/images/system/graphics_linux_model.svg
   :class: img_center


To use a video display, our task is to build such valid pipeline. There are so
many possible hardware configurations (different graphics chipsets, different
video displays, etc.) that the KMS interface is very generic. It lets us build a
pipeline quite liberally and then we can test it before enabling it for real if
it is valid.

Controller and Plane entities of the graphics pipeline are fixed for each
graphics chipset. However Connectors are *not* fixed because some technologies
(such as `DisplayPort Multi-Stream Transport
<https://en.wikipedia.org/wiki/DisplayPort#Multi-Stream_Transport_(MST)>`_)
allows the use of connectors hubs which dynamically add additional Connector
entities. Frames are managed by software so they are not fixed either.

Listing entities
----------------

As our first code example in this tutorial, we will list all the entities of all
the graphic cards we find on the system.  The whole code source can be found
`here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutEntitiesIDs.hs>`_.

We load all the graphic cards with:

.. code:: haskell

      sys   <- defaultSystemInit
      cards <- loadGraphicCards (systemDeviceManager sys)

Then we get entity identifiers with:

.. code:: haskell
      
         mids <- runE (getEntitiesIDs card)

The rest of the code deals with errors and printing the results on the terminal.

The best way to test this code is to use :ref:`haskus-system-build tool
<haskus-system-build-tool>`.

.. code:: text

   > git clone https://github.com/haskus/haskus-system.git
   > cd haskus-system/haskus-system-examples
   > haskus-system-build test --init TutEntitiesIDs

   ===================================================
          Haskus system - build config
   ---------------------------------------------------
   GHC version:      8.6.4
   Linux version:    5.1.15
   Syslinux version: 6.03
   Init program:     TutEntitiesIDs
   ===================================================
   ==> Configuring Stack...
   stack will use a sandboxed GHC it installed
   For more information on paths, see 'stack path' and 'stack exec env'
   To use this GHC and packages outside of a project, consider using:
   stack ghc, stack ghci, stack runghc, or stack exec
   ==> Building with Stack...
   ==> Building ramdisk...
   16299 blocs
   ==> Launching QEMU...
   Card 0
    - Connector 33
    - Controller 31
    - Plane 30
   [    1.026338] reboot: Power down

The tool should download, build and install the necessary dependencies and
execute the resulting system into ``QEMU``. You can see that it reports one
Connector, one Controller and one Plane for the QEMU simulated graphics chipset
(the numbers are their unique identifiers).
