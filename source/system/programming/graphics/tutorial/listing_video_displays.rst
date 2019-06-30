======================
Listing video displays
======================

To list the available video displays that are connected to the computer, we just
have to query the Connector entities and check if there is a video display
connected to them.

The whole source code for this chapter can be found `here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutVideoDisplays.hs>`_. It detects the available video displays and reports their supported modes and their properties.

* We retrieve information about all the entities for each card with ``getEntities card``

* Connector (retrieved with ``entitiesConnectors``) have a ``connectorState``
  property which can be used to detect connected video display:

.. code:: haskell

   case connectorState conn of
      Disconnected           -> -- no connected video display
      ConnectionUnknown      -> -- we can't know
      Connected videoDisplay -> -- we have a connected video display!

We get the supported modes of the video display with ``videoModes
videoDisplay`` and the other properties with ``videoProperties videoDisplay``.

Example of run into QEmu with Linux 5.1.15:

.. code:: text

   > git clone https://github.com/haskus/haskus-system.git
   > cd haskus-system/haskus-system-examples
   > haskus-system-build test --init TutVideoDisplays

   Probing Connector 33
   Modes
   1024x768 60MHz -HSync -VSync
           h: width 1024 start 1048 end 1184 total 1344 skew    0
           v: width  768 start  771 end  777 total  806 scan    0
   1920x1080 60MHz -HSync -VSync
           h: width 1920 start 2008 end 2052 total 2200 skew    0
           v: width 1080 start 1084 end 1089 total 1125 scan    0
   1600x1200 60MHz +HSync +VSync
           h: width 1600 start 1664 end 1856 total 2160 skew    0
           v: width 1200 start 1201 end 1204 total 1250 scan    0
   [...]
   Properties
       var DPMS = On :: Enum [On,Standby,Suspend,Off]
       var link-status = Good :: Enum [Good,Bad]
       val non-desktop = False :: Bool
       var CRTC_ID = 0 :: Object

