======================
Listing video displays
======================

To list the available video displays that are connected to the computer, we just
have to query the Connector entities and check if there is a video display
connected to them.

The whole source code for this chapter can be found `here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutVideoDisplays.hs>`_.
It detects the available video displays and reports information about them.

* We retrieve information about all the entities for each card with ``getEntities card``

* Connectors (retrieved with ``entitiesConnectors``) have ``connectorType`` and
  ``connectorByTypeIndex`` fields which can be used to query the kind of connector (HDMI,
  VGA, DVI, etc.) and the connector index for the given kind of connector.

* Connectors also have a ``connectorState`` field which can be used to detect
  connected video display:

.. code:: haskell

   case connectorState conn of
      Disconnected           -> -- no connected video display
      ConnectionUnknown      -> -- we can't know
      Connected videoDisplay -> -- we have a connected video display!

* We get the supported modes of the video display with ``videoModes`` field of
  ``videoDisplay``, physical size in millimeters with
  ``videoPhysicalWidth/Height``, the sub-pixel layout with ``videoSubPixel`` and
  other properties with ``videoProperties``.

Example of run into QEmu with Linux 5.1.15:

.. code:: text

   > git clone https://github.com/haskus/haskus-system.git
   > cd haskus-system/haskus-system-examples
   > haskus-system-build test --init TutVideoDisplays

   Probing Connector 33: Virtual-1
   Physical size: 0mm X 0 mm
   Sub-pixel layout: SubPixelUnknown
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

Detecting Plugging/Unplugging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Remember that Connector entities can appear and disappear at runtime. That's
because some technologies (such as `DisplayPort Multi-Stream Transport
<https://en.wikipedia.org/wiki/DisplayPort#Multi-Stream_Transport_(MST)>`_)
allows the use of connectors hubs which increases the number of video displays
that can be connected at the same time.

To detect when a video display is connected or disconnected, we could
periodically list the Connectors and check their ``connectorState`` property as
we have done above.

However a better method is to use a mechanism explained in the `basic device
management </system/manual/using/devices>`_ page: when the state of a Connector
changes, the kernel sends to the user-space an event similar to the following
one:

.. code:: haskell

   KernelEvent
      { kernelEventAction = ActionChange
      , kernelEventDevPath = "/devices/.../drm/card0"
      , kernelEventSubSystem = "drm"
      , kernelEventDetails = fromList
         [("DEVNAME","drm/card0")
         ,("MAJOR","226")
         ,("MINOR","0")
         ,("HOTPLUG","1")
         ,("SEQNUM","1259")]}

When our system receives this event, we know it has to check the state of the
Connectors.
