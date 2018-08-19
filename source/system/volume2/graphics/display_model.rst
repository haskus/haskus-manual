==============================================================================
Linux display model
==============================================================================

------------------------------------------------------------------------------
Overview
------------------------------------------------------------------------------

Linux's display model (also known as KMS standing for ``kernel mode-setting``
interface) is composed of several entities that interact with each other. These
entities are represented on the following graph:

.. image:: /_static/images/system/graphics_entities.svg
   :class: img_center

The numbers indicate relationship arities: for instance a ``Plane`` can be
connected to at most a single ``FrameSource`` and a single ``Controller``, but a
``Controller`` can be connected to several ``Planes`` and a ``FrameSource`` can
be used by several ``Planes``.  The arrows indicate entities that store
references to others entities (e.g., ``Encoder`` entities store references to
``Controller`` entities).

In order to display something, we have to configure a pipeline that goes from
some ``PixelSource`` (pixel data stored in memory buffers) to some
``Connectors`` (entities representing physical ports onto which display devices
are connected).

Entity properties can be used to query information about their state and to
configure them (e.g., ``Connector``'s "DPMS" property to configure the power
state of the connected display, ``Plane``'s "rotation" property, etc.).


------------------------------------------------------------------------------
Card
------------------------------------------------------------------------------

The following schema shows a contrived example of a graphic card containing some
of these entities.

.. image:: /_static/images/system/graphics_card.svg
   :class: img_center

Entities belong to a single graphic card: they can't be shared between several
graphic cards (if your system has more than one of these).


------------------------------------------------------------------------------
Connectors
------------------------------------------------------------------------------

Each physical port where you can plug a display device (a monitor, a
video-projector, etc.) corresponds to a ``Connector`` entity in the display
model.

The following code shows how to retrieve the graphic card objects and how to display information about each connector:

.. code:: haskell

   import Haskus.System
   import Haskus.Arch.Linux.Graphics.State
   
   main :: IO ()
   main = runSys <| do
   
      sys   <- defaultSystemInit
      term  <- defaultTerminal
   
      -- get graphic card devices
      cards <- loadGraphicCards (systemDeviceManager sys)
      
      forM_ cards <| \card -> do
         state <- readGraphicsState (graphicCardHandle card)
                  >..~!!> assertShow "Cannot read graphics state"
   
         -- get connector state and info
         let conns = graphicsConnectors state
         
         -- show connector state and info
         writeStrLn term (show conns)
   
      void powerOff

When executed in ``QEMU``, this code produces the following output:

.. code:: haskell

   -- Formatting has been enhanced for readability
   [ Connector
      { connectorID = ConnectorID 21
      , connectorType = Virtual
      , connectorByTypeIndex = 1
      , connectorState = Connected (ConnectedDevice
         { connectedDeviceModes =
            [ Mode
               { ...
               , modeClock = 65000
               , modeHorizontalDisplay = 1024
               , modeVerticalDisplay = 768
               , modeVerticalRefresh = 60
               , modeFlags = fromList [ModeFlagNHSync,ModeFlagNVSync]
               , modeStereo3D = Stereo3DNone
               , modeType = fromList [ModeTypePreferred,ModeTypeDriver]
               , modeName = "1024x768" }
            , ...
            ]
         , connectedDeviceWidth = 0
         , connectedDeviceHeight = 0
         , connectedDeviceSubPixel = SubPixelUnknown
         , connectedDeviceProperties =
            [ Property
               { propertyMeta = PropertyMeta 
                  { ...
                  , propertyName = "DPMS"
                  , propertyType = PropEnum 
                     [ (0,"On")
                     , (1,"Standby")
                     , (2,"Suspend")
                     , (3,"Off")]
                  }
               , propertyValue = 0
               }
            ]
         })
      , connectorPossibleEncoderIDs = [EncoderID 20]
      , connectorEncoderID = Just (EncoderID 20)
      , connectorHandle = Handle ...
      }
   ]


Each connector reports its type in the ``connectorType`` field: in our example
it is a virtual port because we use ``QEMU``, but it could have been ``VGA``,
``HDMI``, ``TV``, ``LVDS``, etc.

If there are several connectors of the same type in the same card, you can
distinguish them with the ``connectorByTypeIndex`` field.

You can check that a display device is actually plugged in a connector with the
``connectorState`` property: in our example, there is a (virtual) screen
connected. 

We can get more information about the connected device:

* ``connectedDeviceModes``: modes supported by the connected display device.  In
  particular, a display resolution is associated to each mode. In our example,
  the display resolution of the first mode is 1024x768; the other modes have
  been left out for clarity.

* ``connectedDeviceWidth`` and ``connectedDeviceHeight``: some display devices
  report their physical dimensions in millimeters.

* ``connectedDeviceSubPixel``: whether the device uses some kind of sub-pixel
  technology.

* ``connectedDeviceProperties``: device specific properties.  In this example,
  there is only a single property named "DPMS" which can take 4 different values
  ("On", "Standby", "Suspend", "Off") and whose current value is 0 ("On"): this
  property can be used to switch the power mode of the screen.

A ``connector`` gets the data to display from an ``encoder``:

* ``connectorPossibleEncoderIDs``: list of encoders that can be used as sources.

* ``connectorEncoderID``: identifier of the currently connected encoder, if any.

Detecting Plugging/Unplugging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can adapt what our system displays to the connected screens, but how do we
detect when a screen is connected or disconnected?

A solution would be to periodically check the value of the ``connectorState``
property. But a better method is to use a mechanism explained in the `basic
device management </system/manual/using/devices>`_ page: when the state of a
connector changes, the kernel sends to the user-space an event similar to the
following one:

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

When the system receives this event, it knows it has to check the state of the
connectors.

.. note::

   Note that the number of ``connector`` entities may change dynamically. For
   instance a single ``DisplayPort`` connector supporting the Multi-Stream
   Transport (MST) allows several monitors to be connected in sequence
   (daisy-chaining): each monitor receives its own video stream and appears as a
   different ``connector`` entity. It is also possible to connect a MST hub that
   increases the number of ``connector`` entities.

------------------------------------------------------------------------------
Encoders
------------------------------------------------------------------------------

Encoders convert pixel data into signals expected by connectors: for instance
``DVI`` and ``HDMI`` connectors need a ``TMDS`` encoder.  Each card provides a
set of encoders and each of them can only work with some controllers and some
connectors. There may be a 1-1 relationship between an ``encoder`` and a
``connector``, in which case the link between them should already be set.

We can display information about encoders using a code similar to the code above
for connectors. When executed into ``QEMU``, we get the following result:

.. code:: haskell

   [ Encoder 
      { encoderID = EncoderID 20
      , encoderType = EncoderTypeDAC
      , encoderControllerID = Just (ControllerID 19)
      , encoderPossibleControllers = [ControllerID 19]
      , encoderPossibleClones = []
      , encoderHandle = Handle ...
      }
   ]

As we can observe, the graphic card emulated by ``QEMU`` emulates a single
``DAC`` encoder.

The ``encoderPossibleClones`` field contains the sibling encoders that can be
used for cloning: only these encoders can share the same controller as a source.

------------------------------------------------------------------------------
Controllers
------------------------------------------------------------------------------

Controllers let you configure:

*  The display mode (display resolution, etc.) that will be used by the
   display devices that are connected to the controller through an encoder and a
   connector.

* The primary source of the pixel data from a ``FrameBuffer`` entity

We can display information about controllers using a code similar to the code above
for connectors. When executed into ``QEMU``, we get the following result:

.. code:: haskell

   [ Controller
      { controllerID = ControllerID 19
      , controllerMode = Just (Mode { ...})
      , controllerFrameBuffer = Just (FrameBufferPos
         { frameBufferPosID = FrameBufferID 46
         , frameBufferPosX = 0
         , frameBufferPosY = 0
         })
      , controllerGammaTableSize = 256
      , controllerHandle = Handle ...
      }
   ]


* ``controllerMode``: the display mode that has to be used by the display device(s).

*  ``controllerFrameBuffer``: the ``FrameBuffer`` entity used as a data source and the coordinates in the ``FrameBuffer`` contents.

------------------------------------------------------------------------------
Planes
------------------------------------------------------------------------------

Some controllers can blend several layers together from different
``FrameBuffer`` entities: these layers are called ``Planes``. Controller support
at least a ``primary`` plane and they can support others such as cursor or
overlay planes.

::

   TODO:
      * List plane resources
      * primary plane
      * cursor planes
      * overlay planes
      * example


------------------------------------------------------------------------------
FrameSource and PixelSource
------------------------------------------------------------------------------

Planes take their input data from abstract ``FrameSource`` entities. A
``FrameSource`` describes a frame with its dimensions (width and height in
pixels), the pixel format (``8-bit RGB``, ``YUYV``, etc.) and one or more
``PixelSource`` indicating where to fetch pixels from.

Some pixel encoding formats require up to 4 ``PixelSource`` entities that are
combined to obtain final pixel colors.

::

   TODO:
      * Pixel formats
      * FrameBuffer dirty
      * Mode

------------------------------------------------------------------------------
Host buffers ("software" rendering on the CPU)
------------------------------------------------------------------------------

Most GPU drivers support pixel buffers stored in host memory (``HostBuffer``,
"dumb buffers" in Linux terminology).  These buffers are not accessible by the
GPU hence we can't use the GPU to perform computation into them. However, we can
easily use them to perform softawre/CPU rendering.

Applications only have to map the contents of the ``HostBuffer`` into their
memory address spaces and to modify it to change what is displayed.

------------------------------------------------------------------------------
Further Reading
------------------------------------------------------------------------------

The two main acronyms for Linux's display model are KMS (standing for "kernel
mode-setting") and DRM (standing for "direct rendering maanger").

As explained in the :ref:`device-management` section, device drivers can support
the ``ioctl`` system call to handle device specific commands from the
user-space. The display interface is almost entirely based on it. Additionally,
``mmap`` is used to map graphic card memory in user-space and ``read`` is used
to read events (V-Blank and page-flip asynchronous completion).

In usual Linux distributions, the ``libdrm`` library provides an interface over
these system calls. You can learn about the low-level interface by reading the
``drm`` manual (``man drm``, ``man drm-kms``, etc.) or its `source code
<https://cgit.freedesktop.org/mesa/drm/>`_.

David Herrmann has written `a good tutorial
<https://dvdhrm.wordpress.com/?s=drm-mode-setting>`_ explaining how to use the
legacy low-level display interface in the form of C source files with detailed
comments. While some details of the interface have changed since he wrote it
(e.g., the way to flip frame buffers and the atomic interface), it is still a
valuable source of information.

The newer atomic interface is described in an `article
<https://lwn.net/Articles/653071}>`_ `series
<https://lwn.net/Articles/653466/>`_ on LWN called "Atomic mode setting design
overview" (August 2015) by Daniel Vetter.

`Wayland <http://wayland.freedesktop.org>`_ is the new display system for usual
Linux based distributions. It can be a great source of inspiration and of
information.

You can also read the Linux kernel code located in ``drivers/gpu/drm`` in the
kernel sources.

Multi-GPU is supported by Linux. In particular:

* Buffer sharing is supported with `DRM Prime <https://01.org/linuxgraphics/gfx-docs/drm/drm-memory-management.html\#drm-prime-support>`_

* GPU switching is supported with `vga_switcheroo <https://01.org/linuxgraphics/gfx-docs/drm/vga_switcheroo.html>`_
