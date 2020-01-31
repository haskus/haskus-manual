=====================================================================
Enumerating Available Devices
=====================================================================

``haskus-system`` provides an easy to use interface to list devices as detected
by the Linux kernel.  To do that, use ``defaultSystemInit`` and
``systemDeviceManager`` as in the following code:

.. code:: haskell

   {-# LANGUAGE BlockArguments #-}

   import Haskus.System
   
   main :: IO ()
   main = runSys do
   
      sys  <- defaultSystemInit
      term <- defaultTerminal
      let dm = systemDeviceManager sys
   
      inputDevs   <- listDevicesWithClass dm "input"
      graphicDevs <- listDevicesWithClass dm "drm"
   
      let
         showDev dev = writeStrLn term ("  - " <> show (fst dev))
         showDevs    = mapM_ showDev
   
      writeStrLn term "Input devices:"
      showDevs inputDevs
   
      writeStrLn term "Display devices:"
      showDevs graphicDevs
   
      powerOff

Linux associates a class to each device. The previous code shows how to
enumerate devices of two classes: "input" and "drm" (direct rendering manager,
i.e., display devices). If you execute it in ``QEMU`` you should obtain results
similar to:

.. code::

   Input devices:
     - "/virtual/input/mice"
     - "/LNXSYSTM:00/LNXPWRBN:00/input/input0/event0"
     - "/platform/i8042/serio0/input/input1/event1"
   Display devices:
     - "/pci0000:00/0000:00:01.0/drm/card0"
     - "/pci0000:00/0000:00:01.0/drm/controlD64"

To be precise, we are not listing devices but event sources: a single device may
have multiple event sources; some event sources may be virtual (for instance the
``mice`` input device is a virtual device that multiplexes all the mouse device
event sources and that is useful if you have more than one connected mouse
devices).

