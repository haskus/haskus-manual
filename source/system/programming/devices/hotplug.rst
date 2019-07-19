=====================================================================
Plug-And-Play (Hot-Pluggable) Devices
=====================================================================

We are now accustomed to (un)plug devices into computers while they are running
and to expect them to be immediately detected and usable (i.e., without
rebooting). For instance input devices (keyboards, mice, joysticks, etc.) or
mass storages. The operating system has to signal when a new device becomes
available or unavailable.

   Linux loads some drivers asynchronously to speed up the boot.  Hence devices
   handled by these drivers are detected after the boot as if they had just been
   plugged in.

``haskus-system`` provides an interface to receive events when the state of the
device tree changes. The following code shows how to get and print these
events:

.. code:: haskell

   {-# LANGUAGE BlockArguments #-}

   import Haskus.System
   
   main :: IO ()
   main = runSys do
   
      term <- defaultTerminal
      sys  <- defaultSystemInit
      let dm = systemDeviceManager sys
   
      -- Display kernel events
      onEvent (dmEvents dm) \ev ->
         writeStrLn term (show ev)
   
      waitForKey term
      powerOff

If you execute this code in ``QEMU``, you should get something similar to:

.. code:: haskell

   -- Formatting has been enhanced for readability
   KernelEvent
      { kernelEventAction = ActionAdd
      , kernelEventDevPath = "/devices/platform/i8042/serio1/input/input3"
      , kernelEventSubSystem = "input"
      , kernelEventDetails = fromList
         [("EV","7")
         ,("KEY","1f0000 0 0 00")
         ,("MODALIAS","input:b0011v0002p0006e0000-e0,...,8,amlsfw")
         ,("NAME","\"ImExPS/2Generic ExplorerMouse\"")
         ,("PHYS","\"isa0060/serio1/input0\"")
         ,("PRODUCT","11/2/6/0")
         ,("PROP","1")
         ,("REL","143")
         ,("SEQNUM","850")]}
   KernelEvent
      { kernelEventAction = ActionAdd
      , kernelEventDevPath = "/devices/platform/i8042/serio1/input/input3/mouse0"
      , kernelEventSubSystem = "input"
      , kernelEventDetails = fromList
         [("DEVNAME","input/mouse0")
         ,("MAJOR","13")
         ,("MINOR","32")
         ,("SEQNUM","851")]}
   KernelEvent
      { kernelEventAction = ActionAdd
      , kernelEventDevPath = "/devices/platform/i8042/serio1/input/input3/event2"
      , kernelEventSubSystem = "input"
      , kernelEventDetails = fromList
         [("DEVNAME","input/event2")
         ,("MAJOR","13")
         ,("MINOR","66")
         ,("SEQNUM","852")]}
   KernelEvent
      { kernelEventAction = ActionChange
      , kernelEventDevPath = "/devices/platform/regulatory.0"
      , kernelEventSubSystem = "platform"
      , kernelEventDetails = fromList
         [("COUNTRY","00")
         ,("MODALIAS","platform:regulatory")
         ,("SEQNUM","853")]}


The three first events are due to Linux lazily loading the driver for the mouse.
The last event is Linux asking the user-space to load the wireless regulatory
information.

