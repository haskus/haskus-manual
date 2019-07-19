.. _device-management:

Device management
-----------------

Device management is the entry-point of system programming. Programs have to
know which devices are available to communicate with the user (graphic cards,
input devices, etc.) or with other machines (network cards, etc.).

In this chapter, we present the basic concepts and we show examples with simple
virtual devices provided by Linux. In the next chapters, we build on these
concepts and we show how to use specific device classes: display devices, input
devices, etc.

Enumerating Available Devices
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Plug-And-Play (Hot-Pluggable) Devices
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Using Devices
~~~~~~~~~~~~~

To use a device, we need to get a handle (i.e., a reference) on it that we will
pass to every function applicable to it. See the code `here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/device-open/Main.hs>`_.

This code reads a 64-bit word from the ``urandom`` device that returns random data
and another from the ``zero`` device that returns bytes set to 0. Finally, we
write a string into the ``null`` device that discards what is written into it.
These three devices are virtual and are always available with Linux's default
configuration.

Device Specific Interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous code example we have used read and write methods as if the
device handle had been a normal file handle. Indeed Linux device drivers define
the operational semantics they want to give to each system call applicable to a
file handle: ``read``, ``write``, ``fseek``, ``mmap``, ``close``, etc. Some
system calls may be invalid with some device handles (e.g., ``write`` with the
``urandom`` driver).

This gives a weak unified interface to device drivers: the system calls are the
same but the operational semantics depends on the driver. Moreover there are a
lot of corner cases, such as system call parameters or flags only valid for some
drivers. Finally, as there aren't enough "generic" system calls to cover the
whole spectrum of device features, the ``ioctl`` system call is used to send
device specific commands to drivers. In practice you really have to know which
device driver you're working with to ensure that you use appropriate system
calls.

To catch up as many errors at compile time as possible, in ``haskus-system`` we
provide device specific interfaces that hide all this complexity. If you use
them, you minimise the risk of accidentally using an invalid system call. Some
of these interfaces are presented in the next chapters. Nevertheless you will
have to use the low-level interface presented in this chapter if you want to
write your own high-level interface to a device class not supported by
``haskus-system`` or if you want to extend an existing one.


Implementation notes
~~~~~~~~~~~~~~~~~~~~

Internally ``haskus-system`` mounts a ``sysfs`` virtual file system through
which the Linux kernel exposes the hardware of the machine. In this file-system
each device is exposed as a sub-directory in the ``/devices`` directory and the
path to the device's directory uniquely identifies the device in the system.

Directory nesting represents the device hierarchy as the system sees it.
Regular files in device directories represent device properties that can be
read and sometimes written into from user-space.  Sometimes, when the tree
relationship between devices is not sufficient, relations between devices are
represented as symbolic links.
   
File descriptor vs Handle
^^^^^^^^^^^^^^^^^^^^^^^^^

Linux allows programs in user-space to have handles on kernel objects.
Suppose the kernel has an object ``A`` and a reference ``R_A`` on ``A``.  Instead of
directly giving ``R_A`` to user-space processes, the kernel maintains a
per-process array of kernel object references: ``D_pid`` for the process with
the ``pid`` identifier.  To "give" ``R_A`` to this process, the kernel finds
an empty cell in ``D_pid``, put ``R_A`` in it and gives the index of the cell to
the process.

For historical reasons, the cell index is called a file descriptor and ``D_pid``
a file descriptor table even if in Linux they can be used for kernel objects
that are not files (e.g., clocks, memory). User-space processes can only refer
to kernel objects through theses indirect references. Note that the file
descriptor table is specific to each process: sharing a file descriptor with
another process does not allow to share the referred kernel object.

In ``haskus-system`` we use the term "handle" instead of "file descriptor" as we
find it less misleading.


Device special files and /dev
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Ideally there would be a system call to get a handle on a device by providing
its unique identifier (similarly to the ``getDevieHandleByName`` API provided by
``haskus-system``). Sadly it's not the case. We have to:

1. Get the unique device triple identifier from its name

   Linux has two ways to uniquely identify devices:
   
   * a path in ``/devices`` in the ``sysfs`` file-system
   * a triple: a major number, a minor number and a device type (``character`` or
     ``block``).

   ``haskus-system`` retrieves the triple by reading different files the the
   ``sysfs`` device directory.

2. Create and open a device special file

   With a device triple we can create a special file (using the ``mknod`` system
   call).
   
   ``haskus-system`` creates the device special file in a virtual file system
   (``tmpfs``), then opens it and finally deletes it.

Usual Linux distributions use a virtual file-system mounted in ``/dev`` and
create device special files in it. They let some applications directly access
device special files in ``/dev`` (e.g., X11). Access control is ensured by file
permissions (user, user groups, etc.). We don't want to do this in
``haskus-system``: we provide high-level APIs instead.


Netlink socket
^^^^^^^^^^^^^^

Linux dynamically adds and removes files and directories in the ``sysfs``
file-system, when devices are plugged or unplugged. To signal it to user-space,
it sends kernel events in a Netlink socket. The Netlink socket is also used to
pass some other messages, for instance when the kernel wants to ask something to
the user-space. ``haskus-system`` handles a Netlink socket, parses received
kernel events and delivers them through a STM broadcast channel.

In usual Linux distributions, a daemon called ``udev`` is responsible of
handling these kernel events. Rules can be written to react to specific events.
In particular, ``udev`` is responsible of creating device special file in the
``/dev`` directory. The naming of theses special files is a big deal for these
distributions as applications use them directly afterwards and don't use the
other unique device identifiers (i.e., the device path in the ``sysfs``
file-system).  In ``haskus-system``, high-level APIs are provided to avoid
direct references to device special files.


Miscellaneous
^^^^^^^^^^^^^

In usual Linux distributions, ``udev`` (``man 7 udev``) is responsible of
handling devices. It reads ``sysfs`` and listens to kernel events to create and
remove device nodes in the ``/dev`` directory, following customizable rules.  It
can also execute custom commands (``crda``, etc.) to respond to kernel requests.
