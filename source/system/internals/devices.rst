=====================================================================
Device management
=====================================================================

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
-------------------------

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
-----------------------------

Ideally there would be a system call to get a handle on a device by providing
its unique identifier (similarly to the ``getDeviceHandleByName`` API provided by
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
--------------

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
-------------

In usual Linux distributions, ``udev`` (``man 7 udev``) is responsible of
handling devices. It reads ``sysfs`` and listens to kernel events to create and
remove device nodes in the ``/dev`` directory, following customizable rules.  It
can also execute custom commands (``crda``, etc.) to respond to kernel requests.

