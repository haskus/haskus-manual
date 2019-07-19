=====================================================================
Using Devices
=====================================================================

To use a device, we need to get a handle (i.e., a reference) on it that we will
pass to every function applicable to it. See the code `here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/device-open/Main.hs>`_.

This code reads a 64-bit word from the ``urandom`` device that returns random data
and another from the ``zero`` device that returns bytes set to 0. Finally, we
write a string into the ``null`` device that discards what is written into it.
These three devices are virtual and are always available with Linux's default
configuration.

**Device Specific Interfaces**

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



