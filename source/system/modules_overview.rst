Modules Overview
================

The code base of ``haskus-system`` is becoming quite large. This page gives an
overview of the different modules.

Binary modules
~~~~~~~~~~~~~~

``haskus-system`` handles low-level data structures in memory such as C structs,
unions, enums, bit fields, etc. Deliberately it doesn't depend on C header files
(.h) and doesn't use preprocessors (cpp2hs, hsc2hs, etc.).

* `Haskus.Format.Binary
  <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Format/Binary>`_:
  modules to manipulate binary data and to easily create C bindings (see the
  `binary documentation </system/manual/binary>`_)

Interface with the Linux kernel
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``haskus-system`` provides foreign primops to call Linux system calls from Haskell
code without going through the libc. In addition to basic system calls, it
provides wrappers for some Linux subsystems/features accessible through
multiplexing syscalls (e.g., ioctl) or through specific file systems (e.g.,
procfs, sysfs).

* `Haskus.Arch.Linux <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Arch/Linux>`_: system calls and low-level interfaces
* `Haskus.Arch.Linux.Input <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Arch/Linux/Input.hs>`_: input subsystem
* `Haskus.Arch.Linux.Graphics <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Arch/Linux/Graphics>`_: kms/drm subsystem

Formats
~~~~~~~

``haskus-system`` provides support for some file formats (e.g., ELF, DWARF, CPIO)
and some file system formats (e.g., ISO9660). These can be used to interact
with Linux (e.g., to look up for functions in the vDSO ELF image), to build
initramfs images or bootable disk images, etc.

* `Haskus.Format.Compression <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Format/Compression>`_: some compression algorithms and containers
* `Haskus.Format.CPIO <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Format/CPIO.hs>`_: CPIO archive format
* `Haskus.Format.Elf <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Format/Elf.hs>`_: ELF object format
* `Haskus.Format.Dwarf <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Format/Dwarf.hs>`_: DWARF debugging information format

Architectures
~~~~~~~~~~~~~

``haskus-system`` provides architecture specific modules (currently only for
x86-64), in particular the thin architecture specific layer to call Linux
system calls. Additionally, Haskus has a dictionnary of x86 instructions; it is
currently used to implement a disassembler and could be used to implement
assemblers, analyzers, emulators, etc. A wrapper for the x86's cpuid
instruction is also provided.

* `Haskus.Arch.X86_64
  <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Arch/X86_64>`_:
  Currently only X86-64 is supported (`x86 documentation </system/manual/x86>`_)

  * `Haskus.Arch.X86_64.ISA <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Arch/X86_64/ISA>`_: instruction set architecture
  * `Haskus.Arch.X86_64.Disassembler <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Arch/X86_64/Disassembler.hs>`_
  * `Haskus.Arch.X86_64.Linux <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Arch/X86_64/Linux>`_: arch-specific Linux interface (syscalls)
  * `Haskus.Arch.X86_64.Cpuid <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/Arch/X86_64/Cpuid.hs>`_: CPUID wrapper

System interface
~~~~~~~~~~~~~~~~

``haskus-system`` provides modules to interact with the system: input devices,
display devices, etc. These modules are used to easily build a custom system
without dealing directly with the low-level Linux interface. It also provides a
custom monad with common features for system programming (logging, etc.).

* `Haskus.System <http://github.com/haskus/haskus-system/tree/master/src/lib/Haskus/System>`_



