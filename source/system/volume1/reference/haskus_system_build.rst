Reference: ``haskus-system-build``
==================================

This is the reference for the ``haskus-system-build`` program.

Commands:

* ``init``: create a new project from a template

   * ``--template`` or ``-t`` (optional): template name

* ``build``: build the project and its dependencies (Linux)

* ``test``: launch the project into QEMU

   * ``--init`` (optional): specify an init program (override ``ramdisk.init``
     in ``system.yaml``)

* ``make-disk``: create a directory containing the whole system

   * ``--output`` or ``-o`` (mandatory): output directory

* ``make-iso``: create an ISO image of the system

* ``test-iso``: test the ISO image with QEMU

* ``make-device``: install the system on a device

   * ``--device`` or ``-d`` (mandatory): device path (e.g., /dev/sdd). For now,
     the first partition is used as a boot partition.
