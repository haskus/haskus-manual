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
