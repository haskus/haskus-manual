Creating ramdisks
-----------------

To execute a system with the ``Linux`` kernel, the easiest way is to create a
ramdisk: an image of the file-system containing your system (basically a ``zip``
file).

To do that, put your system files into a directory ``/path/to/my/system``. Then
execute:

.. code:: bash

   (cd /path/to/my/system ; find . | cpio -o -H newc | gzip) > myimage.img

You need to have the ``cpio`` and ``gzip`` programs installed. It builds a
ramdisk file named ``myimage.img`` in the current directory.


**Automated method**

The `Build.hs
<http://github.com/haskus/haskus-system-examples/tree/master/src/Build.hs>`_
program in the `haskus-system-examples
<http://github.com/haskus/haskus-system-examples>`_ repository can do this
automatically. To build the ramdisk for the ``Demo`` example, call:

.. code:: bash

   ./build.sh _build/ramdisks/Demo.img
