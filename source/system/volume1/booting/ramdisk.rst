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
