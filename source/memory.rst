==========================
Explicit memory management
==========================

This document is about explicit ("manual") memory management in Haskell. It
starts from the very low level memory allocation and buffer management and then
builds up to "pointer" management, binary writer/reader monads, binary
serialization of Haskell data, large file mapping, embedding of large binary
blobs into applications, etc.

WORK-IN-PROGRESS: we only cover the first topics for now

.. toctree::
   :maxdepth: 1
   :numbered:

   memory/intro
   memory/buffer
   # memory/state_of_the_art
