X86 encoding
============

Mode
----

An x86-64 architecture can run in 5 different execution modes, excluding System
Management Mode (SMM).

Different flags from different registers are used to indicate the current mode.
We synthesize them into a virtual ``M`` flag.

.. image:: /_static/images/system/x86/mode.svg
   :class: img_center

Operation, address and stack sizes
----------------------------------

Default size
~~~~~~~~~~~~

The default operation size (DOS) and the default address size (DAS) depend on the
execution mode (M) and on the D/B flag in the CS segment descriptor.

.. image:: /_static/images/system/x86/default_size.svg
   :class: img_center

Similarly, the defaut stack size (DSS) depends on the execution mode and on the
D/B flag in the SS segment descriptor.

.. image:: /_static/images/system/x86/default_stack_size.svg
   :class: img_center

Overridden size
~~~~~~~~~~~~~~~

The DOS and the DAS can be overridden per instruction with the 66 and 67 prefixes
respectively, giving use the overridden operation size (OOS) and the overridden
address size (OAS).

.. image:: /_static/images/system/x86/overridden_size.svg
   :class: img_center

Effective size
~~~~~~~~~~~~~~

Finally, in 64-bit execution mode (M=4), some instructions defaults to a 64-bit
operation size and a new ``W`` prefix can be used to enforce 64-bit operation size.
This gives us the effective operation size (EOS).

.. image:: /_static/images/system/x86/effective_size.svg
   :class: img_center

The effective address size (EAS) is always equal to overridden address size: EAS
= OAS.

Full poster
-----------

.. image:: /_static/images/system/x86/encoding.svg
   :class: img_center
