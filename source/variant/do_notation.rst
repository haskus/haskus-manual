==============================================================================
Do-notation
==============================================================================

There are several ways we can use do-notation with Variant as we would with
``Maybe`` or ``Either`` to handle errors in control-flow for instance.

We recommend the :ref:`FlowT approach <variant_do_notation_flowt>` as it doesn't
use the ``RebindableSyntax`` extension and it is the most concise/expressive
approach.

.. toctree::
   :maxdepth: 1

   do_notation/flowt
   do_notation/either
   do_notation/rebindable_syntax
