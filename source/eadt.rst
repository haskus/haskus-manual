.. _eadt:

==============================================================================
Extensible ADT (EADT)
==============================================================================

EADTs are "extensible algebraic data types": they can be transformed (by adding
or removing constructors) and their constructors are not tied to a specific EADT
type, hence we can use them as constructors of different EADTs.

EADT constructors and operations can be defined independently (even in different
modules) allowing a great modularity. As such there are an answer to the
"expression problem" (cf :ref:`Background <eadt_background>` section).

.. toctree::
   :maxdepth: 1
   :numbered:

   eadt/intro
   eadt/basics
   eadt/explicit_traversal
   eadt/constructor_constraint
   eadt/recursion_schemes
   eadt/safe_pattern_matching
   eadt/constructor_removal
   eadt/background
   eadt/examples
