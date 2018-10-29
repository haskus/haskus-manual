.. _variant:

==============================================================================
Variant
==============================================================================

``Variant`` is a sum type, i.e. a wrapper for a value which can be of different
types. For instance in the following code ``x`` is a variant whose value can be
an ``Int`` or a ``Float``:

.. code::

   import Haskus.Utils.Variant
   
   x :: V '[Int,Float]

The particularity is that we use a type-level list of types to control the possible value type. Compared to usual sum types (e.g. ``Either Int Float``) it allows us to manipulate (extend/filter/etc.) the list in a type-safe way and without requiring new data types.

Recursive sum types (e.g. ``data List a = Nil | Cons a (List a)``) based on
``Variant`` are also supported and are called :ref:`EADT <eadt>` (for "extensible ADT").

.. toctree::
   :maxdepth: 1
   :numbered:

   variant/intro
   variant/setting_values
   variant/safe_pattern_matching
   variant/generic_functions
   variant/converting
   variant/updating
