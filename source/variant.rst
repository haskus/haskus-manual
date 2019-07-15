.. _variant:

==============================================================================
Variant
==============================================================================

``Variant`` is a sum type, i.e. a wrapper for a value which can be of different
types. For instance in the following code ``x`` is a variant whose value can be
an ``Int``, a ``Float`` or a ``String``:

.. code::

   import Haskus.Utils.Variant
   
   x :: V '[Int,Float,String]

We use a type-level list of types to statically constrain the possible value
types. Compared to usual sum types (e.g. ``Either Int Float``) it allows us to
have variants which can contain any number of types and to manipulate
(extend/filter/etc.) the list in a type-safe way and without requiring new data
types.

.. toctree::
   :maxdepth: 1
   :numbered:

   variant/intro
   variant/setting_values
   variant/safe_pattern_matching
   variant/generic_functions
   variant/converting
   variant/updating
   variant/rebindable_syntax

**See also**

* recursive sum types based on ``Variant`` are also supported and are called
  :ref:`EADT <eadt>`

* :ref:`VEither <veither>` is a variant biased towards the first type in the
  list, just like ``Either a b`` is biased towards the second type (``b``)
  allowing instances such as ``instance Functor (Either a)``.

