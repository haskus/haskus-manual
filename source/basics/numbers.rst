.. _numbers:

==============================================================================
Numbers
==============================================================================



------------------------------------------------------------------------------
Natural numbers
------------------------------------------------------------------------------

Natural numbers are non-integer integers (0, 1, 2...). If we use a finite
numbers of bits to encode natural numbers, we can only encode a subset of the
infinite natural number set.

+---------------+--------------------------+
| # bits        | Natural number range     |
+---------------+--------------------------+
| 1             | [0..1]                   |
+---------------+--------------------------+
| 2             | [0..3]                   |
+---------------+--------------------------+
| 3             | [0..7]                   |
+---------------+--------------------------+
| n             | [0..(2^n)-1]             |
+---------------+--------------------------+

``W n`` is the type of natural numbers encoded on ``n`` bits. We can define such
numbers with as follow:

.. code:: haskell

   import Haskus.Format.Number

   > let x = W @5 3
   > :t x
   x :: W 5

Note that if we try to encode an out-of-range natural, it fails:

.. code:: haskell

   > W @50 10000000000000000000000
   W @50 *** Exception: `10000000000000000000000` is out of the range of values
   that can be encoded by a 50-bit natural number: [0..1125899906842623]

Operations on natural numbers track the numbers of bits:

.. code:: haskell

   > W @4 10 .+. W @2 1
   W @5 11

   > W @4 10 .*. W @3 2
   W @7 20

   >  W @5 25 .-. W @2 3
   Just (W @5 22)

   > W @5 2 .-. W @2 3
   Nothing  -- not a Natural (negative number)

   > W @5 25 ./. W @2 3
   Just (W @5 8,W @2 1) -- 25 = 8*3 + 1

Note that we track the statically determined number of bits, not the optimal
one! For example, in ``W @4 10 .+. W @2 1 ==> W @5 11`` the result is ``11``
which can be encoded with a 4-bit natural but the return type is a 5-bit natural
because adding a 4-bit natural and a 2-bit natural *may* result in a 5-bit
natural: e.g. ``W @4 15 .+. W @2 1 ==> W @5 16`` and 16 can't be encoded with a
4-bit natural.

Explicitly specifying the number of bits required to store a literal natural
value can be cumbersome so we can make GHC do it for us with the ``nat``
function:

.. code:: haskell

   > nat @0
   W @1 0

   > nat @5
   W @3 5

   > nat @158748521123465897456465
   W @78 158748521123465897456465
