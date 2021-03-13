.. _numbers:

==============================================================================
Numbers
==============================================================================



------------------------------------------------------------------------------
Natural numbers
------------------------------------------------------------------------------

Natural numbers are non-negative integers (0, 1, 2...). If we use a finite
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

``BitNat n`` is the type of natural numbers encoded on ``n`` bits. We can define such
numbers as follow:

.. code:: haskell

   import Haskus.Format.Number

   > let x = BitNat @5 3
   > :t x
   x :: BitNat 5

Note that if we try to encode an out-of-range natural, it fails:

.. code:: haskell

   > BitNat @50 10000000000000000000000
   BitNat @50 *** Exception: `10000000000000000000000` is out of the range of values
   that can be encoded by a 50-bit natural number: [0..1125899906842623]

Operations on natural numbers track the numbers of bits:

.. code:: haskell

   > BitNat @4 10 .+. BitNat @2 1
   BitNat @5 11

   > BitNat @4 10 .*. BitNat @3 2
   BitNat @7 20

   >  BitNat @5 25 .-. BitNat @2 3
   Just (BitNat @5 22)

   > BitNat @5 2 .-. BitNat @2 3
   Nothing  -- not a Natural (negative number)

   > BitNat @5 25 ./. BitNat @2 3
   Just (BitNat @5 8,BitNat @2 1) -- 25 = 8*3 + 1

Note that we track the statically determined number of bits, not the optimal
one! For example, in ``BitNat @4 10 .+. BitNat @2 1 ==> BitNat @5 11`` the
result is ``11`` which can be encoded with a 4-bit natural but the return type
is a 5-bit natural because adding a 4-bit natural and a 2-bit natural *may*
result in a 5-bit natural: e.g. ``BitNat @4 15 .+. BitNat @2 1 ==> BitNat @5
16`` and 16 can't be encoded with a 4-bit natural.

Explicitly specifying the number of bits required to store a literal natural
value can be cumbersome so we can make GHC do it for us with the ``nat``
function:

.. code:: haskell

   > nat @0
   BitNat @1 0

   > nat @5
   BitNat @3 5

   > nat @158748521123465897456465
   BitNat @78 158748521123465897456465

------------------------------------------------------------------------------
Natural ranges
------------------------------------------------------------------------------

Sometimes we know that the natural numbers that we manipulate are in a fixed
range:

* An hour is in the range [1..12]
* The age of an adult is in the range [18..150]

We can use a natural range to store those values: it ensures that the value is
in the range and it uses just the necessary bits to store (max-min+1) values.

.. code:: haskell

   > type Age = NatRange 18 150
   > natRange @25 :: Age
   NatRange @18 @150 25

   > natRange @16 :: Age
   error: 16 isn't in the range [18,150]

Operations on natural ranges track range boundaries:

.. code:: haskell

   > NatRange @2 @4 3 .++. NatRange @7 @17 13
   NatRange @9 @21 16
