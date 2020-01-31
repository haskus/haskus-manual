Durability and Evolution
------------------------

Our approach should be both durable and evolutive. Durable because we only use
mature technology: Linux and GHC developments both started in early 1990s and
are still very active. The only new layer in the stack is ``haskus-system``
framework.  All of these are open-source free software, ensuring long-term
access to the sources.

The approach is evolutive: Haskell language is evolving in a controlled way with
GHC's extensions (and a potential future Haskell standard revision); GHC as a
compiler and a runtime system is constantly improving and support for new
architectures could be added; Linux support for new hardware and new
architectures is constantly enhanced and specific developments could be done to
add features useful for ``haskus-system`` (or your own system on top of it).

``haskus-system`` framework itself is highly evolutive. First it is new and
not tied to any standard. Moreover code refactoring in Haskell is much easier
than in low-level languages such as C (thanks to the strong typing), hence we
can easily enhance the framework interfaces as user code can easily be adapted.


