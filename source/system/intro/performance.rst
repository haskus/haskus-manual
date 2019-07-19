==============================================================================
Performance
==============================================================================

Using a high-level language such as Haskell is a trade-off between performance
and productivity. Just like using C language instead of plain assembly language
is. Moreover in both cases we expect the compilers to perform optimizations that
are not obvious or that would require complicated hard to maintain codes if they
were to be coded explicitly.

GHC is the Haskell compiler we use. It is a mature compiler still actively
developed. It performs a lot of optimizations. In particular, it performs
inter-modules optimizations so that well-organized modular code doesn't endure
performance costs.

Haskell codes are compiled into native code for the architecture (i.e., there is
no runtime interpretation of the code). In addition, it is possible to use LLVM
as a GHC backend to generate the native code.

The generated native codes are linked with a runtime system provided by GHC that
manages:

* Memory: garbage collection
* Threading: fast and cheap user-space threading
* Software transactional memory (STM): safe memory locking
* Asynchronous I/O: non-blocking I/O interacting with the threading system

Performance-wise, this is a crucial part of the stack we use. It has been
carefully optimized and it is tunable for specific needs. It is composed of
about 40k lines of C code.

As a last resort, it is still possible to call codes written in other languages
from Haskell through the Foreign Function Interface (FFI) or by adding a Primary
Operation (primop). ``haskus-system`` uses these mechanisms to interact with
the Linux kernel.

**Discussion**

It seems to us that this approach is a good trade-off. As comparison points,
most UNIX-like systems rely on unsafe interpreted shell scripts (init systems,
etc.); Google's Android (with Dalvik) used to perform runtime bytecode
interpretation and then just-in-time compilation, currently (with ART) it still
uses a garbage collector; Apple's platforms rely on a garbage collection variant
called "automatic reference counting" in Objective-C and in Swift languages
(while it might be more efficient, it requires much more care from the
programmers); JavaScript based applications and applets (unsafe language, VM,
etc.) tend to generalize even on desktop.
