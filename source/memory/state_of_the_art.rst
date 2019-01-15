==============================================================================
State of the art
==============================================================================

State of the art as of January 14, 2019.

* Data.ByteString.Short:
   ShortByteString ~ BufferI

* Data.ByteString.Strict:
   ByteString ~ BufferSlice ({-# UNPACK #-} !BufferP)


* ByteString: misleading name (not a textual string). Especially because of
   Data.ByteString.Char8.{pack/unpack}


* Data.ByteString.Lazy: BufferList BufferP
   Lazy IO: quite bad
   Fixed chunck size

* ByteString: embedding as literal strings (slow, don't support \0 in the
  buffer)

* ByteArray, MutableByteArray

* Ptr, ForeignPtr: don't track buffer size
