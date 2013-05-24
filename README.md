taglib-api
==========

An FFI layer over TagLib's C bindings that provides functions for reading and
writing metadata for a variety of common audio formats.

It also manages taglib files and strings, automatically freeing allocations when
computation is finished.

TagLib currently supports:
* ID3v1 and ID3v2 for MP3 files
* Ogg Vorbis comments
* ID3 tags and Vorbis comments in FLAC, MPC, Speex, WavPack
  TrueAudio, WAV, AIFF, MP4 and ASF files.

