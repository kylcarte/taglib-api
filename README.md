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

The library is split into two modules, Audio.TagLib, and Audio.TagLib.Internal.

Audio.TagLib provides a memory-managing interface. The function 'taglib' embeds
a TagLib computation into the IO monad. Files may be opened with 'openFile', and
their metadata read and written with the provided functions. When the block is
finished, all open files are saved, and all memory allocated will be freed.

Audio.TagLib.Internal provides bindings for all functions necessary for manual
management of TagLib memory. 'runTagLib' embeds a TagLib computation into the IO
monad, but it takes an initial TagLibEnv and returns the final TagLibEnv along
with the result of the computation.

Any changes made to audio files will not be saved unless 'cleanupFile' is called
on their 'Ptr File'. Additionally, TagLib allocates space in order to return its
string values, and these will remain allocated unless freed by the managed
'taglib' function, or by the manual 'freeTagLibStrings' function.

