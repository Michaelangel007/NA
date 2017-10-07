$1000 byte size limit on NOXARCH.MAIN removed.

I haven't found any bugs, but I only did a small amount of testing on AppleWIN floppy and hard disk.

I stopped converting the game code to elimiante LOADER.P when I realized that LOADER.P, clobbers $C00-$1FFF where NOARCH.MAIN
resides. That's a problem if NOXARCH.MAIN takes over the duties of LOADER.P
