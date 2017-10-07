NOTE: to convert back to this version, NoxArchmain must be loaded to $1000, and used to read/launch loader.P at $2000. 
Enable the $Fx zero page variables for ProRWTS and disable the $5x zero page variables. 




---original notes----
First stable version that support the read byte length and seek.

Read, write, seek and get file length all tested. 


Tested successfully on the following platform, in unenhanced IIe mode. Read, write, seek were all tested, get file length
only had minimal testing on AppleWIN HD. 


AppleWIN harddive
CFFA Harddrive
AppleWIN floppy (disk 1 & 2)
Virtual II floppy (disk 1 & 2)
Real floppy (disk 1 & 2)
CFFA floppy (disk 1 & 2)
Mame floppy (disk 1 & 2)



