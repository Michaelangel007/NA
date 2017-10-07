Nox Archaist
Copyright (C) 2016, 6502 Workshop, LLC. All Rights Reserved.


Pre-Alpha Release 0.12
----------------------------


To play the game engine demo
      *Floppy: boot with na.program.dsk in drive 1, na.main.player.dsk in drive 2.
      *Hard Disk: copy files from both disk images to hard disk subdirectory, and type "-noxarch.system" in that subdirectory.

-New Commands Since Last Release

(P)ush object
(O)pen/operate lever
(T)alk


-Misc Gameplaynotes

*There is an "A" tile in the town. It is currently the only pushable object in the game. Use the (P)ush command + arrow key
to move it around the screen.



-Technical Notes:

*Apple IIe or later with 128k RAM

*6502 mode recommended in emualators. Due to a bug, if the game hits an error trap, sometimes a screen of garbage is displayed
instead of the monitor. 

*Testing has been done primarily on AppleWIN 1.25.0.3 (6502 mode)- 8 & a physical Unenhanced Apple IIe /w 128k



-Distribution Notes:


Post to comp.sys.apple2.programmer on 9/11/2016


Known Issues:

*Mobs generate in the water/ocean sometimes which look like they might not belong there. We don't have graphics or collision controls
yet for seas mobs, so in the meantime we had some fun with it. 

*No limit on player swimming distance. The player will be able to swim in the final release, but there will
be significant limits. Ships will still play an important role. 

*We have an uninvited door ghost. Doors occasionally open by themselves.






