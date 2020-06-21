# ACE-DOS-ROM
Deep Thought Disc Drive System ROM teared apart
by McKlaud / June 2020

The listing aavalable at Jupiter Ace Archive website (http://www.jupiter-ace.co.uk/acedos-dt_rom.html) is not complete. There was therefore the need of review, update and minor correction of the source code. 

Jupiter Ace BOOT ROM 8K  
The Patched ROM used with Deep Thought Disc Interface & DOS ROM


Also BootROM.bin file available there (http://www.jupiter-ace.co.uk/downloads/JA-BOOTROM.zip) requires a minor tweak to make it running corectly. The last byte in this binary must be changed to $00 (NOP). If it will be felt with $6B the VLIST shows 36 bytes of garbage.
