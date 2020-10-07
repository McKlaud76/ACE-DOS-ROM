# Jupiter ACE DOS ROM
Deep Thought Disc Drive System ROM teared apart
by McKlaud / June 2020

![](/images/J_ACE_DT_DOS.jpg)

The listing avalable at Jupiter Ace Archive website (http://www.jupiter-ace.co.uk/acedos-dt_rom.html) is not complete. There was therefore the need of review, update and minor correction of the DOS source code. 

Also JA-BootROM.bin file available there (http://www.jupiter-ace.co.uk/downloads/JA-BOOTROM.zip) requires a minor tweak to make it running corectly. The last byte in the binary must be changed to $00 (NOP). If it is left with the present value of $6B the VLIST will show 36 bytes of garbage.
