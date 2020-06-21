§
Deep Thought ACE DOS code listing.
The DOS system was written by Jeff Shepherd.

; *********************************************************
; ***	                                                ***
; ***	            Jump Table                          ***
; ***	                                                ***
; *********************************************************

Find-index          JP $F900          ; $F800
Find-header         JP $F923          ; $F803
Save-block          JP $F953          ; $F806
Load-block          JP $F9D0          ; $F809
Wait                JP $FA03          ; $F80C
On                  JP $FA0D          ; $F80F
Off                 JP $FA24          ; $F812
Setup               JP $FA2B          ; $F815
Step                JP $FA64          ; $F818
Step-out            JP $FA62          ; $F81B
DRIVE               JP $FA74          ; $F81E
Print               JP $FA86          ; $F821
Track00             JP $FAAS          ; $F824
Save-cat            JP $FAB1          ; $F827
Load-cat            JP $FAC3          ; $F82A
Print-error         JP $FAE7          ; $F82D
XFORMAT             JP $FB0A          ; $F830
Save-file           JP $FB31          ; $F833
Load-file           JP $FB6D          ; $F836
Find-word           JP $FBA6          ; $F839
Enter-word          JP $FBDS          ; $F83C
Lookup-word         JP $FC18          ; $F83F
Hex-byte            JP $FC2C          ; $F842
Hex-char            JP $FC35          ; $F845
CAT                 JP $FC41          ; $F848
Dict-size           JP $FCCA          ; $F84B
Block-length        JP $FCD4          ; $F84E
Delete file         JP $FCDF          ; $F851
DELETE              JP $FD19          ; $F854
Error-?             JP $FD2A          ; $F857
Store               JP $FD35          ; $F85A
DSAVE               JP $FD65          ; $F85D
DBSAVE              JP $FD70          ; $F860
DBLOAD              JP $FD90          ; $F863
RESAVE              JP $FDB2          ; $F866
Word                JP $FDC3          ; $F869
MAP                 JP $FDD0          ; $F86C
DLOAD               JP $F880          ; $F872
SCRATCH             JP $F8D5          ; $F875
Fill-length         JP $FDF0          ; $F878


; *********************************************************
; ***                                                   ***
; ***               Constants                           ***
; ***                                                   ***
; *********************************************************

Acia-status     EQU  $21        ; Acia status register.
Acia-in         EQU  $23        ; Acia data input register.
Acia-control    EQU  $01        ; Acia control register.
Acia-out        EQU  $03        ; Acia data output register.

Pia-a-i         EQU  $29        ; PIA port A data input.
Pia-a-o         EQU  $09        ; PIA port A data output.
Pia-b-o         EQU  $0D        ; PIA port B data output.
Pia-a-cr        EQU  $0B        ; PIA port A control register.
Pia-b-cr        EQU  $0F        ; PIA port B control register.

Dos-words       EQU  $FFFD      ; Address of name length field
                                ; of first DOS FORTH word.
Forth-link      EQU  $3C47      ; Address of the link field
                                ; of the word FORTH.
Cat-size        EQU  $F8FE      ; Address of the first of two
                                ; bytes holding the length
                                ; of the cat in bytes + 1.
Message-space   EQU  $FE30      ; Address of the first string
                                ; used by Print.
Pad             EQU  $2701      ; Address of the Forth PAD
RAMTOP          EQU  $3C18      ; Hold the address of the highest
                                ; byte used by the Jupiter Ace.
                                ; The drive number is stored at
                                ; (RAMTOP). The cat starts at
                                ; (RAMTOP) + 1.
                                ; where a word read by WORD
                                ; is placed.
Enter-forth     EQU  $4B9       ; Enter forth from machine code.
WORD            EQU  $5AB       ; Parameter field address of WORD.
End-forth       EQU  $1A0E      ; Forth word to enter Z-80 code.




; *********************************************************
; ***                                                   ***
; ***               Find-index                          ***
; ***                                                   ***
; *********************************************************


; * Waits for the index hole to pass.

; * No arguments or results
; * DE and HL preserved.
; * Can give "Wot no disk" or "Wot no index hole".

; * Calls: None
; * Called by: Find-header and Save-block.


            ; First, loop till high, then loop till low.

            LD BC,$7D00             ; Time out count.
Whilelo     DEC BC                  ; Decrease count
            LD A,B                  ; and test
            OR C                    ; for zero.
            LD A,3                  ; If zero then call Error-?
            CALL Z,Error-?          ; with 3 for "Wot no disk".
            IN A, (Pia-a-i)
            AND 1                   ; Test bit 0.
            JR Z,Whilelo            ; Loop till high.

            LD BC,$7D00             ; Time out count.
Whilehi     DEC BC                  ; Decrease count
            LD A,B                  ; and test
            OR C                    ; for zero.
            LD A,4                  ; Error 4 is
            CALL Z,Error-?          ; "Wot no index hole"
            IN A, (Pia-a-i)
            AND 1                   ; Test bit 0.
            JR NZ,Whilehi           ; Loop till low.
            RET




; *********************************************************
; ***                                                   ***
; ***               Find-header                         ***
; ***                                                   ***
; *********************************************************

; * Waits for the index hole to pass and then reads
; * the header which is a sequence of $FF's followed by
; * a 42.

; * A=0 if header found in time.
; * A=5 if header not found.
; * DE and HL preserved.
; * Can give "Wot no disk" or "Wot no index hole".

; * Calls: Find-index.
; * Called by: Load-block and Save-block.

            CALL Find-index
            LD BC,$7D00             ; Time out count.
 NotFF      DEC BC                  ; Decrease count
            LD A,B                  ; and test for zero.
            OR C
            LD A,5                  ; Return with 5 if zero.
            RET Z
            IN A,(Acia-status)
            AND 1                   ; Test Read-full bit.
            JR Z,NotFF              ; Loop if not ready.
            IN A,(Acia-in)          ; Read byte from disk.
            CP $FF
            JR NZ,NotFF             ; Loop while byte is FF.

IsFF        DEC BC                  ; Check for timeout
            LD A,B                  ; again as before.
            OR C                    ; Test for zero.
            LD A,5                  ; Return 5 for "Wot no header"
            RET Z                   ; if zero.
            IN A,(Acia-status)
            AND 1                   ; Test Read-full bit.
            JR Z,IsFF               ; Loop if not ready.
            IN A,(Acia-in)          ; Read byte from disk.
            CP $FF
            JR Z,IsFF               ; Loop while byte is FF.
            CP 42                   ; If its not FF and
            JR NZ,NotFF             ; its not 42, look for a FF again.
            XOR A                   ; Clear A (No error).
            RET




; *********************************************************
; ***                                                   ***
; ***               Save-block                          ***
; ***                                                   ***
; *********************************************************

; * Saves a single block of data and then verifies it against
; * memory. Assumes that the head is at the correct track.

; * On entry, HL = start address of data to be saved,
;             DE = number of bytes to be saved.

; * Calls: Wait, Find-header and Find-index.
; * Called by: Save-cat and Save-file.

            IN A,(Pia-a-i)          ; Test write-protect
            AND 4                   ; line.
            LD A,6                  ; Give "Disk is write protected"
            CALL Z,Error-?          ; if line low.
                                    ; (Assumes disk enabled).
            LD C,20                 ; Wait for 20ms,
            CALL Wait               ; for head to settle.

            DI                      ; Disable interrupts,
                                    ; Writing is time dependant.
            CALL Find-index

            PUSH DE                 ; Save start and length
            PUSH HL                 ; for verify.

            LD A,%10110111          ; Motor on, write enable.
            OUT (Pia-a-o),A

            LD C,2                  ; Wait for lms to set
            CALL Wait               ; blank at start of track.

            LD B,8                  ; Write 8 $FFs onto disk.
NextFF      IN A, (Acia-status)     ; Test ready for data.
            AND 2
            JR Z,NextFF             ; Loop till ACIA ready.
            LD A,$FF
            OUT (Acia-out),A
            DJNZ NextFF             ; Repeat for other bytes.

            ; B has been set to zero a this point as it
            ; is used as checksum.

Wait42      IN A,(Acia-status)      ; Write a single 42,
            AND 2                   ; first waiting
            JR Z,Wait42             ; for ACIA ready.
            LD A,42
            OUT (Acia-out),A

            ; Now store the data.

Mainst      IN A, (Acia-status)     ; Wait for
            AND 2                   ; ACIA ready.
            JR Z, Mainst

            LD A, (HL)              ; Get byte form data.
            OUT (Acia-out),A        ; Write it to disk.
            ADD B                   ; Add it to check sun so far,
            LD B,A
            INC HL                  ; Point to next byte in data.
            DEC DE                  ; One less byte to be written.
            LD A,D                  ; Loop
            OR E                    ; till
            JR NZ,Mainst            ; count zero.

            ; Write check sun at end.

Checkw      IN A, (Acia-status)     ; Wait for
            AND 2                   ; ACIA ready.
            JR Z,Checkw
            LD A,B                  ; Write checksum.
            OUT (Acia-out),A

            LD C,1                  ; Wait 1ms for ACIA to
            CALL Wait	            ; finish writing last bytes.

            LD A,%10111111          ; Motor on, write disable.
            OUT (Pia-a-o),A

            POP HL                  ; Recover start and length.
            POP DE

            ; Verify data.

            CALL Find-header
            CALL Error-?            ; Error if header not found.

Vloop       IN A, (Acia-status)     ; Wait for data
            LD C,A                  ; keeping status for possible
            AND 1                   ; error message.
            JR Z,Vloop

            IN A,(Acia-in)          ; Get byte from disk.
            CP (HL)                 ; Is it the same as memory ?
            LD A,2                  ; Raise "Verify error" if not.
            CALL NZ,Error-?

            LD A,C                  ; Test for error from ACIA.
            AND %1110000            ; Framing, parity or overrun.
            CALL NZ,Error-?

            INC HL                  ; Point to next byte in data.
            DEC DE                  ; One less byte to be checked.
            LD A,D                  ; Loop
            OR E                    ; till
            JR NZ,Vloop             ; count zero.

            ; Checksum not read.

            EI
            RET


; *********************************************************
; ***                                                   ***
; ***               Load-block                          ***
; ***                                                   ***
; *********************************************************

; * Loads a single block of data.
; * Assumes that the head is at the correct track.

; * On entry, HL = start address to load data at.
; *           DE = number of bytes to be loaded.
; *                 (must be the same as when saved).
; * On exit, A = error number, 0 if no error.

; * Calls: Wait and Find-header.
; * Called by: Load-cat and Load-file.

            LD C,20                 ; Wait for 20ms,
            CALL Wait               ; for head to settle.
            DI                      ; Disable interrupts,
                                    ; Loading is time dependant.

            CALL Find-header
            OR A                    ; Test for not error (=0).
            RET NZ                  ; Return if error in finding header.

            LD B,A                  ; B := 0, Zero checksum to start with.

Loadlop     IN A, (Acia-status)     ; Wait for data
            LD C,A                  ; keeping status for possible
            AND 1                   ; error message.
            JR Z,Loadlop

            IN A, (Acia-in)         ; Get byte from disk.
            LD (HL),A               ; Store it into memory.
            ADD A,B                 ; Checksum so far.
            LD B,A

            LD A,C                  ; Test for error from ACIA.
            AND %1110000            ; Framing, parity or overrun.
            JR NZ,IntoffR           ; Return if error.

            INC HL                  ; Point to next byte in data.
            DEC DE                  ; One less byte to be loaded.
            LD A,D                  ; Loop
            OR E                    ; till
            JR NZ,Loadlop           ; count zero.

            ; Load and check checksum.
            ; ACIA error not checked on checksum read.

LChLoop     IN A, (Acia-status)     ; Wait for data
            AND 1
            jR Z,LChLoop
            IN A, (Acia-in)         ; Checksum from disk.
            CP B                    ; Compare with sum calculated.
            ID A,1                  ; Error 1 if not the same.
            JR NZ,IntoffR
            XOR A                   ; A := 0 for no error.

IntoffR     EI
            RET




; *********************************************************
; ***                                                   ***
; ***               Wait                                ***
; ***                                                   ***
; *********************************************************

; * Waits a specified number of milli seconds.
; * Depends on a Z-80 clock of 3.25 MHz.

; * On entry, C is the time to be waited in ms.
; * BC,DE,HL and A are preserved.

; * Calls: None.
; * Called by: Load-block, Save-block, On and Step.

            PUSH BC                 ; Must be the same on exit.
Wait1       LD B,$FA                ; Loop lasting 1ms.
Wait        DJNZ Wait2
            DEC C                   ; Repeat the number in C times.
            JR NZ,Waitl
            POP BC
            RET


; *********************************************************
; ***                                                   ***
; ***               On                                  ***
; ***                                                   ***
; *********************************************************

; * Switchs the drive motor on and then, after a delay,
; * selects the drive.

; * No arguments or results.
: * Uses HL, A, C

; * Calls Wait.
; * Called by: Load-cat, XFORMAT.

            LD A,%10111111          ; Motor on.
            OUT (Pia-a-o),A

            LD C,255                ; Wait about half a second.
            CALL Wait
            CALL Wait

            LD HL, (RAMTOP)         ; Point to first byte
            LD A, (HL)              ; reserved for disk.
                                    ; This is setup by DRIVE.
            OUT (Pia-b-o),A         ; Select drive.
            LD C,$40                ; Wait for head load.
            JP Wait



; *********************************************************
; ***                                                   ***
; ***               Off                                 ***
; ***                                                   ***
; *********************************************************

; * Switchs the drive motor off and selects no drive.

; * No arguments or results.
; * Uses A only.

; * Calls: None.
; * Called by: XFORMAT,Save-cat,Error-?,CAT,LD and BLD.

            LD A,%11111111          ; All high (disabled).
            OUT (Pia-b-o),A         ; Deselect drive.
            OUT (Pia-a-o),A         ; Motor off.
            RET




; *********************************************************
; ***                                                   ***
; ***               Setup                               ***
; ***                                                   ***
; *********************************************************

; * Initializes PIA and ACIA,
; * and then allocates RAM for the cat and sets to drive 0.

; * Calls: Wait.
; * Called by: None.

            LD BC, (Catsize)        ; HL = top of detected RAM.
            OR A                    ; Clear carry.
            SBC HL, DE
            LD (RAMTOP),HL
            LD SP, HL
            LD (HL),$FD             ; Drive 0.
            LD C,100                ; Wait 0.1s for PIA reset
            CALL Wait               ; to be high.
            LD B,A

            ; First initialise PIA.

            XOR A                   ; A := 0
            OUT (Pia-a-cr),A        ; Set both control registers
            OUT (Pia-b-cr),A        ; to allow access to direction
                                    ; control registers.
            DEC A                   ; A := $FF
            OUT (Pia-b-o),A         ; All lines output for port B.
            LD A,%11111000          ; Three lines input for
            OUT (Pia-a-o),A         ; port A.
            LD A,4                  ; Set both control registers
            OUT (Pia-a-cr),A        ; to allow access to
            OUT (Pia-b-cr),A        ; data registers.
            LD A,$FF                ; Set all outputs high (inactive).
            OUT (Pia-a-o),A
            OUT (Pia-b-o),A

            ; Initialise ACIA.

            ID A,3                  ; Reset ACIA.
            OUT (Acia-control),A
            LD A,%11100             ; Set ACIA to 9-bit parity.
            OUT (Acia-control),A

            LD A,B
            JP $32                  ; Resume startup.







; *********************************************************
; ***                                                   ***
; ***               Step-out                            ***
; ***                                                   ***
; *********************************************************

; * Steps head one track further out.

; * No arguments or results.
; * DE and HL preserved.

; * Calls: Wait.
; * Called by: Save-cat, Load-cat, Save-file and Load-file.

                LD A,%10101111	    ; Direction and motor on low.

                ; Implicit CALL Step and RET.






; *********************************************************
; ***                                                   ***
; ***               Step                                ***
; ***                                                   ***
; *********************************************************

; * Steps head in or out depending on argument.

; * Step in if A = %10111111.
; * Step out if A = % 10101111.
; * DE and HL preserved.

; * Calls: Wait.
; * A Called by: Step-out and Track00.

            OUT (Pia-a-o),A         ; Specify direction.
            LD C,5                  ; Can do one step every 5ms.
            CALL Wait
            SUB %100000             ; Pull step low.
            OUT (Pia-a-o),A
            ADD %100000             ; And then high again.
            OUT (Pia-a-o),A
            RET



; *********************************************************
; ***                                                   ***
; ***               DRIVE                               ***
; ***                                                   ***
; *********************************************************

; * Forth word to select drive and side.
; * Least significant bit of number on parameter stack
; * gives side, next two bits give drive number.


                RST 24              ; Pop DE from parameter stack.

                ; Convert number to correct form for PIA register.
                ; A := not((E and 1) | (1 << (E >> 1))).

                LD A,%11111110
                LD B,E
                INC B
drlab1          RLCA
                DEC B
                JR Z,drlab2
                DJNZ drlabl
                DEC A

                ; Store it in the first byte above RAMTOP.

drlab2          LD HL, (RAMTOP)
                LD (HL),A
                JP (1Y)             ; Forth return.






; *********************************************************
; ***                                                   ***
; ***               Print                               ***
; ***                                                   ***
; *********************************************************

; * Prints a text message specified by a number in A.
; * A The text store used has message arranged consecutively
; * in order of increasing number.
; * 00 is used to separate message.
; * Characters with codes > 127 are interpreted as messages
; * to be inserted recursively.
; * This method of store is used to save memory as many
; * messages are similar.

; * On entry A= message number.

; * Calls: none.
; * Called by:

                LD DE,message-space ; Start of message store.
                LD B,A              ; Count 00's the number in A.
Prsrch          LD A,(DE)
                INC DE
                OR A                ; Test zero.
                JR NZ,Prsrch
                DJNZ Prsrch

Prloop          LD A,(DE)           ; First character of message.
                INC DE              ; point to next character.
                OR A                ; Test for zero which
                RET Z               ; marks the end.
                LD B,A              ; See if the top bit is set.
                AND $7F             ; Making a copy with the
                CP B                ; top bit clear at the same time.
                OR Z,Prwrch         ; If bit not set print as character.
                PUSH DE             ; Else call Print recursively.
                CALL Print
                POP DE
                JR Prloop

Prwrch          RST 8              ; Wrch.
                JR Prloop



; *********************************************************
; ***                                                   ***
; ***               Track00                             ***
; ***                                                   ***
; *********************************************************

; A Moves head to track zero.

; * No args or results.
; * DE and HL preserved.

: * Calls: Step.
; * A Called by: Save-cat and Load-cat.

Trk001p         IN A,(Pia-a-i)          ; Input lines
                AND 2                   ; track00 bit.
                RET Z                   ; Quit if already at track00.
                LD A,%10111111          ; For step in.
                CALL Step
                JR Trk001p              ; Test again.



; *********************************************************
; ***                                                   ***
; ***               Save-cat                            ***
; ***                                                   ***
; *********************************************************

: * Saves the cat to disk.
; * No args or results.

: * Calls Track00,Dictsize,Save-block,Step-out.
; * Called by: STORE,RESTORE,BSTORE,XFORMAT and DELETE.

            CALL Track00
            CALL Dictsize
            CALL Save-block             ; Save first copy on track 0.
            CALL Step-out
            CALL Dictsize
            JP	Save-block              ; Save second copy on track 1.






; *********************************************************
; ***                                                   ***
; ***               Load-cat                            ***
; ***                                                   ***
; *********************************************************

; * Loads the cat from disk after switching on the drive.

; * No args or results.

; * Calls: On,TrackOO,Dictsize, Load-block, Step-out,Print,
           Print-error and Error-?
; * Called by: STQRE/RESTORE,BSTORE,LD,BLD,CAT and DELETE.

              CALL On
              CALL Track00
              CALL Dictsize           ; Load first copy.
              CALL Load-block
              PUSH AF
              CALL Step-out           ; Move to track 1.
              POP AF
              OR A
              RET Z                   ; Return if no error.
              CALL Print-error
              LD A,$18                ; "Second cat read".
              CALL Print
              CALL Dictsize
              CALL Load-block         ; Load second copy.
              JP Error-?



; *********************************************************
; ***                                                   ***
; ***               Print-error                         ***
; ***                                                   ***
; *********************************************************

; * If A holds an error number, it prints the error.
; * Otherwise, it has no effect.

; * On entry A = error number.

                OR A                ; Test for A = 0
                RET Z               ; Return if so.
                LD B,A              ; Error number in B.
                LD A,$0D            ; Print carriage return.
                RST 8               ; Wrch.

                LD A,B
                AND $70             ; Test for ACIA error.
                JR Z,Erknown        ; Skip if it isn't.
                LD B,$12
                LD C,A
                AND $40
                JR NZ,Erknown       ; Parity error.
                DEC B
                LD C,A
                AND $20
                JR NZ,Erknown       ; Overrun error.
                                    ; (Should be impossible).
                DEC B               ; Else, Framing error.
Erknown         LD A,B
                ADD 5               ; Errors start a message 5.
                CALL Print
                LD A,$0D            ; Print carriage return.
                RST 8               ; Wrch.
                RET




; *********************************************************
; ***                                                   ***
; ***                 XFORMAT                           ***
; ***                                                   ***
; *********************************************************

; * Forth word to write a blank catalogue to disk.
; * This specifies the block length and number of tracks.

; * Top of the parameter stack is the number of track.
; * Next on the parameter stack is the number of bytes
; * per track.

                CALL On
                RST 24              ; Pop DE from parameter stack.
                LD A,E              ; Only least significant byte.
                RST 24              ; Pop DE from parameter stack.

                LD HL,(RAMTOP)
                INC HL              ; First byte of cat.
                LD (HL),A           ; Number of tracks.
                INC HL
                LD (HL),E           ; Number of bytes per track.
                INC HL
                LD (HL),D
                INC HL

                LD (HL),0           ; Rest of the cat is filled
                LD D,H              ; with zeros.
                LD E,L
                INC DE
                LD BC, (Cat-size)
                DEC BC
                DEC BC
                DEC BC
                DEC BC
                LDIR

                CALL Save-cat
                CALL Off
                JP (IY)             ; Forth return.



; *********************************************************
; ***                                                   ***
; ***                 Save-file                         ***
; ***                                                   ***
; *********************************************************

; * Saves an area of memory to disk.
; * Doesn't read or alter the disk catalogue.

; * On entry:   HL = starting address of the memory to be saved.
; *             DE = the number of bytes to be saved.
; *              B = the file number of the file to be created.

: * Calls: Block-length,Step-out and Save-block.
; * Called by: STORE, BSTORE and RESTORE.


                LD C,2              ; Offset from (RAMTOP)+l of byte
                                    ; below start of track file table.
Sfnextb         PUSH HL             ; Save start address,
                PUSH DE             ; length.

Sfnextt         LD HL,(RAMTOP)
                INC HL              ; Load number of tracks.
                LD A, (HL)

                CP C                ; If the previous tack number
                LD A,$A             ; was equal to the limit, raise
                CALL Z,Error-?      ; a "Disk is full" error.

                PUSH BC             ; Move head to next track.
                CALL Step-out
                POP BC              ; Only BC was corrupted
                                    ; in Step-out.

                INC C               ; Pointer to table slot for
                                    ; this track.

                LD D,0              ; Calculate address in the cat
                LD E,C              ; of this tracks file entry.
                ADD HL,DE
                LD A,(HL)           ; The file for this track.
                OR A                ; Is it empty (=0) ?
                JR NZ,Sfnextt       ; If it isn't, try next track.

                LD (HL),B           ; If it is, claim it for this file.

                POP HL              ; HL := file length.
                POP DE              ; DE := start address.
                PUSH BC             ; Save BC.
                PUSH DE             ; Save start address.
                CALL block-length   ; DE := block length.

                OR A                ; CY := 0
                SBC HL,DE
                JR Z,Sfend          ; If file is exactly one block.
                JR C,Sfend          ; If file less than one block.

                 EX (SP),HL

                ï»¿; At this point, HL - start address,
                ; DE = block length, and remaining file length
                ; and track and file numbers saved.

                CALL Save-block     ; Leaves HL pointing to start
                                    ; of next block.

                POP DE              ; Save remaining file.
                POP BC
                JR Sfnextb

Sfend           ADD HL,DE           ; File was shorter than a block
                                    ; so make length positive again.
                EX DE,HL            ; DE := length still to be saved.
                POP HL              ; HL := start address.

                CALL Save-block
                POP BC
                RET




; *********************************************************
; ***                                                   ***
; ***               Load-file                           ***
; ***                                                   ***
; *********************************************************

; * Loads a file into an area of memory.
; * Doesn't read or alter the disk catalogue.
;
; * On entry: HL = the starting address of the memory.
; *           DE = the number of bytes to be loaded
; *            B = the file number of the file to be loaded.

; * Calls: Block-length, Step-out and load-block.
; * Called by: LD,RUN and BLD.

              LD C,3                ; Offset from (RAMTOP) of the byte
                                    ; below start of track file table.
Lfnextb       PUSH HL               ; Save start address,
              PUSH DE               ; length.

Lfnextt       PUSH BC               ; Move head to next track.
              CALL Step-out
              POP BC                ; Only BC was corrupted in Step-out

              INC C                 ; Pointer to slot for this track.

              LD HL,(RAMTOP)
              LD D,O                ; Calculate the address in the cat
              LD E,C                ; of this tracks file entry.
              ADD HL,DE
              LD A,(HL)             ; The file for this track.
              CP B                  ; Is it the file we want ?
              JR NZ,Lfnextt         ; Try next track it if isn't.

              POP HL                ; HL := file length.
              POP DE                ; DE := start address.
              PUSH BC               ; Save BC.
              PUSH DE               ; Save start address.
              CALL block-length     ; DE := block length.

              OR A                  ; CY := 0
              SBC HL,DE
              JR Z,Lfend            ; If file is exactly one block.
              JR C,Lfend            ; If file less than one block.

              EX (SP),HL

             ; At this point, HL = start address,
             ; DE = block length, and remaining file length and
             ; track and file numbers saved.

              CALL Load-block       ; Leaves HL pointing to start
                                    ; of the next block.
            CALL Error-?

            POP DB                  ; Load remaining file.
            POP BC
            JR Sfnextb

ï»¿Lfend      ADD HL,DE               ; File was shorter than a block
                                    ; so make it positive again.
            EX DE,HL                ; DE := length still to be loaded.
            POP HL                  ; HL := start address.

            CALL Load-block
            CALL Error-?

            POP BC
            RET


; *********************************************************
; ***                                                   ***
; ***               Find-word                           ***
; ***                                                   ***
; *********************************************************

; * Finds the address of a word in the cat if it exists and
; * the address of the first free point in the cat if it doesn't.

; * On exit:    if A=0, word not found, HL = end of used cat.
; *             else, word found, DE = address of first data field.
; *                               HL = address of length field,
                                   A = file number.
; * Calls: WORD.
; * Called by: Enter-word, Lookup-word and DELETE.

                LD C,0              ; File number in C.

                LD HL,(RAMTOP)      ; Find start of names in cat.
                INC HL
                LD E, (HL)
                LD D,0
                INC HL
                ADD HL,DE

                LD DE,Pad           ; Address of name to be used.

Fwloop          EX HL,DE            ; DE := pos in cat, HL = pos in PAD.
                INC C               ; Next file number.

                LD A, (DE)          ; Length field.
                OR A                ; Zero marks the end of cat.
                RET Z               ; So return if zero.

                PUSH DE
                PUSH HL

                LD B,A              ; Length field in B.
                INC B

FwComp          CP (HL)             ; Compare lengths first, then chars.
                JR NZ,Fwnotf        ; Break if any difference.
                INC HL              ; Next character in PAD.
                INC DE              ; Next character in cat.
                LD A,(DE)           ; Next character.
                DJNZ Fwcomp         ; End of string ?

                ; File name has been found

                POP HL	            ; Throw away pad pointer.
                POP HL              ; Recover address of length field.

                LD A,C              ; File number in A.
                RET

Fwnotf          LD A,B              ; Find start of next word.
                ADD 4
                LD L,A
                LD H,0
                ï»¿ADD HL,DE
                POP DE              ; Recover start of pad pointer.
                INC SP              ; Through away address of previous
                INC SP              ; length field.
                JR Fwloop           ; Try next word.





; *********************************************************
; ***                                                   ***
; ***               Enter-word                          ***
; ***                                                   ***
; *********************************************************

; * Enters a new file name into the catalogue.

; * On entry, the file name is in the input buffer.
; * HL = start address of file (0 for a DICT file).
; * DE = length of file in bytes.
; * On exit, DE and HL have their entry values and
: * B = the file number of the file.

* Calls: Find-word and Error-?
* Called by: STORE and BSTORE.

                PUSH DE             ; Save DE and HL.
                PUSH HL
                PUSH DE             ; Two copies of DE.

                CALL Find-word      ; Sets C to file number.
                OR A                ; Test for found.
                LD A,7              ; "File name already exist".
                CALL NZ,Error-?     ; Call if found.

                LD B,(HL)           ; HL was set to point at
                                    ; the name in the PAD by Find-word.
                LD A,B
                LD (DE),A           ; Store name length in cat.
                                    ; Find word made DE point to
                                    ; the first free point in the cat.

                PUSH HL
                PUSH DE

                LD HL, (RAMTOP)     ; Start of cat.
                OR A                ; Clear carry.
                SBC HL,DE           ; HL := -bytes used.

                LD DE, ($F8FE)      ; Cat size.
                ADD HL,DE           ; HL := bytes free.

                ADD 8               ; bytes needed +3.
                LD E,A
                LD D,0
                OR A                ; Clear carry.
                SBC HL,DE           ; Set carry if not enough room.

                POP DE
                POP HL
                LD A,9              ; "Cat full".

                CALL C,Error-?

Ewcopy          INC DE              ; Next position in the cat.
                INC HL              ; Next character in the name.
                LD A,(HL)           ; Copy character.
                LD (DE),A
                DJNZ Ewcopy         ; Repeat for length of name.

                EX HL,DE            ; HL := pointer to cat.
                POP DE              ; file length.

                INC HL              ; Store length.
                LD (HL),E
                INC HL
                LD (HL),D

                POP DE              ; File start address.
                INC HL
                LD (HL),E
                INC HL
                LD (HL),D

                INC HL              ; Zero marks end of names.
                LD (HL),0

                POP HL
                EX HL,DE            ; HL := start, DE := length.
                LD B,C              ; B := file number.
                RET


; *********************************************************
; ***                                                   ***
; ***               Lookup- word                        ***
; ***                                                   ***
; *********************************************************


; * Finds the file number, length and start address of the ;
; * file with name in the input buffer.

; * On exit: B = file number,
; *          DE = length,
; *          HL = start address (0 of DICT files).

; * Calls: Find-word and Error-?.
; * Called by: LD, BLD and RUN.

                CALL Find-word
                LD B,A              ; B := file number.
                OR A                ; Test for file not found.
                LD A,8              ; "File name not found".
                CALL Z,Error-?      ; Raise error if not found.

                EX HL,DE            ; HL points to first data field.

                LD E,(HL)           ; Load DE with length.
                INC HL
                LD D, (HL)
                INC HL

                LD A, (HL)          ; Load HL with start address.
                INC HL
                LD H, (HL)
                LD L,A

                RET




; *********************************************************
; ***                                                   ***
; ***               Hex-byte                            ***
; ***                                                   ***
; *********************************************************

; * Prints the contents of A as a two digit hexadecimal number.

; * On entry: A = number to be printed.
; * Only register A is used.

; * Calls: Hex-char.
; * Called by: CAT.

                PUSH AF
                RR A                ; Get most significant nibble.
                RR A
                RR A
                RR A
                CALL Hex-char       ; Print it.
                POP AF              ; Implicit CALL of Hex-char
                                    ; and then return.




; *********************************************************
; ***                                                   ***
; ***               Hex-char                            ***
; ***                                                   ***
; *********************************************************


; * Prints the least significant nibble of the byte in A
; * as a single hexadecimal digit.

; * On entry: A = nibble to be printed.
; * Only register A is used.

; * Calls: None.
; * Called by: Hex-digit and CAT.

                AND $F              ; Mask off high nibble.
                CP $A               ; if >= 10,
                JR C,Numdigt        ; add "A"-"9"+1.
                ADD 7
Numdigt         ADD $30             ; ASCII for "0".
                RST 8               ; Wrch.
                RET




; *********************************************************
; ***                                                   ***
; ***                     CAT                           ***
; ***                                                   ***
; *********************************************************

; * Gives a list of all the file names on the disk together
; * with their lengths and starting addresses.
; * (Dictionary files have "DICT" in place of the starting
; * address).
; * It then gives the number of bytes free for use on the disk
; * and a map of track usage.
; * It is a FORTH word.

; * Calls: Load-cat, Off, Hex-byte, Hex-char and Block-length.

                CALL Load-cat       ; Turns the motor on first.
                CALL Off

                LD A,$D             ; CR.
                RST 8               ; Wrch.

                LD HL, (RAMTOP)     ; Point to start of names.
                INC HL
                LD E,(HL)           ; number of tracks.
                LD D,0
                INC HL
                ADD HL,DE

Namelp          LD C,$15            ; Number of character for name.
                LD A, (HL)
                OR A                ; Zero marks end of names.
                JR Z,findfre

                LD B,A              ; Name length in B.
Namepr          INC HL
                DEC C
                LD A, (HL)
                RST 8               ; Wrch.
                DJNZ Namepr         ; Print rest of name.

                ; Pad with spaces to get to the same column.

                LD A,C
                AND $1F             ; File name might have been
                                    ; longer than a line.
                                    ; (Assumes 32 chars per line).
                LD B,A
Catpdlp         LD A,$20            ; ASCII for space.
                RST 8               ; Wrch.
                DJNZ Catpdlp

                ; Print length in hex.

                INC HL
                LD C, (HL)
                INC HL
                LD A, (HL)
                CALL Hex-byte

                LD A,C
                CALL Hex-byte

                ; Print space.

                LD A,$20            ; ASCII for space.
                RST 8               ; Wrch.

                ; Print start address in hex of "DICT".

                INC HL
                LD C, (HL)
                INC HL
                LD A, (HL)
                INC HL              ; Point to start of next word.

                LD B,A              ; Test start = 0.
                OR C
                JR NZ,Catbyts
                LD A,$19            ; "DICT".
                CALL Print
                JR Catjn

Catbyts         LD A,B              ; Print as hex
                CALL Hex-byte
                LD A,C
                CALL Hex-byte

                LD A,$D             ; CR.
                RST 8               ; Wrch.
                JR Namelp           ; Next file name.

Findfre         LD A,$D             ; CR.
                RST 8               ; Wrch.

                LD HL, (RAMTOP)     ; Find start of file for track
                INC HL              ; vector.
                LD B,(HL)           ; Number of tracks.
                DEC B               ; Two tracks used for cats.
                DEC B
                CALL Block-length   ; DE := number of bytes on a track.
                INC HL
                INC HL              ; First element of vector.

                PUSH HL             ; Temp start of vector.
                LD HL,0             ; Bytes free accumulated in HL
                LD C,0              ; and C.

Freelp          EX HL, (SP)         ; HL = vector pointer.
                INC HL              ; Next track.
                LD A, (HL)          ; File number of track.
                EX HL, (SP)         ; HL = Count.

                OR A                ; Test A=0 ie free.
                JR NZ,Notfree       ; If its used, don't count it.
                ADD HL,DE           ; If it is free, add a track full
                                    ; of bytes.
                JR NC,Notfree       ; Use C for high byte if 16 bits
                INC C               ; overflow.
Notfree         DJNZ Freelp         ; Try all tracks.

                ï»¿POP DE             ; discard vector pointer.

                LD A,C              ; Most significant byte of count.
                CALL Hex-byte
                LD A,H              ; Middle byte of count.
                CALL Hex-byte
                LD A,L              ; Least significant byte of count.
                CALL Hex-byte

                LD A,$1A            ; " bytes free".
                CALL Print

                JP (IY)             ; Forth return.


; *********************************************************
; ***                                                   ***
; ***               Dict-size                           ***
; ***                                                   ***
; *********************************************************

; * Sets DE and HL to the start and length of the catalogue.

; * DE = length of cat on exit.
; * HL = starting address of cat on exit.

; Calls: none.
; Called by: Load-cat and Save-cat.

                LD HL,(RAMTOP)
                INC HL              ; Miss the byte used for DRIVE.
                LD DE,($F8FE)       ; Cat size.
                DEC DE              ; Don't save drive number.
                RET


; *********************************************************
; ***                                                   ***
; ***               Block-length                        ***
; ***                                                   ***
; *********************************************************

; * Loads the block length of the current disk into DE without
; * changing any other registers.

; * DE = Block length on exit.

; * Calls: None.
; * Called by: Load-file, Save-file.

                PUSH HL             ; HL must be the same at the end.
                LD HL, (RAMTOP)
                INC HL
                INC HL              ; The block length is in the second
                LD E,(HL)           ; and third bytes of the cat.
                INC HL
                LD D,(HL)
                POP HL              ; Recover HL.
                RET



; *********************************************************
; ***                                                   ***
; ***               Delete-file                         ***
; ***                                                   ***
; *********************************************************

; * Deletes the file with its name in the pad from the
; * catalogue copy in RAM.

; * No args or results.

; * Calls: Find-word.
; * Called by: DELETE and RESTORE.

                CALL Find-word          ; DE := data field of word,
                                        ; HL := name length field,
                                        ; C := file number.

                LD C,A

                OR A                    ; Test for word not found.
                LD A,8                  ; "File name does not exit".
                CALL Z,Error-?

                PUSH DE
                PUSH HL

                ; First free all blocks used by this file and
                ; decrease the file number entries if they are
                ; greater than this file.

                LD HL,(RAMTOP)          ; Find the start of the
                INC HL                  ; block usage vector.
                LD B,(HL)               ; number of tracks.
                DEC B                   ; Two tracks used for cats.
                DEC B
                INC HL
                INC HL                  ; Skip block size bytes.

Delfl           INC HL                  ; Next track.
                LD A,(HL)               ; File for this track.
                CP C                    ; Is if used for this file.
                JR Z, Delfd             ; If it is, set it free.
                JR C, Delfst            ; If its < this file, leave it.
                DEC A                   ; If its greater than this file,
                                        ; the file number will have decreased
                                        ; by one.
                JR Delfst
Delfd           XOR A                   ; A := 0 for empty track.
Delfst          LD (HL),A               ; Store back to cat.
                DJNZ Delfl              ; Repeat for all tracks.

                ; Second, remove the name it self by shuffling
                ; all the names above it down.

                POP DE                  ; name length field.
                POP HL                  ; data field.
                INC HL                  ; HL := name length field
                INC HL                  ;       of next word.
                INC HL
                ï»¿INC HL

Delfl2          LD A, (HL)              ; Name length.
                LD (DE),A               ; Store it even it its zero.
                OR A                    ; If it is zero, stop.
                RET Z
                ADD 4                   ; Four data bytes to copy as well.
                LD B,A

Delfl3          INC HL                  ; Copy one name and data.
                INC DE
                LD A, (HL)
                LD (DE),A
                DJNZ Delfl3
                INC HL                  ; Then repeat for the rest
                INC DE                  ; of the names.
                JR Delfl2


; *********************************************************
; ***                                                   ***
; ***               DELETE                              ***
; ***                                                   ***
; *********************************************************

; * Forth word to delete a file.

; * No arguments or results.

; * Calls; Load-cat,Delete-file, Save-cat and Off.

                CALL Word
                CALL Load-cat
                CALL Delete-file
                CALL Save-cat
                CALL Off
                JP (IY)             ; Forth return.


; *********************************************************
; ***                                                   ***
; ***               Error-?                             ***
; ***                                                   ***
; *********************************************************

; * Prints and error message and then ABORTS (if A>0).

; * A = error number on entry.
; * If A = 0, the routine returns immediately.

; * Calls: Print-error and Off.
; * Called by: Almost everything.

                OR A                ; Test A=0
                RET Z               ; Return if it is.

                CALL Print-error
                CALL Off            ; Switch drive off.
                                    ; (Doesn't matter if it wasn't on).

                EI                  ; Could have been called with
                                    ; interrupts off.
                RST 32              ; ABORT.
                $FF                 ; No error number.



; *********************************************************
; ***                                                   ***
; ***               Store                               ***
; ***                                                   ***
; *********************************************************

; * Store a dictionary file.

; * No arguments or results.

; * Calls; Save-file,Save-cat and Off.

                CALL Fill-length
                LD HL,(STKBOT)          ; End of dict.
                LD DE, (Forth)          ; Newest word in FORTH.
                LD (HL),E               ; Save it at the end of the dict.
                INC HL
                LD (HL),D
                INC HL

                LD DE,DCTRAM            ; Start of dict (after FORTH).
                OR A                    ; Clear carry.
                SBC HL,DE
                EX HL,DE                ; HL = start, DE = length.
                LD A,E                  ; Is there anything to save.
                SUB 2                   ; Two bytes added even if nothing.
                OR D
                LD A,$D                 ; "Why save 0 bytes".
                CALL Z,Error-?

                PUSH HL
                LD HL,0                 ; 0 represents a DICT file.
                CALL Enter-word
                POP HL
                CALL Save-file
                CALL Save-cat
                CALL Off
                RET



; *********************************************************
; ***                                                   ***
; ***               DSAVE                               ***
; ***                                                   ***
; *********************************************************

; * Forth word to store a dictionary file.
; * Disk equivalent of SAVE.

; * No arguments or results.

; * Calls; Load-cat and Store.

                CALL Word
                CALL Load-cat
                CALL Save
                JP (IY)



; *********************************************************
; ***                                                   ***
; ***               DBSAVE                              ***
; ***                                                   ***
; *********************************************************

; * Forth word to store a bytes file.
; * Disk equivalent of BSAVE.

; * No arguments or results.

; * Calls; Load-cat,Save-file,Save-cat and Off.

                CALL Word
                CALL Load-cat

                RST 24                  ; Pop DE from parameter stack.
                PUSH DE
                RST 24                  ; Pop DE from parameter stack.
                POP HL
                EX DE,HL                ; DE = length, HL = start.

                LD A,D                  ; Test len = 0.
                OR E
                LD A,$D                 ; "Why BSTORE 0 bytes".
                CALL Z,Error-?

                CALL Enter-word
                CALL Save-file
                CALL Save-cat
                CALL Off
                JP (IY)                 ; Forth return.



; *********************************************************
; ***                                                   ***
; ***               DLOAD                               ***
; ***                                                   ***
; *********************************************************

; * Forth word to load a bytes file.
; * Disk equivalent of BLOAD.

; * No arguments or results.

; * Calls; Load-cat,Load-file, Error-? and Off.

                CALL Word
                CALL Load-cat
                RST 24                  ; Pop DE from parameter stack.
                                        ; First argument ignored.
                RST 24                  ; Pop DE from parameter stack.
                PUSH DE                 ; Save start address.
                CALL Lookup-word

                LD A,H                  ; Check not DICT.
                OR L
                LD A,$B                 ; "BLD a DICT".
                CALL Z,Error-?

                EX HL, (SP)             ; HL = parameter start.
                LD A,H                  ; If zero ...
                OR L
                JR Z,Bldlb              ; Use start from cat.
                EX HL,(SP)
Bldlb           POP HL

                CALL Load-file
                CAll Off
                JP (IY)                 ; Forth return.





; *********************************************************
; ***                                                   ***
; ***               RESAVE                              ***
; ***                                                   ***
; *********************************************************

; * Forth word to save a dictionary file under a name which
; * already exists.

; * No arguments or results.

; * Calls; Load-cat,Delete,Save.

                CALL Word
                CALL Load-cat
                CALL Delete-file
                CALL Save-cat
                CALL Store
                JP (IY)



; *********************************************************
; ***                                                   ***
; ***               Word                                ***
; ***                                                   ***
; *********************************************************

; * Transfers a word from the input buffer to the pad.

; * No arguments or results.
; * All registers corrupted.

; * Calls: The forth word WORD.

                LD DE,$20               ; Put a space on top,
                RST 16                  ; of the parameter stack.
                                        ; WORD uses this character
                                        ; as its termination mark.
                CALL Enter-forth
                WORD
                End-forth

                RST 24                  ; Discard the top e.,f the stack
                                        ; which is the address of the pad.
                RET





; *********************************************************
; ***                                                   ***
; ***               MAP                                 ***
; ***                                                   ***
; *********************************************************

; * Prints the file number for each track as a  single
; * hexadecimal digit. "-" for not used.
; * It is a Forth word.

; * Calls: Hex-char, Load-cat and Off.

                CALL Load-cat
                CALL Off
                LD HL,(RAMTOP)
                INC HL
                LD B, (HL)                  ; Number of tracks,
                DEC B                       ; Two tracks uses cat.
                DEC B
                INC HL                      ; Skip block size.
                INC HL

Catmap          INC HL
                LD A, (HL)
                OR A                        ; Use space not 0 for an
                JR Z,Mapl                   ; empty track. (0 for 0 mod 16) .
                CALL Hex-char
                JR Map2
Mapl            LD A,$2D                    : "-".
                RST 8                       ; Wrch
Map2            DJNZ Catmap                 ; Print all tracks
                JP (IY)                     ; Forth return.



; *********************************************************
; ***                                                   ***
; ***               DLOAD                               ***
; ***                                                   ***
; *********************************************************

; * Loads a dict or bytes (At stored address) file from
; * disk to RAM.
; * It is a Forth word.

; * No args or results.

; * Calls: Word, Load-cat, Lookup-word, Load-file, Off
; *        and the Forth ROM for LOAD.

                CALL Fill-length
                CALL Word
                CALL Load-cat
                CALL Lookup-word            ; HL := start address,
                                            ; DE := length and
                                            ; B := file number.
                LD A,H                      ; Test start address = 0
                OR L                        ; Which indicates a dict.
                JR Z,Dloadl1                ; Jump if dict.

                CALL Load-file              ; Load bytes file at address
                CALL Off                    ; Read from cat.
                JP (IY)

Dloadl1         LD ($2325),SP               ; Transfer SP to HL.
                LD HL,($2325)

                PUSH DE                     ; Save length to be loaded.
                LD DE,$40                   ; There must be some room
                OR A                        ; after loading.
                SBC HL,DE
                POP DE

                PUSH HL                     ; Save top of usable RAM.
                LD HL,(STKBOT)              ; Start point of loading.
                ADD HL,DE                   ; End point of loading.
                EX HL,DE
                EX HL, (SP)

                ; At this point, HL = highest point which can be used,
                                 DE = highest point which will be used,
                ; and the length of load is on the stack.

                OR A                        ; Clear carry.
                SBC HL,DE                   ; Compare.

                POP DE
                LD A,$F                     ; "Not enough RAM".
                CALL C,Error-?              ; Error if DE was > HL.

                LD HL,(STKBOT)              ; First free byte after dict.
                PUSH DE
                PUSH HL
                CALL Load-file
                CALL Off

                ï»¿POP HL
                POP DE
                DEC DE                      ; Two bytes were added
                DEC DE                      ; for vocab pointer,
                LD ($2325),DE               ; Will be read as length
                                            ; added to diet.
                ADD HL,DE
                LD E, (HL)                  ; Read vocab pointer.
                INC HL
                LD D, (HL)
                LD ($2329),DE               ; Will be read as newest
                                            ; word.
                JP $19AA                    ; Enter LOAD adjust section.





; *********************************************************
; ***                                                   ***
; ***               SCRATCH                             ***
; ***                                                   ***
; *********************************************************

; * Forth word to remove all words from dictionary,
; * Similar to FORGET <Oldest word>.

; * Calls: None.

                LD HL,$3C4C                 ; Forth.
                LD ($3C31),HL               ; CURRENT.
                LD ($3C33),HL               ; CONTEXT.
                LD L,$4F
                LD ($3C35),HL               ; VOCLINK.
                LD L,$51
                LD ($3C37),HL               ; STKBOT.
                LD L,$45
                LD ($3C39),HL               ; DICT.
                LD L,$5D
                LD ($3C3B),HL               ; SPARE.
                LD L,$49
                LD ($3C49),HL               ; Forth link.
                JP (IY)


; *********************************************************
; ***                                                   ***
; ***               Fill-length                         ***
; ***                                                   ***
; *********************************************************

; * Fills in the length field of the newest word in the
; * dictionary if it is not already filled and
; * then sets DICT to 0 to show that it is now filled.

; * Calls: None.

; * Called by: Store and DLOAD.

                LD HL,(STKBOT)              ; HERE.
                LD DE,(DICT)                ; Address of length field or zero.
                LD A,D                      ; Test for zero for already
                OR E                        ; Filled.
                RET Z
                SBC HL,DE                   ; Carry cleared by previous
                OR E.
                EX HL,DE                    ; HL is the address of the length
                                            ; field, DE is the length.
                LD (HL),E                   ; Store length.
                INC HL
                LD (HL),D

                LD HL,0                     ; (DICT) := 0
                LD (DICT),HL
                RET
