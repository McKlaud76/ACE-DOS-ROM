; * Deep Thought ACEDOS.ROM code listing.
; * The DOS system was written by Jeff Shepherd.

; * Corrections to Jupiter ACE Archive listing by McKlaud/June 2020

; *********************************************************
; ***	                                                ***
; ***	            ACE DOS System Map                  ***
; ***	                                                ***
; *********************************************************

; * ACE ROM Address:

; * $F000	+-----------------------------------------+
; *		|					  |
; *		|	1st block of words definition 	  |
; *		|	mainly using ROM routines	  |
; *		|					  |
; * $F2B0	+-----------------------------------------+
; *		|					  |
; * 		|	Empty block (1360 bytes of $FF)   |
; *		|					  |
; * $F800	+-----------------------------------------+
; *		|					  |
; *		|	ACE DOS Jump Table		  |
; *		|					  |
; * $FF90	+-----------------------------------------+
; *		|					  |
; *		|	ACE DOS				  |
; *		|					  |
; * $FE07	+-----------------------------------------+
; *		|					  |
; *		|	Empty block (41 bytes of $FF)	  |
; *		|					  |
; * $FE30	+-----------------------------------------+
; *		|					  |
; *		|	Error Messages Space		  |
; *		|					  |
; * $FF2F	+-----------------------------------------+
; *		|					  |
; *		|	Empty block (26 bytes of $FF)	  |
; *		|					  |
; * $FF45	+-----------------------------------------+
; *		|					  |
; *		|	2nd block of words definition 	  |
; *		|	of ACE DOS			  |
; *		|					  |
; * $FFFF	+-----------------------------------------+


; *********************************************************
; ***	                                                ***
; ***	       ACE DOS FORTH Words Index                ***
; ***	                                                ***
; *********************************************************

; * 1st block of ACE DOS word definitions start at $F000
; * with D. word:

; * Address	Word
; * -------------------------------------------------------
; * $F000	D.
; * $F013	E.
; * $F034	RAMTOP
; * $F049	START-OF-NAMES
; * $F06C	FIND-FREE
; * $F0A8	CAT-SINGLE-FILE
; * $F105	TEST-PAGE
; * $F131	CAT-HEADER
; * $F1C5	CAT-FILES
; * $F1F9	CAT-END
; * $F220	PRINT-CAT
; * $F238	DRIVE-NUMBER
; * $F281	LOAD-CAT
; * $F298	DIR
; * $F2A8	CPY

; * 2nd block of ACE DOS word definitions start at $FF45 with FORMAT word:

; * Address	Word
; * -------------------------------------------------------
; * $FF45	FORMAT
; * $FF5E	RUN
; * $FF78	MAP
; * $FF82	SCRATCH
; * $FF90	RESAVE
; * $FF9D	DRIVE
; * $FFA9	XFORMAT
; * $FFB7	CAT
; * $FFC1	DELETE
; * $FFCE	DBSAVE
; * $FFDB	DSAVE
; * $FFE7	DBLOAD
; * $FFF4	DLOAD

; *********************************************************

; * ASM Listing starting point

		ORG $F000	   ; * added on 1/06/2020

; *********************************************************
; ***                                                   ***
; ***               Constants                           ***
; ***                                                   ***
; *********************************************************

Acia_status	EQU  $21        ; Acia status register.
Acia_in         EQU  $23        ; Acia data input register.
Acia_control    EQU  $01        ; Acia control register.
Acia_out        EQU  $03        ; Acia data output register.

Pia_a_i         EQU  $29        ; PIA port A data input.
Pia_a_o         EQU  $09        ; PIA port A data output.
Pia_b_o         EQU  $0D        ; PIA port B data output.
Pia_a_cr        EQU  $0B        ; PIA port A control register.
Pia_b_cr        EQU  $0F        ; PIA port B control register.

Find_index	EQU  $F800	; Address of the fist entry
				; to the Jump Table
Dos_words       EQU  $FFFD      ; Address of 'name length field'
                                ; of first DOS FORTH word DLOAD
Forth_link      EQU  $3C47      ; Address of the 'link field'
                                ; of the word FORTH.
Cat_size        EQU  $F8FE      ; Address of the first of two
                                ; bytes holding the length
                                ; of the cat in bytes + 1.
Message_space   EQU  $FE30      ; Address of the first string
                                ; used by Print.
Pad             EQU  $2701      ; Address of the FORTH PAD
RAMTOP          EQU  $3C18      ; Hold the address of the highest
                                ; byte used by the Jupiter Ace.
                                ; The drive number is stored at
                                ; (RAMTOP). The cat starts at
                                ; (RAMTOP) + 1.
                                ; where a word read by WORD
                                ; is placed.
Enter_forth     EQU  $04B9      ; Enter FORTH from machine code.
WORD            EQU  $05AB      ; Parameter field address of WORD.
End_forth       EQU  $1A0E      ; FORTH word to enter Z80 code.

STKBOT          EQU  $3C37	;
DICT            EQU  $3C39	;


; *	1st Block of Words Definition starts at $F000

; *********************************************************
; ***	                                                ***
; ***	       		D. word                         ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF000	  	DM "D"			; 'name field'
        	DB '.' + $80		; last charater inverted

LF002        	DW $0011            	; 'word lenght field'

LF004	  	DW $0000		; 'link field' - end of linked list

LF006		DB $02			; 'name length field'

LF007	  	DW $0EC3            	; 'code field' - docolon

LF009		DW $098D		; <#
		DW $09E1		; #S
		DW $099C		; #>
		DW $096E		; TYPE
		DW $04B6		; Exit

; *********************************************************
; ***	                                                ***
; ***	       		E. word                         ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF013	  	DM "E"			; 'name field'
        	DB '.' + $80		; last charater inverted

LF015        	DW $001F            	; 'word lenght field'

LF017	  	DW $F006		; 'link field' to 'name leght field' of
					; D. word

LF019		DB $02			; 'name length field'

LF01A	  	DW $0EC3            	; 'code field' - docolon

LF01C		DW $1011		; Stack next word
		DW $0000		; EXWRCH
		DW $098D		; <#
		DW $09E1		; #S
		DW $099C		; #>
		DW $1011		; Stack next word
		DW $0007		; ????
		DW $0912		; OVER
		DW $0DE1		; #S
		DW $0A83		; SPACES
		DW $096E		; TYPE
		DW $04B6		; Exit

; *********************************************************
; ***	                                                ***
; ***	       		RAMTOP word                     ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF034	  	DM "RAMTO"		; 'name field'
        	DB 'P' + $80		; last charater inverted

        	DW $000F		; 'word lenght field'

LF03C	  	DW $F019     		; 'link field' to 'name leght field' of
					; E. word

LF03E		DB $06			; 'name length field'

LF03F	  	DW $0EC3            	; 'code field' - docolon

LF041		DW $1011		; Stack next word
		DW $3C18		; RAMTOP
		DW $08B3		; @
		DW $04B6		; Exit

; *********************************************************
; ***	                                                ***
; ***	       	START-OF-NAMES word                     ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF049	  	DM "START-OF-NAME"	; 'name field'
        	DB 'S' + $80		; last charater inverted

        	DW $0015		; 'word lenght field'

LF059	  	DW $F03E     		; 'link field' to 'name leght field' of
					; RAMTOP word

LF05B		DB $0E			; 'name length field'

LF05C	  	DW $0EC3            	; 'code field' - docolon

LF05E		DW $F03F		; ???
		DW $0E09		; 1+
		DW $086B		; DUP
		DW $0896		; C@            - fetch content byte
		DW $0DD2		; +
		DW $0E09		; 1+
		DW $04B6		; Exit

; *********************************************************
; ***	                                                ***
; ***	       	FIND-FREE word 	                        ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF06C	  	DM "FIND-FRE"		; 'name field'
        	DB 'E' + $80		; last charater inverted

        	DW $0033		; 'word lenght field'

LF077	  	DW $F05B     		; 'link field' to 'name leght field' of
					; START-OF-NAMES word

LF079		DB $09			; 'name length field'

LF07A	  	DW $0EC3            	; 'code field' - docolon

LF07C		DW $1011		; Stack next word
		DW $0000		; Zero - end marker
		DW $F05C		; START-OF-NAMES
		DW $F03F		; RAMTOP
		DW $1011		; Stack next word
		DW $0004		; ???
		DW $0DD2		; +
		DW $1323		; shuffle (ROM routine)
		DW $12E9		; I
		DW $0896		; C@            - fetch content byte
		DW $0C1A		; 0=
		DW $1283		; ?branch (ROM routine)
		DW $0005		; ???
		DW $0E09		; 1+
		DW $12A4		; End?
		DW $1332		; shuffle (ROM routine)
		DW $FFEF		; DBLOAD
		DW $0F3F		; ???
		DW $0E13		; 2+
		DW $08B3		; @
		DW $0CA8		; U*
		DW $04B6		; Exit

; *********************************************************
; ***	                                                ***
; ***	       	CAT-SINGLE-FILE word 	                ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF0A8	  	DM "CAT-SINGLE-FIL"	; 'name field'
        	DB 'E' + $80		; last charater inverted

        	DW $004E		; 'word lenght field'

LF0B9	  	DW $F079     		; 'link field' to 'name leght field' of
					; FIND-FREE word

LF0BB		DB $0F			; 'name length field'

LF0BC	  	DW $0EC3            	; 'code field' - docolon

LF0BE		DW $0912		; OVER
		DW $0E09		; 1+
		DW $0912		; OVER
		DW $096E		; TYPE
		DW $1011		; Stack next word
		DW $0010		; Push word DE if non-zero
		DW $0912		; OVER
		DW $0DE1		; -
		DW $1011		; Stack next word
		DW $001F
LF0D2		DW $0E4B		; AND
		DW $0A83		; SPACES
; ????
		DW $04B6		; Exit
; ????

; *********************************************************
; ***	                                                ***
; ***	       	TEST-PAGE word                          ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF105	  	DM "TEST-PAG"		; 'name field'
        	DB 'E' + $80		; last charater inverted

        	DW $0023		; 'word lenght field'

LF110	  	DW $F0BB     		; 'link field' to 'name leght field' of
					; CAT-SINGLE-FILE word

LF112		DB $09			; 'name length field'

LF113	  	DW $0EC3            	; 'code field' - docolon

LF115		DW $0E1F		; 1-
		DW $086B		; DUP
		DW $0C1A		; 0=
		DW $1283		; ?branch       - forward if not a
 		DW $1112		; DUP
LF11E ???	DW $9F00
;???

; to be completed

LF12F		DW $04B6		; Exit

; to be completed

; *********************************************************
; ***	                                                ***
; ***	       		CAT-HEADER word                 ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF131	  	DM "CAT-HEADE"		; 'name field'
        	DB 'R' + $80		; last charater inverted

        	DW $007A		; 'word lenght field'

LF13D	  	DW $F112     		; 'link field' to 'name leght field' of
					; TEST-PAGE word

LF13F		DB $0A			; 'name length field'

LF140	  	DW $0EC3            	; 'code field' - docolon

		DW $0A1D		; CLS
		DW $1396		; pr_embedded string

; to be completed

LF1B3		DW $04B6		; Exit

; $F1B5 - $F1C4 - 26 bytes of $FF

; *********************************************************
; ***	                                                ***
; ***	       		CAT-FILE word                   ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; to be completed

LF1C5

; to be completed

LF1F7		DW $04B6		; Exit

; *********************************************************
; ***	                                                ***
; ***	       		CAT-END word                    ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; to be completed

LF1F9

; to be completed

; *********************************************************
; ***	                                                ***
; ***	       		PRINT-CAT word                  ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF220	  	DM "PRINT-CA"		; 'name field'
        	DB 'T' + $80		; last charater inverted

LF229        	DW $000F            	; ????

LF22B	  	DW $F204		; 'link field' to 'name leght field' of
					; CAT-END word

LF22D		DB $09			; 'name length field'

LF22E		DW $0EC3           	; 'code field' - docolon
		DW $F140		; CAT-HEADER
		DW $F1D3		;
		DW $F205		;
		DW $04B6		; Exit


; to be completed DRIVE-NUMBER

LF238

; *********************************************************
; ***	                                                ***
; ***	       		LOAD-CAT word                   ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF281	  	DM "LOAD-CA"		; 'name field'
        	DB 'T' + $80		; last charater inverted

LF289        	DW $0000            	; ????

LF28B	  	DW $F248		; 'link field' to 'name leght field' of
					; DRIVE word

LF28D		DB $08			; 'name length field'

LF28E		DW $F290            	; 'code field'

LF290		CALL Load_cat		; Load_cat
		CALL Off		; Off
		JP (IY)			; FORTH return.

; *********************************************************
; ***	                                                ***
; ***	       		DIR word                        ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF298	  	DM "DI"			; 'name field'
        	DB 'R' + $80		; last charater inverted

LF29B        	DW $0000            	; ????

LF29D	  	DW $F28D		; 'link field' to 'name leght field' of
					; LOAD-CAT word

LF29F		DB $03			; 'name length field'

LF2A0		DW $0EC3            	; 'code field' - docolon

LF2A2		DW $F28E		;
		DW $F22E		; LOAD-CAT
		DW $04B6		; Exit

; *********************************************************
; ***	                                                ***
; ***	       		CPY word                        ***
; ***	                                                ***
; *********************************************************
; * It is a FORTH word.

; Word to ...

LF2A8	  	DM "CP"			; 'name field'
        	DB 'Y' + $80		; last charater inverted

LF2AB        	DW $0000            	; ????

LF2AD	  	DW $F29F		; 'link field' to 'name leght field' of
					; DIR word

LF2AF		DB $03			; 'name length field'

; $72B0 - $F7FF: 1360 bytes of $FF

; *********************************************************
; ***	                                                ***
; ***	            Jump Table                          ***
; ***	                                                ***
; *********************************************************

; * Jump table starts at $F800 with Find_index

Find_index      JP $F900          ; $F800 - location in the ROM
Find_header     JP $F923          ; $F803
Save_block      JP $F953          ; $F806
Load_block      JP $F9D0          ; $F809
Wait            JP $FA03          ; $F80C
On              JP $FA0D          ; $F80F
Off             JP $FA24          ; $F812
Setup           JP $FA2B          ; $F815
Step            JP $FA64          ; $F818
Step_out        JP $FA62          ; $F81B
DRIVE           JP $FA74          ; $F81E
Print           JP $FA86          ; $F821
Track00         JP $FAA5          ; $F824
Save_cat        JP $FAB1          ; $F827
Load_cat        JP $FAC3          ; $F82A
Print_error     JP $FAE7          ; $F82D
XFORMAT         JP $FB0A          ; $F830
Save_file       JP $FB31          ; $F833
Load_file       JP $FB6D          ; $F836
Find_word       JP $FBA6          ; $F839
Enter_word      JP $FBD5          ; $F83C
Lookup_word     JP $FC18          ; $F83F
Hex_byte        JP $FC2C          ; $F842
Hex_char        JP $FC35          ; $F845
CAT             JP $FC41          ; $F848
Dict_size       JP $FCCA          ; $F84B
Block_length    JP $FCD4          ; $F84E
Delete_file     JP $FCDF          ; $F851
DELETE          JP $FD19          ; $F854
Error_Msg       JP $FD2A          ; $F857
Store           JP $FD35          ; $F85A
DSAVE           JP $FD65          ; $F85D
DBSAVE          JP $FD70          ; $F860
DBLOAD          JP $FD90          ; $F863
RESAVE          JP $FDB2          ; $F866
Word            JP $FDC3          ; $F869
MAP             JP $FDD0          ; $F86C
			          ; $F86F to $F871 filled with $FF
DLOAD           JP $F880          ; $F872
SCRATCH         JP $F8D5          ; $F875

Fill_length     JP $FDF0          ; $F878


; *********************************************************
; ***                                                   ***
; ***               DLOAD                               ***
; ***                                                   ***
; *********************************************************
; * It is a Forth word.

; * Loads a dict or bytes (At stored address) file from
; * disk to RAM.

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
                EX DE,HL		    ; in JAA listing was EX HL,DE - 01/06/2020
                EX (SP),HL		    ; in JAA listing was EX HL,(SP) - 01/06/2020

                ; At this point, HL = highest point which can be used,
                ;                DE = highest point which will be used,
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

                POP HL
                POP DE
                DEC DE                      ; Two bytes were added
                DEC DE                      ; for vocab pointer,
                LD ($2325),DE               ; Will be read as length
                                            ; added to diet.
                ADD HL,DE
                LD E,(HL)                  ; Read vocab pointer.
                INC HL
                LD D,(HL)
                LD ($2329),DE               ; Will be read as newest
                                            ; word.
                JP $19AA                    ; Enter LOAD adjust section.

; *********************************************************
; ***                                                   ***
; ***               SCRATCH                             ***
; ***                                                   ***
; *********************************************************
; * It is a Forth word.

; * Word to remove all words from dictionary,
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
                LD ($3C4C),HL               ; Forth link - corrected from $3C49 - 01/06/2020
                JP (IY)

; *********************************************************
; ***                                                   ***
; ***               Find_index                          ***
; ***                                                   ***
; *********************************************************

; * Waits for the index hole to pass.

; * No arguments or results
; * DE and HL preserved.
; * Can give "Wot no disk" or "Wot no index hole".

; * Calls: None
; * Called by: Find_header and Save_block.


            	; First, loop till high, then loop till low.

            	LD BC,$7D00             ; Time out count.

Whilelo     	DEC BC                  ; Decrease count
            	LD A,B                  ; and test
            	OR C                    ; for zero.
            	LD A,$03                ; If zero then call Error_Msg
            	CALL Z,Error_Msg        ; Message "Wot no disk".
            	IN A,(Pia_a_i)
            	AND 1                   ; Test bit 0.
            	JR Z,Whilelo            ; Loop till high.
            	LD BC,$7D00             ; Time out count.

Whilehi   	DEC BC                  ; Decrease count
            	LD A,B                  ; and test
            	OR C                    ; for zero.
            	LD A,$04                ; If zero then call Error_Msg
            	CALL Z,Error_Msg        ; Message "Wot no index hole"
            	IN A,(Pia_a_i)
            	AND 1                   ; Test bit 0.
            	JR NZ,Whilehi           ; Loop till low.
            	RET

; *********************************************************
; ***                                                   ***
; ***               Find_header                         ***
; ***                                                   ***
; *********************************************************

; * Waits for the index hole to pass and then reads
; * the header which is a sequence of $FF's followed by
; * a 42.

; * A=0 if header found in time.
; * A=5 if header not found.
; * DE and HL preserved.
; * Can give "Wot no disk" or "Wot no index hole".

; * Calls: Find_index.
; * Called by: Load_block and Save_block.

            	CALL Find_index
            	LD BC,$7D00             ; Time out count.

 NotFF    	DEC BC                  ; Decrease count
            	LD A,B                  ; and test for zero.
            	OR C
            	LD A,5                  ; Return with 5 if zero.
            	RET Z
            	IN A,(Acia_status)
            	AND 1                   ; Test Read-full bit.
            	JR Z,NotFF              ; Loop if not ready.
            	IN A,(Acia_in)          ; Read byte from disk.
            	CP $FF
            	JR NZ,NotFF             ; Loop while byte is FF.

IsFF     	DEC BC                  ; Check for timeout
            	LD A,B                  ; again as before.
            	OR C                    ; Test for zero.
            	LD A,5                  ; Return 5 for "Wot no header"
            	RET Z                   ; if zero.
            	IN A,(Acia_status)
            	AND 1                   ; Test Read-full bit.
            	JR Z,IsFF               ; Loop if not ready.
            	IN A,(Acia_in)          ; Read byte from disk.
            	CP $FF
            	JR Z,IsFF               ; Loop while byte is FF.
            	CP 42                   ; If its not FF and
            	JR NZ,NotFF             ; its not 42, look for a FF again.
            	XOR A                   ; Clear A (No error).
            	RET

; *********************************************************
; ***                                                   ***
; ***               Save_block                          ***
; ***                                                   ***
; *********************************************************

; * Saves a single block of data and then verifies it against
; * memory. Assumes that the head is at the correct track.

; * On entry, HL = start address of data to be saved,
;             DE = number of bytes to be saved.

; * Calls: Wait, Find_header and Find_index.
; * Called by: Save_cat and Save_file.

            	IN A,(Pia_a_i)          ; Test write-protect
            	AND 4                   ; line.
            	LD A,$06                ; Message  "Disk is write protected"
            	CALL Z,Error_Msg        ; if line low.
                                    	; (Assumes disk enabled).
            	LD C,20                 ; Wait for 20ms,
            	CALL Wait               ; for head to settle.
            	DI                      ; Disable interrupts,
                                    	; Writing is time dependant.
            	CALL Find_index
            	PUSH DE                 ; Save start and length
            	PUSH HL                 ; for verify.
            	LD A,%10110111          ; Motor on, write enable.
            	OUT (Pia_a_o),A
            	LD C,2                  ; Wait for 2ms to set
            	CALL Wait               ; blank at start of track.
            	LD B,8                  ; Write 8 $FFs onto disk.

NextFF      	IN A,(Acia_status)      ; Test ready for data.
            	AND 2
            	JR Z,NextFF             ; Loop till ACIA ready.
            	LD A,$FF
            	OUT (Acia_out),A
            	DJNZ NextFF             ; Repeat for other bytes.

            	; B has been set to zero a this point as it
            	; is used as checksum.
Wait42      	IN A,(Acia_status)      ; Write a single 42,
            	AND 2                   ; first waiting
            	JR Z,Wait42             ; for ACIA ready.
            	LD A,42
            	OUT (Acia_out),A

            	; Now store the data.
Mainst      	IN A,(Acia_status)      ; Wait for
            	AND 2                   ; ACIA ready.
            	JR Z, Mainst
            	LD A, (HL)              ; Get byte form data.
            	OUT (Acia_out),A        ; Write it to disk.
            	ADD A,B                 ; Add it to check sun so far,
           	LD B,A
             	INC HL                  ; Point to next byte in data.
            	DEC DE                  ; One less byte to be written.
            	LD A,D                  ; Loop
            	OR E                    ; till
            	JR NZ,Mainst            ; count zero.

            	; Write check sun at end.
Checkw      	IN A,(Acia_status)      ; Wait for
            	AND 2                   ; ACIA ready.
            	JR Z,Checkw
            	LD A,B                  ; Write checksum.
            	OUT (Acia_out),A
            	LD C,1                  ; Wait 1ms for ACIA to
            	CALL Wait	        ; finish writing last bytes.
            	LD A,%10111111          ; Motor on, write disable.
            	OUT (Pia_a_o),A
            	POP HL                  ; Recover start and length.
            	POP DE
            	; Verify data.
            	CALL Find_header
            	CALL Error_Msg          ; Error if header not found.

Vloop      	IN A,(Acia_status)      ; Wait for data
            	LD C,A                  ; keeping status for possible
            	AND 1                   ; error message.
            	JR Z,Vloop
            	IN A,(Acia_in)          ; Get byte from disk.
            	CP (HL)                 ; Is it the same as memory ?
            	LD A,$02                ; Message "Verify error" if not.
            	CALL NZ,Error_Msg
            	LD A,C                  ; Test for error from ACIA.
            	AND %1110000            ; Framing, parity or overrun.
            	CALL NZ,Error_Msg
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
; ***               Load_block                          ***
; ***                                                   ***
; *********************************************************

; * Loads a single block of data.
; * Assumes that the head is at the correct track.

; * On entry, HL = start address to load data at.
; *           DE = number of bytes to be loaded.
; *                 (must be the same as when saved).
; * On exit, A = error number, 0 if no error.

; * Calls: Wait and Find_header.
; * Called by: Load_cat and Load_file.

            	LD C,20                 ; Wait for 20ms,
            	CALL Wait               ; for head to settle.
            	DI                      ; Disable interrupts,
                                    	; Loading is time dependant.
            	CALL Find_header
            	OR A                    ; Test for not error (=0).
            	RET NZ                  ; Return if error in finding header.
            	LD B,A                  ; B := 0, Zero checksum to start with.

Loadlop   	IN A,(Acia_status)      ; Wait for data
            	LD C,A                  ; keeping status for possible
            	AND 1                   ; error message.
            	JR Z,Loadlop

            	IN A,(Acia_in)          ; Get byte from disk.
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
LChLoop     	IN A, (c)     ; Wait for data
            	AND 1
            	jR Z,LChLoop
            	IN A,(Acia_in)          ; Checksum from disk.
            	CP B                    ; Compare with sum calculated.
            	LD A,1                  ; Error 1 if not the same.
            	JR NZ,IntoffR
            	XOR A                   ; A := 0 for no error.

IntoffR     	EI
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
; * Called by: Load_block, Save_block, On and Step.

            	PUSH BC                 ; Must be the same on exit.
Wait1      	LD B,$FA                ; Loop lasting 1ms.
Wait2       	DJNZ Wait2
            	DEC C                   ; Repeat the number in C times.
            	JR NZ,Wait1
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
; * Uses HL, A, C

; * Calls Wait.
; * Called by: Load_cat, XFORMAT.

            	LD A,%10111111          ; Motor on.
            	OUT (Pia_a_o),A

            	LD C,255                ; Wait about half a second.
            	CALL Wait
            	CALL Wait

            	LD HL,(RAMTOP)          ; Point to first byte
            	LD A, (HL)              ; reserved for disk.
                                    	; This is setup by DRIVE.
            	OUT (Pia_b_o),A         ; Select drive.
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
; * Called by: XFORMAT,Save_cat,Error_Msg,CAT,LD and BLD.

            	LD A,%11111111          ; All high (disabled).
            	OUT (Pia_b_o),A         ; Deselect drive.
            	OUT (Pia_a_o),A         ; Motor off.
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

            	LD BC,(Cat_size)        ; HL = top of detected RAM.
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
            	OUT (Pia_a_cr),A        ; Set both control registers
            	OUT (Pia_b_cr),A        ; to allow access to direction
                            		; control registers.
    		DEC A                   ; A := $FF
            	OUT (Pia_b_o),A         ; All lines output for port B.
            	LD A,%11111000          ; Three lines input for
            	OUT (Pia_a_o),A         ; port A.
            	LD A,4                  ; Set both control registers
            	OUT (Pia_a_cr),A        ; to allow access to
            	OUT (Pia_b_cr),A        ; data registers.
            	LD A,$FF                ; Set all outputs high (inactive).
            	OUT (Pia_a_o),A
            	OUT (Pia_b_o),A

            	; Initialise ACIA.
            	LD A,3                  ; Reset ACIA.
            	OUT (Acia_control),A
            	LD A,%11100             ; Set ACIA to 9-bit parity.
            	OUT (Acia_control),A

            	LD A,B
            	JP $32                  ; Resume startup.

; *********************************************************
; ***                                                   ***
; ***               Step_out                            ***
; ***                                                   ***
; *********************************************************

; * Steps head one track further out.

; * No arguments or results.
; * DE and HL preserved.

; * Calls: Wait.
; * Called by: Save_cat, Load_cat, Save_file and Load_file.
                LD A,%10101111	     ; Direction and motor on low ($AF).

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
; * A Called by: Step_out and Track00.

            	OUT (Pia_a_o),A      ; Specify direction.
            	LD C,5               ; Can do one step every 5ms.
            	CALL Wait
            	SUB %100000          ; Pull step low.
            	OUT (Pia_a_o),A
            	ADD %100000          ; And then high again.
            	OUT (Pia_a_o),A
            	RET

; *********************************************************
; ***                                                   ***
; ***               DRIVE                               ***
; ***                                                   ***
; *********************************************************
; * It is a FORTH word.

; * It selects drive and side.
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
                DJNZ drlab1
                DEC A

                ; Store it in the first byte above RAMTOP.
drlab2          LD HL,(RAMTOP)
                LD (HL),A
                JP (IY)             ; FORTH return.

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

; * On entry A = message number.

; * Calls: none.
; * Called by:

                LD DE,Message_space ; Start of message store.
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
                JR Z,Prwrch         ; If bit not set print as character.
                PUSH DE             ; Else call Print recursively.
                CALL Print
                POP DE
                JR Prloop

Prwrch          RST 8               ; Wrch.
                JR Prloop

; *********************************************************
; ***                                                   ***
; ***               Track00                             ***
; ***                                                   ***
; *********************************************************

; A Moves head to track zero.

; * No args or results.
; * DE and HL preserved.

; * Calls: Step.
; * A Called by: Save_cat and Load_cat.

Trk001p         IN A,(Pia_a_i)          ; Input lines
                AND 2                   ; track00 bit.
                RET Z                   ; Quit if already at track00.
                LD A,%10111111          ; For step in.
                CALL Step
                JR Trk001p              ; Test again.

; *********************************************************
; ***                                                   ***
; ***               Save_cat                            ***
; ***                                                   ***
; *********************************************************

; * Saves the cat to disk.
; * No args or results.

; * Calls Track00,Dictsize,Save_block,Step_out.
; * Called by: STORE,RESTORE,BSTORE,XFORMAT and DELETE.

            CALL Track00
            CALL Dictsize
            CALL Save_block             ; Save first copy on track 0.
            CALL Step_out
            CALL Dictsize
            JP	Save_block              ; Save second copy on track 1.

; *********************************************************
; ***                                                   ***
; ***               Load_cat                            ***
; ***                                                   ***
; *********************************************************

; * Loads the cat from disk after switching on the drive.

; * No args or results.

; * Calls: On,TrackOO,Dictsize, Load_block, Step_out,Print,
; *        Print_error and Error_Msg
; * Called by: STQRE/RESTORE,BSTORE,LD,BLD,CAT and DELETE.

              CALL On
              CALL Track00
              CALL Dictsize           ; Load first copy.
              CALL Load_block
              PUSH AF
              CALL Step_out           ; Move to track 1.
              POP AF
              OR A
              RET Z                   ; Return if no error.
              CALL Print_error
              LD A,$18                ; Message "Second cat read".
              CALL Print
              CALL Dictsize
              CALL Load_block         ; Load second copy.
              JP Error_Msg

; *********************************************************
; ***                                                   ***
; ***               Print_error                         ***
; ***                                                   ***
; *********************************************************

; * If A holds an error number, it prints the error.
; * Otherwise, it has no effect.

; * On entry A = error number.

                OR A                ; Test for A = 0
                RET Z               ; Return if so.
                LD B,A              ; Error number in B.
                LD A,$0D            ; Print carriage return (ASCII .CR).
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
                ADD 5               ; Errors start a message no 5.
                CALL Print
                LD A,$0D            ; Print carriage return (ASCII .CR).
                RST 8               ; Wrch.
                RET

; *********************************************************
; ***                                                   ***
; ***                 XFORMAT                           ***
; ***                                                   ***
; *********************************************************
; * It is a FORTH word.

; * Word to write a blank catalogue to disk.
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
                LD BC,(Cat_size)
                DEC BC
                DEC BC
                DEC BC
                DEC BC
                LDIR

                CALL Save_cat
                CALL Off
                JP (IY)             ; FORTH return.

; *********************************************************
; ***                                                   ***
; ***                 Save_file                         ***
; ***                                                   ***
; *********************************************************

; * Saves an area of memory to disk.
; * Doesn't read or alter the disk catalogue.

; * On entry:   HL = starting address of the memory to be saved.
; *             DE = the number of bytes to be saved.
; *              B = the file number of the file to be created.

; * Calls: Block_length,Step_out and Save_block.
; * Called by: STORE, BSTORE and RESTORE.


                LD C,2              ; Offset from (RAMTOP)+l of byte
                                    ; below start of track file table.
Sfnextb         PUSH HL             ; Save start address,
                PUSH DE             ; length.

Sfnextt         LD HL,(RAMTOP)
                INC HL              ; Load number of tracks.
                LD A,(HL)

                CP C                ; If the previous tack number
                LD A,$0A            ; was equal to the limit, raise
                CALL Z,Error_Msg    ; Message "Disk is full".

                PUSH BC             ; Move head to next track.
                CALL Step_out
                POP BC              ; Only BC was corrupted
                                    ; in Step_out.

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
                CALL Block_length   ; DE := block length.

                OR A                ; CY := 0
                SBC HL,DE
                JR Z,Sfend          ; If file is exactly one block.
                JR C,Sfend          ; If file less than one block.

                EX (SP),HL

                ; At this point, HL - start address,
                ; DE = block length, and remaining file length
                ; and track and file numbers saved.

                CALL Save_block     ; Leaves HL pointing to start
                                    ; of next block.
                POP DE              ; Save remaining file.
                POP BC
                JR Sfnextb

Sfend           ADD HL,DE           ; File was shorter than a block
                                    ; so make length positive again.
                EX DE,HL            ; DE := length still to be saved.
                POP HL              ; HL := start address.

                CALL Save_block
                POP BC
                RET

; *********************************************************
; ***                                                   ***
; ***               Load_file                           ***
; ***                                                   ***
; *********************************************************

; * Loads a file into an area of memory.
; * Doesn't read or alter the disk catalogue.
;
; * On entry: HL = the starting address of the memory.
; *           DE = the number of bytes to be loaded
; *            B = the file number of the file to be loaded.

; * Calls: Block_length, Step_out and Load_block.
; * Called by: LD,RUN and BLD.

        	LD C,3              ; Offset from (RAMTOP) of the byte
                                    ; below start of track file table.
Lfnextb       	PUSH HL             ; Save start address,
              	PUSH DE             ; length.

Lfnextt      	PUSH BC             ; Move head to next track.
              	CALL Step_out
              	POP BC              ; Only BC was corrupted in Step_out

              	INC C               ; Pointer to slot for this track.

              	LD HL,(RAMTOP)
              	LD D,O              ; Calculate the address in the cat
              	LD E,C              ; of this tracks file entry.
              	ADD HL,DE
              	LD A,(HL)           ; The file for this track.
              	CP B                ; Is it the file we want ?
              	JR NZ,Lfnextt       ; Try next track it if isn't.

              	POP HL              ; HL := file length.
              	POP DE              ; DE := start address.
              	PUSH BC             ; Save BC.
              	PUSH DE             ; Save start address.
              	CALL Block_length   ; DE := block length.

              	OR A                ; CY := 0
              	SBC HL,DE
              	JR Z,Lfend          ; If file is exactly one block.
              	JR C,Lfend          ; If file less than one block.

              	EX (SP),HL

              	; At this point, HL = start address,
              	; DE = block length, and remaining file length and
              	; track and file numbers saved.
               	CALL Load_block     ; Leaves HL pointing to start
                                    ; of the next block.
        	CALL Error_Msg

            	POP DE              ; Load remaining file.
            	POP BC
            	JR Sfnextb

Lfend      	ADD HL,DE           ; File was shorter than a block
                                    ; so make it positive again.
            	EX DE,HL            ; DE := length still to be loaded.
            	POP HL              ; HL := start address.

            	CALL Load_block
            	CALL Error_Msg

            	POP BC
            	RET

; *********************************************************
; ***                                                   ***
; ***               Find_word                           ***
; ***                                                   ***
; *********************************************************

; * Finds the address of a word in the cat if it exists and
; * the address of the first free point in the cat if it doesn't.

; * On exit:    if A=0, word not found, HL = end of used cat.
; *             else, word found, DE = address of first data field.
; *                               HL = address of length field,
; *                               A = file number.
; * Calls: WORD.
; * Called by: Enter_word, Lookup_word and DELETE.

                LD C,0              ; File number in C.

                LD HL,(RAMTOP)      ; Find start of names in cat.
                INC HL
                LD E,(HL)
                LD D,0
                INC HL
                ADD HL,DE

                LD DE,Pad           ; Address of name to be used.

Fwloop          EX HL,DE            ; DE := pos in cat, HL = pos in PAD.
                INC C               ; Next file number.

                LD A,(DE)           ; Length field.
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
                ADD HL,DE
                POP DE              ; Recover start of pad pointer.
                INC SP              ; Through away address of previous
                INC SP              ; length field.
                JR Fwloop           ; Try next word.

; *********************************************************
; ***                                                   ***
; ***               Enter_word                          ***
; ***                                                   ***
; *********************************************************

; * Enters a new file name into the catalogue.

; * On entry, the file name is in the input buffer.
; * HL = start address of file (0 for a DICT file).
; * DE = length of file in bytes.
; * On exit, DE and HL have their entry values and
; * B = the file number of the file.

; * Calls: Find_word and Error_Msg
; * Called by: STORE and BSTORE.

                PUSH DE             ; Save DE and HL.
                PUSH HL
                PUSH DE             ; Two copies of DE.

                CALL Find_word      ; Sets C to file number.
                OR A                ; Test for found.
                LD A,$07            ; Message "File name already exist".
                CALL NZ,Error_Msg   ; Call if found.

                LD B,(HL)           ; HL was set to point at
                                    ; the name in the PAD by Find_word.
                LD A,B
                LD (DE),A           ; Store name length in cat.
                                    ; Find word made DE point to
                                    ; the first free point in the cat.

                PUSH HL
                PUSH DE

                LD HL,(RAMTOP)      ; Start of cat.
                OR A                ; Clear carry.
                SBC HL,DE           ; HL := -bytes used.

                LD DE,($F8FE)       ; Cat size.
                ADD HL,DE           ; HL := bytes free.

                ADD 8               ; bytes needed +3.
                LD E,A
                LD D,0
                OR A                ; Clear carry.
                SBC HL,DE           ; Set carry if not enough room.

                POP DE
                POP HL
                LD A,$09            ; Message "Cat full".

                CALL C,Error_Msg

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
; ***               Lookup_word                        ***
; ***                                                   ***
; *********************************************************

; * Finds the file number, length and start address of the ;
; * file with name in the input buffer.

; * On exit: B = file number,
; *          DE = length,
; *          HL = start address (0 of DICT files).

; * Calls: Find_word and Error_Msg.
; * Called by: LD, BLD and RUN.

                CALL Find_word
                LD B,A              ; B := file number.
                OR A                ; Test for file not found.
                LD A,$08            ; Message "File name not found".
                CALL Z,Error_Msg    ; Raise error if not found.

                EX HL,DE            ; HL points to first data field.

                LD E,(HL)           ; Load DE with length.
                INC HL
                LD D, (HL)
                INC HL

                LD A,(HL)           ; Load HL with start address.
                INC HL
                LD H,(HL)
                LD L,A

                RET

; *********************************************************
; ***                                                   ***
; ***               Hex_byte                            ***
; ***                                                   ***
; *********************************************************

; * Prints the contents of A as a two digit hexadecimal number.

; * On entry: A = number to be printed.
; * Only register A is used.

; * Calls: Hex_char.
; * Called by: CAT.

                PUSH AF
                RR A                ; Get most significant nibble.
                RR A
                RR A
                RR A
                CALL Hex_char       ; Print it.
                POP AF              ; Implicit CALL of Hex_char
                                    ; and then return.

; *********************************************************
; ***                                                   ***
; ***               Hex_char                            ***
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
; * It is a FORTH word.

; * Gives a list of all the file names on the disk together
; * with their lengths and starting addresses.
; * (Dictionary files have "DICT" in place of the starting
; * address).
; * It then gives the number of bytes free for use on the disk
; * and a map of track usage.

; * Calls: Load_cat, Off, Hex_byte, Hex_char and Block_length.

                CALL Load_cat       ; Turns the motor on first.
                CALL Off

                LD A,$0D            ; CR.
                RST 8               ; Wrch.

                LD HL, (RAMTOP)     ; Point to start of names.
                INC HL
                LD E,(HL)           ; number of tracks.
                LD D,0
                INC HL
                ADD HL,DE

Namelp          LD C,$15            ; Number of character for name.
                LD A,(HL)
                OR A                ; Zero marks end of names.
                JR Z,findfre

                LD B,A              ; Name length in B.
Namepr          INC HL
                DEC C
                LD A,(HL)
                RST 8               ; Wrch.
                DJNZ Namepr         ; Print rest of name.

                ; Pad with spaces to get to the same column.

                LD A,C
                AND $1F             ; File name might have been
                                    ; longer than a line.
                                    ; (Assumes 32 chars per line).
                LD B,A
Catpdlp         LD A,$20            ; ASCII for space " ".
                RST 8               ; Wrch.
                DJNZ Catpdlp

                ; Print length in hex.

                INC HL
                LD C,(HL)
                INC HL
                LD A,(HL)
                CALL Hex_byte

                LD A,C
                CALL Hex_byte

                ; Print space.

                LD A,$20            ; ASCII for space " ".
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
                LD A,$19            ; Message "DICT".
                CALL Print
                JR Catjn

Catbyts         LD A,B              ; Print as hex
                CALL Hex_byte
                LD A,C
                CALL Hex_byte

                LD A,$0D            ; CR.
                RST 8               ; Wrch.
                JR Namelp           ; Next file name.

Findfre         LD A,$0D            ; CR.
                RST 8               ; Wrch.

                LD HL,(RAMTOP)      ; Find start of file for track
                INC HL              ; vector.
                LD B,(HL)           ; Number of tracks.
                DEC B               ; Two tracks used for cats.
                DEC B
                CALL Block_length   ; DE := number of bytes on a track.
                INC HL
                INC HL              ; First element of vector.

                PUSH HL             ; Temp start of vector.
                LD HL,0             ; Bytes free accumulated in HL
                LD C,0              ; and C.

Freelp          EX HL, (SP)         ; HL = vector pointer.
                INC HL              ; Next track.
                LD A,(HL)           ; File number of track.
                EX HL,(SP)          ; HL = Count.

                OR A                ; Test A=0 ie free.
                JR NZ,Notfree       ; If its used, don't count it.
                ADD HL,DE           ; If it is free, add a track full
                                    ; of bytes.
                JR NC,Notfree       ; Use C for high byte if 16 bits
                INC C               ; overflow.
Notfree         DJNZ Freelp         ; Try all tracks.

                POP DE             ; discard vector pointer.

                LD A,C              ; Most significant byte of count.
                CALL Hex_byte
                LD A,H              ; Middle byte of count.
                CALL Hex_byte
                LD A,L              ; Least significant byte of count.
                CALL Hex_byte

                LD A,$1A            ; " bytes free".
                CALL Print

                JP (IY)             ; FORTH return.

; *********************************************************
; ***                                                   ***
; ***               Dict_size                           ***
; ***                                                   ***
; *********************************************************

; * Sets DE and HL to the start and length of the catalogue.

; * DE = length of cat on exit.
; * HL = starting address of cat on exit.

; Calls: none.
; Called by: Load_cat and Save_cat.

                LD HL,(RAMTOP)
                INC HL              ; Miss the byte used for DRIVE.
                LD DE,($F8FE)       ; Cat size.
                DEC DE              ; Don't save drive number.
                RET

; *********************************************************
; ***                                                   ***
; ***               Block_length                        ***
; ***                                                   ***
; *********************************************************

; * Loads the block length of the current disk into DE without
; * changing any other registers.

; * DE = Block length on exit.

; * Calls: None.
; * Called by: Load_file, Save_file.

                PUSH HL             ; HL must be the same at the end.
                LD HL,(RAMTOP)
                INC HL
                INC HL              ; The block length is in the second
                LD E,(HL)           ; and third bytes of the cat.
                INC HL
                LD D,(HL)
                POP HL              ; Recover HL.
                RET

; *********************************************************
; ***                                                   ***
; ***               Delete_file                         ***
; ***                                                   ***
; *********************************************************

; * Deletes the file with its name in the pad from the
; * catalogue copy in RAM.

; * No args or results.

; * Calls: Find_word.
; * Called by: DELETE and RESTORE.

                CALL Find_word          ; DE := data field of word,
                                        ; HL := name length field,
                                        ; C := file number.

                LD C,A

                OR A                    ; Test for word not found.
                LD A,8                  ; "File name not found".
                CALL Z,Error_Msg

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
                JR Z,Delfd              ; If it is, set it free.
                JR C,Delfst             ; If its < this file, leave it.
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
                INC HL

Delfl2          LD A,(HL)               ; Name length.
                LD (DE),A               ; Store it even it its zero.
                OR A                    ; If it is zero, stop.
                RET Z
                ADD 4                   ; Four data bytes to copy as well.
                LD B,A

Delfl3          INC HL                  ; Copy one name and data.
                INC DE
                LD A,(HL)
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
; * It is a FORTH word.

; * Word to delete a file.

; * No arguments or results.

; * Calls; Load_cat,Delete_file, Save_cat and Off.

                CALL Word
                CALL Load_cat
                CALL Delete_file
                CALL Save_cat
                CALL Off
                JP (IY)             ; FORTH return.


; *********************************************************
; ***                                                   ***
; ***               Error_Msg                             ***
; ***                                                   ***
; *********************************************************

; * Prints and error message and then ABORTS (if A>0).

; * A = error number on entry.
; * If A = 0, the routine returns immediately.

; * Calls: Print_error and Off.
; * Called by: Almost everything.

                OR A                ; Test A=0
                RET Z               ; Return if it is.

                CALL Print_error
                CALL Off            ; Switch drive off.
                                    ; (Doesn't matter if it wasn't on).

                EI                  ; Could have been called with
                                    ; interrupts off.
                RST 32              ; ABORT.
                DB $FF              ; No error number.

; *********************************************************
; ***                                                   ***
; ***               Store                               ***
; ***                                                   ***
; *********************************************************

; * Store a dictionary file.

; * No arguments or results.

; * Calls; Save_file,Save_cat and Off.

                CALL Fill_length
                LD HL,(STKBOT)          ; End of dict.
                LD DE,(Forth)           ; Newest word in FORTH.
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
                LD A,$D                 ; "Why save 0 bytes?".
                CALL Z,Error_Msg

                PUSH HL
                LD HL,0                 ; 0 represents a DICT file.
                CALL Enter_word
                POP HL
                CALL Save_file
                CALL Save_cat
                CALL Off
                RET

; *********************************************************
; ***                                                   ***
; ***               DSAVE                               ***
; ***                                                   ***
; *********************************************************
; * It is a FORTH word.

; * Word to store a dictionary file.
; * Disk equivalent of SAVE.

; * No arguments or results.

; * Calls; Load_cat and Store.

                CALL Word
                CALL Load_cat
                CALL Save
                JP (IY)

; *********************************************************
; ***                                                   ***
; ***               DBSAVE                              ***
; ***                                                   ***
; *********************************************************
; * It is a FORTH word.

; * Wword to store a bytes file.
; * Disk equivalent of BSAVE.

; * No arguments or results.

; * Calls; Load_cat,Save_file,Save_cat and Off.

                CALL Word
                CALL Load_cat

                RST 24                  ; Pop DE from parameter stack.
                PUSH DE
                RST 24                  ; Pop DE from parameter stack.
                POP HL
                EX DE,HL                ; DE = length, HL = start.

                LD A,D                  ; Test len = 0.
                OR E
                LD A,$D                 ; "Why save 0 bytes?".
                CALL Z,Error_Msg

                CALL Enter_word
                CALL Save_file
                CALL Save_cat
                CALL Off
                JP (IY)                 ; FORTH return.

; *********************************************************
; ***                                                   ***
; ***               DBLOAD                              ***
; ***                                                   ***
; *********************************************************
; * It is a FORTH word.

; * Word to load a bytes file.
; * Disk equivalent of BLOAD.

; * No arguments or results.

; * Calls; Load_cat,Load_file, Error_Msg and Off.

                CALL Word
                CALL Load_cat
                RST 24                  ; Pop DE from parameter stack.
                                        ; First argument ignored.
                RST 24                  ; Pop DE from parameter stack.
                PUSH DE                 ; Save start address.
                CALL Lookup_word

                LD A,H                  ; Check not DICT.
                OR L
                LD A,$B                 ; Error: "BLD a dict".
                CALL Z,Error_Msg

                EX HL,(SP)              ; HL = parameter start.
                LD A,H                  ; If zero ...
                OR L
                JR Z,Bldlb              ; Use start from cat.
                EX HL,(SP)

Bldlb           POP HL
                CALL Load_file
                CAll Off
                JP (IY)                 ; FORTH return.

; *********************************************************
; ***                                                   ***
; ***               RESAVE                              ***
; ***                                                   ***
; *********************************************************
; * It is a FORTH word.

; * Word to save a dictionary file under a name which
; * already exists.

; * No arguments or results.

; * Calls; Load_cat,Delete,Save.

                CALL Word
                CALL Load_cat
                CALL Delete_file
                CALL Save_cat
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

; * Calls: The FORTH word WORD.

                LD DE,$20               ; Put a space on top,
                RST 16                  ; of the parameter stack.
                                        ; WORD uses this character
                                        ; as its termination mark.
                CALL Enter_forth	; Under address 0FDC7h: CD B9 04 - 01/06/2020
                dw WORD			; Under address:0FDCAh:	AB 05 - 01/06/2020
                dw End_forth		; Under address:0FDCEh:	OE 1A - 01/06/2020
                RST 24                  ; Discard the top e.,f the stack
                                        ; which is the address of the pad.
                RET

; *********************************************************
; ***                                                   ***
; ***               MAP                                 ***
; ***                                                   ***
; *********************************************************
; * It is a FORTH word.

; * Prints the file number for each track as a  single
; * hexadecimal digit. "-" for not used.

; * Calls: Hex_char, Load_cat and Off.

                CALL Load_cat
                CALL Off
                LD HL,(RAMTOP)
                INC HL
                LD B,(HL)                  ; Number of tracks,
                DEC B                       ; Two tracks uses cat.
                DEC B
                INC HL                      ; Skip block size.
                INC HL

Catmap          INC HL
                LD A,(HL)
                OR A                        ; Use space not 0 for an
                JR Z,Map1                   ; empty track. (0 for 0 mod 16) .
                CALL Hex_char
                JR Map2

Map1            LD A,$2D                    ; "-".
                RST 8                       ; Wrch

Map2            DJNZ Catmap                 ; Print all tracks
                JP (IY)                     ; FORTH return.

; *********************************************************
; ***                                                   ***
; ***               Fill_length                         ***
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
                ; OR E.			    ; Not in the ROM binary - 01/06/2020
                EX HL,DE                    ; HL is the address of the length
                                            ; field, DE is the length.
                LD (HL),E                   ; Store length.
                INC HL
                LD (HL),D
                LD HL,0                     ; (DICT) := 0
                LD (DICT),HL
                RET

; ---- in ROM ends at $FE06 - 01/06/2020

; * Block of 41 empty bytes

; ---- in ROM starts at $FE30
; *********************************************************
; ***                                                   ***
; ***               Message_space                       ***
; ***                                                   ***
; *********************************************************

; * Error messages printed on the screen
; * The Message_space starts at $FE30

; * A The text store used has message arranged consecutively
; * in order of increasing number.
; * 00 is used to separate message.
; * Characters with codes > 127 are interpreted as messages
; * to be inserted recursively.

; * Calls: None.

; * Called by: Print

; * Message no.	Text
; * 0   $00	(No error) - not included in the list
; * 1   $01	Check sum error
; * 2   $02	Verify error
; * 3   $03	Wot no disk?
; * 4	$04     Wot no index hole?
; * 5   $05     Wot no header?
; * 6   $06     Disk is write protected
; * 7   $07     File name already exist
; * 8	$08     File name not found
; * 9   $09     Cat full
; * 10  $0A     Disk is full
; * 11  $0B     BLD a dict??
; * 12  $0C     (spare)
; * 13  $0D     Why save 0 bytes
; * 14  $0E     (spare)
; * 15  $0F     Not enough RAM
; * 16  $10     Framing error
; * 17  $11     Overrun error
; * 18  $12     Parity error
; *
; * 24  $18     Second cat read
; * 25	$19	dict
; * 26	$1A	bytes free

Message_space	DB $00				; Text separator
Text01		DM "Wot no "

		DB $00				; Text separator
Text02 		DM " error"

		DB $00				; Text separator
Text03		DM "File name "

		DB $00				; Text separator
Text04		DM " full"

		DB $00				; Text separator
Text05		DM "Disk "

		DB $00				; Text separator
Text06		DM "Check sum"
		DB $82				; Link to " error"

		DB $00				; Text separator
Text07		DM "Verify"
		DB $82				; Link to " error"

		DB $00				; Text separator
Text08		DB $81				; Link to "Wot no "
		DM "disk ?"

		DB $00				; Text separator
Text09		DB $81				; Link to "Wot no "
		DM "index hole ?"

		DB $00				; Text separator
Text10		DB $81				; Link to "Wot no "
		DM "header ?"

		DB $00				; Text separator
Text11		DB $85				; Link to "Disk "
		DM "is write protected"

		DB $00				; Text separator
Text12		DB $83				; Link to "File name "
		DM "already exisits"

		DB $00				; Text separator
Text13		DB $83				; Link to "File name "
		DM "not found"

		DB $00				; Text separator
Text15		DM "Cat"
		DB $84				; Link to " full"

		DB $00				; Text separator
Text16		DB $85				; Link to "Disk "
		DM "is"
		DB $84				; Link to " full"

		DB $00				; Text separator
Text17		DM "BLD a "
		DB $99				; Link to "dict"
		DM " ??"

		DB $00				; Text separator

		DB $00				; Text separator
Text18		DM "Why save 0 bytes ?"

		DB $00				; Text separator

		DB $00				; Text separator
Text19		DM "Not enough RAM"

		DB $00				; Text separator
Text20		DM "Framing"
		DB $82				; Link to " error"

		DB $00				; Text separator
Text21		DM "Overrun"
		DB $82				; Link to " error"

		DB $00				; Text separator
Text22		DM "Parity"
		DB $82				; Link to " error"

		DB $00				; Text separator
Text23		DM "Second cat read "

		DB $00				; Text separator
Text24		DM "dict"

		DB $00				; Text separator
Text25		DM " bytes free "

		DB $00				; Text separator

; * Message_space end


; *	2st Block of Words Definition starts at $FF45

; *********************************************************
; ***                                                   ***
; ***         	  FORMAT word                           ***
; ***                                                   ***
; *********************************************************

LFF45	  	DM "FORMA"		; 'name field'
        	DB 'T' + $80		; last charater inverted

LFF4B        	DW $0013            	; ???

LFF4D	  	DW $F2AF		; 'link field' to 'name leght field' of
					; CPY word

LFF4F		DB $06			; 'name length field'

LFF50	  	DW $0EC3            	; 'code field' - docolon

LFF52		DW $1011		; Stack next word
		DW $0008		; Print routine from ACE ROM
		DW $1011		; Stack next word
		DW $0028		; Init. routine from ACE ROM
		DW $FFB5		; XFORMAT
		DW $04B6		; Exit

; *********************************************************
; ***                                                   ***
; ***         	  RUN word                              ***
; ***                                                   ***
; *********************************************************

LFF5E	  	DM "RU"			; 'name field'
        	DB 'N' + $80		; last charater inverted

LFF61        	DW $0017            	; ???

LFF63	  	DW $FF4F		; 'link field' to 'name leght field' of
					; FORMAT word

LFF65		DB $03			; 'name length field'

LFF66	  	DW $0EC3            	; 'code field' - docolon

LFF67		DW $FF8E		; SCRATCH
		DW $FFFE		; DLOAD
		DW $1011		; Stack next word
		DW $3C4C		; CURRENT system variable
		DW $08B3		; @
		DW $0E09		; 1+
		DW $069A		; EXECUTE
		DW $04B6		; Exit

; *********************************************************
; ***                                                   ***
; ***         	  MAP word                              ***
; ***                                                   ***
; *********************************************************

LFF78	  	DM "MA"			; 'name field'
		DB 'P' + $80		; last charater inverted

LFF7B        	DW $0007            	; ???

LFF7D	  	DW $FF65		; 'link field' to 'name leght field' of
					; RUN word

LFF7F		DB $03			; 'name length field'

LFF80	  	DW $F86C            	; “code field” address of machine code
					; for MAP word

; *********************************************************
; ***                                                   ***
; ***         	  SCRATCH word                              ***
; ***                                                   ***
; *********************************************************

LFF82	  	DM "SCRATC"		; 'name field'
		DB 'H' + $80		; last charater inverted

LFF89        	DW $0007            	; ???

LFF8B	  	DW $FF7F		; 'link field' to 'name leght field' of
					; MAP word

LFF8D		DB $07			; 'name length field'

LFF8E	  	DW $F875            	; “code field” address of machine code
					; for SCRATCH word

; *********************************************************
; ***                                                   ***
; ***         	  RESAVE word                           ***
; ***                                                   ***
; *********************************************************

LFF90	  	DM "RESAV"		; 'name field'
		DB 'E' + $80		; last charater inverted

LFF96        	DW $0007            	; ???

LFF98	  	DW $FF8D		; 'link field' to 'name leght field' of
					; SCRATCH word

LFF9A		DB $06			; 'name length field'

LFF9B	  	DW $F866            	; “code field” address of machine code
					; for RESAVE word

; *********************************************************
; ***                                                   ***
; ***         	  DRIVE word                            ***
; ***                                                   ***
; *********************************************************

LFF9D	  	DM "DRIV"		; 'name field'
		DB 'E' + $80		; last charater inverted

LFFA2        	DW $0007            	; ???

LFFA4	  	DW $FF9A		; 'link field' to 'name leght field' of
					; RESAVE word

LFFA6		DB $05			; 'name length field'

LFFA7	  	DW $F81E            	; “code field” address of machine code
					; for DRIVE word

; *********************************************************
; ***                                                   ***
; ***         	  XFORMAT word                          ***
; ***                                                   ***
; *********************************************************

LFFA9	  	DM "XFORMA"		; 'name field'
		DB 'T' + $80		; last charater inverted

LFFB0        	DW $0007            	; ???

LFFB2	  	DW $FFA6		; 'link field' to 'name leght field' of
					; DRIVE word

LFFB4		DB $07			; 'name length field'

LFFB5	  	DW $F830            	; “code field” address of machine code
					; for XFORMAT word

; *********************************************************
; ***                                                   ***
; ***         	  CAT word                              ***
; ***                                                   ***
; *********************************************************

LFFB7	  	DM "CA"			; 'name field'
		DB 'T' + $80		; last charater inverted

LFFBA        	DW $0007            	; ???

LFFBC	  	DW $FFB4		; 'link field' to 'name leght field' of
					; XFORMAT word

LFFBE		DB $03			; 'name length field'

LFFBF	  	DW $F830            	; “code field” address of machine code
					; for CAT word

; *********************************************************
; ***                                                   ***
; ***         	  DELETE word                           ***
; ***                                                   ***
; *********************************************************

LFFC1	  	DM "DELET"		; 'name field'
		DB 'E' + $80		; last charater inverted

LFFC7        	DW $0007            	; ???

LFFC9	  	DW $FFBE		; 'link field' to 'name leght field' of
					; CAT word

LFFCB		DB $06			; 'name length field'

LFFCC	  	DW $F854            	; “code field” address of machine code
					; for DELETE word

; *********************************************************
; ***                                                   ***
; ***         	  DBSAVE word                           ***
; ***                                                   ***
; *********************************************************

LFFCE	  	DM "DBSAV"		; 'name field'
        	DB 'E' + $80		; last charater inverted

LFFD4        	DW $FFFF            	; ???

LFFD6	  	DW $FFCB		; 'link field' to 'name leght field' of
					;  DELETE word

LFFD8		DB $06			; 'name length field'

LFFD9	  	DW $F860            	; “code field” address of machine code
					; for DBSAVE word

; *********************************************************
; ***                                                   ***
; ***         	  DSAVE word                            ***
; ***                                                   ***
; *********************************************************

LFFDB	  	DM "DSAV"		; 'name field'
        	DB 'E' + $80		; last charater inverted

LFFE0        	DW $0007            	; ???

LFFE2	  	DW $FFD8		; 'link field' to 'name leght field' of
					;  DBSAVE word

LFFE4		DB $05			; 'name length field'

LFFE5	  	DW $F85D            	; “code field” address of machine code
					; for DSAVE word

; *********************************************************
; ***                                                   ***
; ***         	  DBLOAD word                           ***
; ***                                                   ***
; *********************************************************

LFFE7	  	DM "DBLOA"		; 'name field'
        	DB 'D' + $80		; last charater inverted

LFFED        	DW $0007            	; ???

LFFEF	  	DW $FFE4		; 'link field' to 'name leght field' of
					;  DSAVE word

LFFF1		DB $06			; 'name length field'

LFFF2	  	DW $F863            	; “code field” address of machine code
					; for DBLOAD word

; *********************************************************
; ***                                                   ***
; ***         	  DLOAD word                            ***
; ***                                                   ***
; *********************************************************

LFFF4	  	DM "DLOA"		; 'name field'
        	DB 'D' + $80		; last charater inverted

LFFF9        	DW $3C47            	; ???

LFFFB	  	DW $FFF1		; 'link field' to 'name leght field' of
					;  DBLOAD word

LFFFD		DB $05			; 'name length field'

LFFFE	  	DW $F872            	; “code field” address of machine code
					; for DLOAD word

; * End of ASM code
