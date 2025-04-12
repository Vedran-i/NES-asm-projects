.segment "HEADER"
  .byte $4E, $45, $53, $1A   ; iNES header identifier
  .byte 2                    ; 2x 16KB PRG code
  .byte 1                    ; 1x 8KB CHR data
  .byte $01, $00             ; Mapper 0, vertical mirroring

.segment "ZEROPAGE"
controller1: .res 1          ; Reserve 1 byte for controller state
color_index:  .res 1  ; Reserve 1 byte to store the current index of the color


.segment "VECTORS"
  .addr NMI                  ; NMI vector
  .addr RESET               ; Reset vector
  .addr 0                    ; IRQ vector (unused)

.segment "STARTUP"

.segment "CODE"

RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2

LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address(sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$F0              ; Compare X to hex $20, decimal 32

  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  LDA #%10000000        ; enable NMI, sprites from Pattern Table 1
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001

  RTS

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer


  JSR LatchController

ReadA: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

  LDA $0201
  CLC
  ADC #$01
  STA $0201

ReadADone:        ; handling this button is done
  
ReadB: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

  
bullets:

  LDA $0204, X 



ReadBDone:        ; handling this button is done

ReadSelect: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadSelectDone   ; branch to ReadBDone if button is NOT pressed (0)

ReadSelectDone:        ; handling this button is done

ReadStart: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadStartDone   ; branch to ReadBDone if button is NOT pressed (0)

ReadStartDone:

ReadUp:
LDA $4016       ; player 1 - B
AND #%00000001  ; only look at bit 0
BEQ ReadUpDone   ; branch to ReadBDone if button is NOT pressed (0)

LDA $0200       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$02        ; A = A + 1
STA $0200       ; save sprite X position

ReadUpDone:

down:
LDA $4016       ; player 1 - B
AND #%00000001  ; only look at bit 0
BEQ downdone   ; branch to ReadBDone if button is NOT pressed (0)

LDA $0200       ; load sprite1 X position
CLC             ; make sure the carry flag is clear
ADC #$02        ; A = A + 1
STA $0200       ; save sprite X position

downdone:

left:
LDA $4016       ; player 1 - B
AND #%00000001  ; only look at bit 0
BEQ leftdone   ; branch to ReadBDone if button is NOT pressed (0) 

LDA $0203       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$02        ; A = A + 1
STA $0203       ; save sprite X position

leftdone:

right:

LDA $4016       ; player 1 - B
AND #%00000001  ; only look at bit 0
BEQ rightdone  ; branch to ReadBDone if button is NOT pressed (0)

LDA $0203       ; load sprite1 X position
CLC             ; make sure the carry flag is clear
ADC #$02        ; A = A + 1
STA $0203       ; save sprite X position

rightdone:


BGscroll:

LDA $0207       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$01        ; A = A + 1
STA $0207       ; save sprite X position 

LDA $020B       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$02        ; A = A + 1
STA $020B       ; save sprite X position 

LDA $020F       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$03        ; A = A + 1
STA $020F       ; save sprite X position 

LDA $0213       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$01        ; A = A + 1
STA $0213       ; save sprite X position 

LDA $0217       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$02        ; A = A + 1
STA $0217       ; save sprite X position 
  
LDA $021B       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$01        ; A = A + 1
STA $021B       ; save sprite X position  

LDA $021F       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$03        ; A = A + 1
STA $021F       ; save sprite X position  

LDA $0223       ; load sprite1 X position
SEC             ; make sure the carry flag is clear
SBC #$02        ; A = A + 1
STA $0223       ; save sprite X position  



  LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons
  RTS







hello:
  .byte $00, $00, $00, $00  ; Why do I need these here?
  .byte $00, $00, $00, $00
  .byte $6a, $00, $00, $6c
  .byte $6a, $01, $00, $76
  .byte $6c, $02, $00, $80
  .byte $6c, $02, $00, $8A
  .byte $6c, $03, $00, $94

palette:
  .byte $0F,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
  .byte $0F,$1C,$35,$02,$31,$02,$38,$3C,$0F,$1C,$35,$02,$31,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .byte $80, $75, $00, $80   ;sprite 0


  sprites2:
     ;vert tile attr horiz
  .byte $20, $74, $00, $99   ;sprite 0
  .byte $36, $74, $00, $66   ;sprite 0
  .byte $50, $74, $00, $89   ;sprite 0
  .byte $A1, $74, $00, $55   ;sprite 0
  .byte $BB, $74, $00, $89   ;sprite 0
  .byte $79, $74, $00, $20   ;sprite 0
  .byte $CC, $74, $00, $60   ;sprite 0
  .byte $80, $74, $00, $CB   ;sprite 0
  


 .segment "CHARS"
    .incbin "mario.chr"        ; Includes the 8 KB CHR-ROM graphics file
  

