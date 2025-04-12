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
  CPX #$FF              ; FF makes it so that 64 sprites can appear on screen. Lower number = less sprites

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

  LDA $0214       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$05       ; A = A + 1
  STA $0214       ; save sprite X position

  LDA $0216       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$00       ; A = A + 1
  STA $0216       ; save sprite X position

  LDA $0218       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$05       ; A = A + 1
  STA $0218       ; save sprite X position

  LDA $0219       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$00       ; A = A + 1
  STA $0219       ; save sprite X position

  LDA $021C       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$05       ; A = A + 1
  STA $021C       ; save sprite X position

  LDA $021A       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$00       ; A = A + 1
  STA $021A       ; save sprite X position

  LDA $021E       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$00       ; A = A + 1
  STA $021E       ; save sprite X position
  
  LDA $0222       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$05       ; A = A + 1
  STA $0222       ; save sprite X position

  LDA $0210       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$05       ; A = A + 1
  STA $0210       ; save sprite X position


; Enable the noise channel
  LDA #%00001000       ; Bit 3 set = enable noise channel
  sta $4015

    ; Configure Noise Channel Envelope
  lda #%00110100       ; Volume 4, Envelope disabled, decay rate fast
                         ; Bit 7 = 0 (disable envelope)
                         ; Bit 6 = 1 (constant volume)
                         ; Bit 5-0 = 4 (volume)
  sta $400C            ; Write to Noise Envelope/Volume register

    ; Configure Noise Frequency
  lda #%00100000       ; Frequency index = $23 (higher frequency for sharpness)
                         ; Bit 7 = 0 (non-looping random noise)
                         ; Bits 4-0 = $23 (frequency index)
  sta $400E            ; Write to Noise Period register

    ; Restart the length counter
  lda #%00001000       ; Load length counter (short duration)
  sta $400F            ; Writing to $400F also resets envelope and length counter

DelayLoopX5:
  ldx #$10            ; Outer loop for a longer delay   
DelayLoopOuterX5:
  ldy #$10            ; Inner loop
DelayLoopInnerX5:
  dey
  bne DelayLoopInnerX5  ; Repeat inner loop until Y = 0
  dex
  bne DelayLoopOuterX5  ; Repeat outer loop until X = 0

  lda #$00
  sta $4015



ReadADone:        ; handling this button is done
  
ReadB: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

  LDA $0214       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$02       ; A = A + 1
  STA $0214       ; save sprite X position

  LDA $0216       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$00       ; A = A + 1
  STA $0216       ; save sprite X position

  LDA $0218       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$02       ; A = A + 1
  STA $0218       ; save sprite X position

  LDA $0219       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$00       ; A = A + 1
  STA $0219       ; save sprite X position

  LDA $021C       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$02       ; A = A + 1
  STA $021C       ; save sprite X position

  LDA $021A       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$00       ; A = A + 1
  STA $021A       ; save sprite X position

  LDA $021E       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$00       ; A = A + 1
  STA $021E       ; save sprite X position
  
  LDA $0222       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$02       ; A = A + 1
  STA $0222       ; save sprite X position

  LDA $0210       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$02       ; A = A + 1
  STA $0210       ; save sprite X position



  ; Enable the noise channel
    lda #%00001000       ; Bit 3 set = enable noise channel
    sta $4015

    ; Configure Noise Channel Envelope
    lda #%00110100       ; Volume 4, Envelope disabled, decay rate fast
                         ; Bit 7 = 0 (disable envelope)
                         ; Bit 6 = 1 (constant volume)
                         ; Bit 5-0 = 4 (volume)
    sta $400C            ; Write to Noise Envelope/Volume register

    ; Configure Noise Frequency
    lda #%00100000       ; Frequency index = $23 (higher frequency for sharpness)
                         ; Bit 7 = 0 (non-looping random noise)
                         ; Bits 4-0 = $23 (frequency index)
    sta $400E            ; Write to Noise Period register

    ; Restart the length counter
    lda #%00001000       ; Load length counter (short duration)
    sta $400F            ; Writing to $400F also resets envelope and length counter

DelayLoopX:
    ldx #$10            ; Outer loop for a longer delay   
DelayLoopOuterX:
    ldy #$10            ; Inner loop
DelayLoopInnerX:
    dey
    bne DelayLoopInnerX  ; Repeat inner loop until Y = 0
    dex
    bne DelayLoopOuterX  ; Repeat outer loop until X = 0

    lda #$00
    sta $4015


ReadBDone:        ; handling this button is done

ReadSelect: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadSelectDone   ; branch to ReadBDone if button is NOT pressed (0)
   
  LDA $0202       ; load sprite1 X position
  SEC             ; make sure the carry flag is clear
  SBC #$02        ; A = A + 1
  STA $0203       ; save sprite X position
  
  LDA $0207       ; load sprite2 X position
  SEC             ; make sure the carry flag is clear
  SBC #$02        ; A = A + 1
  STA $0203       ; save sprite X position

  LDA $020B       ; load sprite3 X position
  SEC             ; make sure the carry flag is clear
  SBC #$02        ; A = A + 1
  STA $0203       ; save sprite X position

  LDA $020F       ; load sprite4 X position
  SEC             ; make sure the carry flag is clear
  SBC #$02        ; A = A + 1
  STA $0203       ; save sprite X position

ReadSelectDone:        ; handling this button is done

ReadStart: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadStartDone   ; branch to ReadBDone if button is NOT pressed (0)
   
  LDA $0200       ; load sprite1 Y position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $0200       ; save sprite X position
  
  lda #01
  sta $2004
 
  

  ReadStartDone:



  ReadUp: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadUpDone   ; branch to ReadBDone if button is NOT pressed (0)


; Enable Sound channel
    lda #%00000001
    sta $4015           ; Enable Square 1 channel, disable others

    lda #%00010110
    sta $4015           ; Enable Square 2, Triangle, and DMC channels. Disable Square 1 and Noise.

    lda #$0F
    sta $4015           ; Enable Square 1, Square 2, Triangle, and Noise channels. Disable DMC.

    lda #%00000111      ; Enable Square 1, Square 2, and Triangle channels
    sta $4015

    ; Play C#m chord
    ; Square 1 (C# note)
    lda #%00111000      ; Duty 00, Volume 8 (half volume)
    sta $4000
    lda #$C9            ; $0C9 is a C# in NTSC mode
    sta $4002           ; Low 8 bits of period
    lda #$00
    sta $4003           ; High 3 bits of period

    ; Square 2 (E note)
    lda #%01110110      ; Duty 01, Volume 6
    sta $4004
    lda #$A9            ; $0A9 is an E in NTSC mode
    sta $4006
    lda #$00
    sta $4007


    


        ; Delay loop (adjust for the desired delay duration)
DelayLoop2:
    ldx #$FF            ; Outer loop for a longer delay   
DelayLoopOuter2:
    ldy #$FF            ; Inner loop
DelayLoopInner2:
    dey
    bne DelayLoopInner2  ; Repeat inner loop until Y = 0
    dex
    bne DelayLoopOuter2  ; Repeat outer loop until X = 0
    

    ; Stop sound after the delay
    lda #$00
    sta $4015           ; Disable all sound channels



ReadUpDone:
  
  ReadDown: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadDownDone   ; branch to ReadBDone if button is NOT pressed (0)


 
 ReadDownDone:

  ReadLeft: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadLeftDone   ; branch to ReadBDone if button is NOT pressed (0)

  LDA $0203       ; load sprite1 X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $0203       ; save sprite X position
  
  LDA $0207       ; load sprite2 X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $0207       ; save sprite X position

  LDA $020B       ; load sprite3 X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $020B       ; save sprite X position

  LDA $020F       ; load sprite4 X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $020F       ; save sprite X position

  ReadLeftDone:

  ReadRight:
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadRightDone   ; branch to ReadBDone if button is NOT pressed (0)

  LDA $0203       ; load sprite1 X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0203       ; save sprite X position
  
  LDA $0207       ; load sprite2 X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0207       ; save sprite X position

  LDA $020B       ; load sprite3 X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $020B       ; save sprite X position

  LDA $020F       ; load sprite4 X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $020F       ; save sprite X position

ReadRightDone:





  RTI




  LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons
  RTS





palette:
  .byte $0F,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
  .byte $0F,$0C,$35,$16,$31,$02,$38,$3C,$0F,$1C,$35,$02,$31,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .byte $80, $00, $00, $80   ;sprite 0
  .byte $80, $01, $00, $88   ;sprite 1
  .byte $88, $34, $00, $80   ;sprite 2
  .byte $88, $35, $00, $88   ;sprite 3

sprites2:
     ;vert tile attr horiz
  .byte $10, $65, $00, $80   ;sprite 0
  .byte $50, $65, $00, $80   ;sprite 0
  .byte $90, $64, $00, $80   ;sprite 0
  .byte $D0, $65, $00, $80   ;sprite 0

sprites3:
     ;For right side

  .byte $08, $02, $10, $08
  .byte $10, $02, $10, $08
  .byte $18, $02, $10, $08
  .byte $20, $02, $10, $08
  .byte $28, $02, $10, $08
  .byte $30, $02, $10, $08
  .byte $38, $02, $10, $08
  .byte $40, $02, $10, $08
  .byte $48, $02, $10, $08
  .byte $50, $02, $10, $08
  .byte $58, $02, $10, $08
  .byte $60, $02, $10, $08
  .byte $68, $02, $10, $08
  .byte $70, $02, $10, $08
  .byte $78, $02, $10, $08
  .byte $80, $02, $10, $08
  .byte $88, $02, $10, $08
  .byte $90, $02, $10, $08
  .byte $98, $02, $10, $08
  .byte $A0, $02, $10, $08
  .byte $A8, $02, $10, $08
  .byte $B0, $02, $10, $08
  .byte $B8, $02, $10, $08
  .byte $C0, $02, $10, $08
  .byte $C8, $02, $10, $08
  .byte $D0, $02, $10, $08
  .byte $D8, $02, $10, $08
  .byte $E0, $02, $10, $08

  .byte $08, $02, $10, $F8   ;sprite 0
  .byte $10, $02, $10, $F8   ;sprite 0
  .byte $18, $02, $10, $F8   ;sprite 0
  .byte $20, $02, $10, $F8   ;sprite 0
  .byte $28, $02, $10, $F8   ;sprite 0
  .byte $30, $02, $10, $F8   ;sprite 0
  .byte $38, $02, $10, $F8   ;sprite 0
  .byte $40, $02, $10, $F8   ;sprite 0
  .byte $48, $02, $10, $F8   ;sprite 0
  .byte $50, $02, $10, $F8   ;sprite 0
  .byte $58, $02, $10, $F8   ;sprite 0
  .byte $60, $02, $10, $F8   ;sprite 0
  .byte $68, $02, $10, $F8   ;sprite 0
  .byte $70, $02, $10, $F8   ;sprite 0
  .byte $78, $02, $10, $F8   ;sprite 0
  .byte $80, $02, $10, $F8   ;sprite 0
  .byte $88, $02, $10, $F8   ;sprite 0
  .byte $90, $02, $10, $F8   ;sprite 0
  .byte $98, $02, $10, $F8   ;sprite 0
  .byte $A0, $02, $10, $F8   ;sprite 0
  .byte $A8, $02, $10, $F8   ;sprite 0
  .byte $B0, $02, $10, $F8   ;sprite 0
  .byte $B8, $02, $10, $F8   ;sprite 0
  .byte $C0, $02, $10, $F8   ;sprite 0
  .byte $C8, $02, $10, $F8   ;sprite 0
  .byte $D0, $02, $10, $F8   ;sprite 0
  .byte $D8, $02, $10, $F8   ;sprite 0
  .byte $E0, $02, $10, $F8   ;sprite 0







 .segment "CHARS"
    .incbin "mario.chr"        ; Includes the 8 KB CHR-ROM graphics file

    

  
 
  

