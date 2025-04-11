.segment "HEADER"
  .byte $4E, $45, $53, $1A   ; iNES header identifier
  .byte 2                    ; 2x 16KB PRG code
  .byte 1                    ; 1x 8KB CHR data
  .byte $01, $00             ; Mapper 0, vertical mirroring

.segment "ZEROPAGE"


.segment "VECTORS"
  .addr NMI                    ; NMI vector
  .addr RESET               ; Reset vector
  .addr 0                    ; IRQ vector (unused)

.segment "STARTUP"

.segment "CODE"

Sprite1_Y = $0200
Sprite2_Y = $0208
Sprite3_Y = $0210

Sprite1_X = $0203
Sprite2_X = $020B
Sprite3_X = $0213

PushB = $60
Ground = $81



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

  JSR vblankwait

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
   
  JSR vblankwait

LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
              
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons


ReadA: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  
  JSR Jump

ReadADone:        ; handling this button is done
  

ReadB: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  JSR Sunset
  
ReadBDone:        ; handling this button is done


PhysicsEngine:
  LDA $0200        ; Load sprite Y position
  CMP #$80         ; Check if at ground level
  BCS NoMove       ; If at or below ground, stop

    ; Move sprite down by 1
  CLC
  ADC #$02
  STA $0200        ; Store updated Y position

NoMove:


Sprite2movement:  ; both enemies
  LDA $0207       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$03        ; A = A - 1
  STA $0207       ; save sprite X position

  LDA $020B       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$03        ; A = A - 1
  STA $020B       ; save sprite X position

  LDA $0213       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0213       ; save sprite X position


Check_Collision:

  LDA Sprite1_X
  CLC
  ADC Sprite2_X
  BCC NoCollisionX

  CMP #$02
  BCS NoCollisionX

  LDA Sprite1_Y
  SEC
  SBC Sprite2_Y
  BCC NoCollisionY

  CMP #$20
  BCS NoCollisionY

  JMP CollisionDetected

NoCollisionX:

NoCollisionY:


CheckOtherCollision:

  LDA Sprite1_X
  CLC
  ADC Sprite3_X
  BCC NoCollisionX1

  CMP #$08
  BCS NoCollisionX1

  LDA Sprite1_Y
  SEC
  SBC Sprite3_Y
  BCC NoCollisionY1

  CMP #$08
  BCS NoCollisionY1

  JMP CollisionDetected

NoCollisionX1:

NoCollisionY1:

  RTS             ; return from interrupt

Sunset:

  LDA #PushB     
  CLC
  ADC #$01
  STA $020C

  LDA #$3F
  STA $2006
  LDA #$00
  STA $2006        ; Set PPU address to $3F00 (background color)

  LDA #$27         ; Color value (change this to any valid NES color)
  STA $2007        ; Write to palette

  RTS

CollisionDetected:
  LDA #$00
  STA $2000
  STA $2001

  LDA #$3F
  STA $2006
  LDA #$00
  STA $2006        ; Set PPU address to $3F00 (background color)

  LDA #$17         ; Color value (change this to any valid NES color)
  STA $2007        ; Write to palette


Jump:

  LDA Sprite1_Y
  SEC
  SBC #$05
  STA Sprite1_Y

  LDA $0200        ; Load sprite Y position
  CMP #$59         ; Check if at ground level
  BCS jumpdone     ; If at or below ground, stop
                   ; Move sprite down by 1 
  CLC
  ADC #$03

  STA $0200        ; Store updated Y position

jumpdone:

  RTS


vblankwait:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait
  RTS


background:
  .byte $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 2
  .byte $47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47  ;;all sky

attribute:
  .byte %11111111, %11111111, %01111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %01111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %01111111, %00011111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %01111111, %00011111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %01111111, %00011111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %01111111, %00011111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %01111111, %00011111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %01111111, %00011111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %01111111, %00011111, %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %01111111, %00011111, %11111111, %11111111, %11111111, %11111111
  

  .byte $24,$24,$24,$24, $47,$47,$24,$24 ,$47,$47,$47,$47, $47,$47,$24,$24 ,$24,$24,$24,$24 ,$24,$24,$24,$24, $24,$24,$24,$24, $55,$56,$24,$24  ;;brick bottoms
  .byte $24,$24,$24,$24, $47,$47,$24,$24 ,$47,$47,$47,$47, $47,$47,$47,$24 ,$24,$24,$24,$24 ,$24,$24,$24,$24, $24,$24,$24,$24, $55,$56,$24,$24  ;;brick bottoms
  
palette:
  .byte $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .byte $22,$0D,$20,$17,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .byte $80, $01, $00, $80   ;sprite 0
  .byte $80, $86, $00, $F8   ;sprite 1
  .byte $78, $86, $00, $F8   ;sprite 1
  .byte $20, $02, $00, $30   ;sprite 1
  .byte $5D, $75, $00, $FF   ;sprite 1

 .segment "CHARS"
    .incbin "mario.chr"        ; Includes the 8 KB CHR-ROM graphics file
  

