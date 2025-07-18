.segment "HEADER"
    .byte "NES"
    .byte $1a
    .byte $02
    .byte $01
    .byte %00000001
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00, $00, $00, $00, $00
.segment "STARTUP"
.segment "ZEROPAGE"
    pointerLo: .res 1    ; pointer variables declared in RAM
    pointerHi: .res 1    ; low byte first, high byte immediately after
    GameState: .res 1
    Speed: .res 1
    Speed2: .res 1
.segment "CODE"



TitleScreen = $00
PlayingGame = $01 
GameOver = $02

Sprite1_Y = $0200
Sprite2_Y = $0208
Sprite3_Y = $0210
Sprite4_Y = $0214

Sprite1_X = $0203
Sprite2_X = $020B
Sprite3_X = $0213
Sprite4_X = $0217

PushB = $60
Ground = $81




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subroutines ;;;
vblankwait:
    BIT $2002 
    BPL vblankwait 
    RTS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init code ;;;
RESET:
    SEI 
    CLD 
    LDX #$40
    STX $4017
    LDX #$ff
    TXS 
    INX 
    STX $2000   ; disable NMI
    STX $2001   ; disable rendering
    STX $4010   ; disab;e DMC IRQs

    JSR vblankwait

    TXA 
clearmem:
    STA $0000,X
    STA $0100,X
    STA $0300,X
    STA $0400,X
    STA $0500,X
    STA $0600,X
    STA $0700,X
    LDA #$fe
    STA $0200,X
    LDA #$00
    INX 
    BNE clearmem 

    JSR vblankwait

    LDA $02     ; high byte for sprite memory
    STA $4014
    NOP 

clearnametables:
    LDA $2002   ; reset PPU status
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006
    LDX #$08
    LDY #$00
    LDA #$24    ; clear background tile
:
    STA $2007
    DEY 
    BNE :-
    DEX 
    BNE :-

loadpalettes:
    LDA $2002
    LDA #$3f
    STA $2006
    LDA #$00
    STA $2006
    LDX #$00
loadpalettesloop:
    LDA palette,X   ; load data from adddress (palette + X)
                        ; 1st time through loop it will load palette+0
                        ; 2nd time through loop it will load palette+1
                        ; 3rd time through loop it will load palette+2
                        ; etc
    STA $2007
    INX 
    CPX #$20
    BNE loadpalettesloop

loadsprites:
    LDX #$00
loadspritesloop:
    LDA sprites,X
    STA $0200,X
    INX 
    CPX #$20
    BNE loadspritesloop 
                
;;; Using nested loops to load the background efficiently ;;;
loadbackground:
    LDA $2002               ; read PPU status to reset the high/low latch
    LDA #$20
    STA $2006               ; write high byte of $2000 address
    LDA #$00
    STA $2006               ; write low byte of $2000 address

    LDA #<background 
    STA pointerLo           ; put the low byte of address of background into pointer
    LDA #>background        ; #> is the same as HIGH() function in NESASM, used to get the high byte
    STA pointerHi           ; put high byte of address into pointer

    LDX #$00                ; start at pointer + 0
    LDY #$00
outsideloop:

insideloop:
    LDA (pointerLo),Y       ; copy one background byte from address in pointer + Y
    STA $2007               ; runs 256*4 times

    INY                     ; inside loop counter
    CPY #$00                
    BNE insideloop          ; run inside loop 256 times before continuing

    INC pointerHi           ; low byte went from 0 -> 256, so high byte needs to be changed now

    INX                     ; increment outside loop counter
    CPX #$04                ; needs to happen $04 times, to copy 1KB data
    BNE outsideloop         


    CLI 
    LDA #%10010000  ; enable NMI, sprites from pattern table 0, background from 1
    STA $2000
    LDA #%00011110  ; background and sprites enable, no left clipping
    STA $2001

forever:
    JMP forever 

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NMI / vblank ;;;
VBLANK:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address 
    LDA #$02
    STA $4014


GameEngine:  
  LDA GameState
  CMP #0
  BEQ EngineTitle    ;;game is displaying title screen


  LDA GameState
  CMP #1
  BEQ EnginePlaying   ;;game is playing

  LDA GameState
  CMP #2
  BEQ GameOverScreen   ;;game is playing


GameOverScreen:





LDA #$00
STA $2000
STA $2001


LDA #$50
STA $2000
STA $2001



LDA #%00001000       ; Bit 3 set = enable noise channel
  STA $4015

; Configure Noise Channel Envelope
  LDA #%00110100       ; Volume 4, Envelope disabled, decay rate fast
                         ; Bit 7 = 0 (disable envelope)
                         ; Bit 6 = 1 (constant volume)
                         ; Bit 5-0 = 4 (volume)
  STA $400C            ; Write to Noise Envelope/Volume register

; Configure Noise Frequency
  LDA #%00111110       ; Frequency index = $23 (higher frequency for sharpness)
                         ; Bit 7 = 0 (non-looping random noise)
                         ; Bits 4-0 = $23 (frequency index)
  STA $400E            ; Write to Noise Period register

; Restart the length counter
  LDA #%00001000       ; Load length counter (short duration)
  STA $400F            ; Writing to $400F also resets envelope and length counter

DelayLoopX11:
  LDX #$Fb            ; Outer loop for a longer delay   
DelayLoopOuterX11:
  LDY #$Fb            ; Inner loop
DelayLoopInnerX11:
  DEY
  BNE DelayLoopInnerX11  ; Repeat inner loop until Y = 0
  DEX
  BNE DelayLoopOuterX11  ; Repeat outer loop until X = 0

;Stops sound
  LDA #$00
  STA $4015


LDA #%00001000       ; Bit 3 set = enable noise channel
  STA $4015

; Configure Noise Channel Envelope
  LDA #%00110100       ; Volume 4, Envelope disabled, decay rate fast
                         ; Bit 7 = 0 (disable envelope)
                         ; Bit 6 = 1 (constant volume)
                         ; Bit 5-0 = 4 (volume)
  STA $400C            ; Write to Noise Envelope/Volume register

; Configure Noise Frequency
  LDA #%00111111       ; Frequency index = $23 (higher frequency for sharpness)
                         ; Bit 7 = 0 (non-looping random noise)
                         ; Bits 4-0 = $23 (frequency index)
  STA $400E            ; Write to Noise Period register

; Restart the length counter
  LDA #%00001000       ; Load length counter (short duration)
  STA $400F            ; Writing to $400F also resets envelope and length counter

DelayLoopX1:
  LDX #$Fb            ; Outer loop for a longer delay   
DelayLoopOuterX1:
  LDY #$Fb            ; Inner loop
DelayLoopInnerX1:
  DEY
  BNE DelayLoopInnerX1  ; Repeat inner loop until Y = 0
  DEX
  BNE DelayLoopOuterX1  ; Repeat outer loop until X = 0

;Stops sound
  LDA #$00
  STA $4015





GameEngineDone:





EngineTitle:



LatchControllerT:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons


ReadAT: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADoneT   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)


  LDA #$01
  STA GameState



ReadADoneT:        ; handling this button is done



 JMP GameEngineDone

EnginePlaying:



LDA #$03
STA Speed
LDA #$01
STA Speed2

     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite / nametable / attributes / palettes

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

  LDA $0210
  STA $0210



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
  SBC Speed        ; A = A - 1
  STA $0207       ; save sprite X position

  LDA $020B       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC Speed        ; A = A - 1
  STA $020B       ; save sprite X position

  LDA $0213       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC Speed2        ; A = A - 1
  STA $0213       ; save sprite X position

  LDA $0217       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0217       ; save sprite X position





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





CheckOtherOtherCollision:



  LDA Sprite1_X
  CLC
  ADC Sprite4_X
  BCC NoCollisionX11

  CMP #$08
  BCS NoCollisionX11

  LDA Sprite1_Y
  SEC
  SBC Sprite4_Y
  BCC NoCollisionY11

  CMP #$08
  BCS NoCollisionY11

  JMP CollisionDetectedSpeed

NoCollisionX11:

NoCollisionY11:



RTI             ; return from interrupt

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

LDA #$08
STA Speed



RTS



CollisionDetected: 


LDA $202
CLC
ADC #$05
STA $202



  LDA #$3F
  STA $2006
  LDA #$00
  STA $2006        ; Set PPU address to $3F00 (background color)

  LDA #$16         ; Color value (change this to any valid NES color)
  STA $2007        ; Write to palette

  LDA #$02
  STA GameState



CollisionDetectedSpeed:

LDA #$08
STA Speed




RTI


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

delay:
DelayLoop2:
    ldx #$FF            ; Outer loop for a longer delay   
DelayLoopOuter2:
    ldy #$FF            ; Inner loop
DelayLoopInner2:
    dey
    CMP #$00
    bne DelayLoopInner2  ; Repeat inner loop until Y = 0
    dex
    bne DelayLoopOuter2  ; Repeat outer loop until X = 0
RTS


background:
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$08  ;;row 2
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$08  ;;all sky

  .byte $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 3
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .byte $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 4
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 6
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 7
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .byte $24,$24,$24,$24,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 8
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 9
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 10
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 11
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .byte $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 12
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 13
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 14
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 15
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .byte $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 16
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 17
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C
  .byte $6C,$6C,$6C,$6C,$6C,$6D,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C,$6C

  .byte $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 19
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$03,$03,$54,$24,$24  ;;some brick tops

  .byte $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 20
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 21
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 22
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 23
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .byte $25,$24,$24,$24,$47,$47,$24,$24,$25,$47,$47,$47,$47,$47,$24,$24  ;;row 24
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;brick bottoms

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 25
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 26
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 27
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;some brick tops

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 28
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 29
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 30
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky




attributes:  ;8 x 8 = 64 bytes
  .byte %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %01010000
  .byte %00000000, %01010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .byte %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .byte %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .byte %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .byte %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .byte %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .byte %00000000, %00010000, %01010000, %00010000, %00000000, %00000000, %00000000, %00110000


  .byte $24,$24,$24,$24, $47,$47,$24,$24 
  .byte $47,$47,$47,$47, $47,$47,$24,$24 
  .byte $24,$24,$24,$24 ,$24,$24,$24,$24
  .byte $24,$24,$24,$24, $55,$56,$24,$24  ;;brick bottoms
  .byte $47,$47,$47,$47, $47,$47,$24,$24 
  .byte $24,$24,$24,$24 ,$24,$24,$24,$24
  .byte $24,$24,$24,$24, $55,$56,$24,$24 



palette:
  .byte $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .byte $22,$16,$27,$18,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .byte $80, $B3, $00, $80   ;player
  .byte $80, $86, $00, $68   ;wall
  .byte $78, $86, $00, $68   ;wall
  .byte $20, $02, $00, $30   ;sun
  .byte $5D, $80, $00, $FF   ;ball
  .byte $5D, $75, $02, $4F   ;ball2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
    .word VBLANK 
    .word RESET 
    .word 0
.segment "CHARS"
    .incbin "mario.chr"
