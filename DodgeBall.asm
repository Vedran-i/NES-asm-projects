.segment "HEADER"
  .byte $4E, $45, $53, $1A   ; iNES header identifier
  .byte 2                    ; 2x 16KB PRG code
  .byte 1                    ; 1x 8KB CHR data
  .byte $01, $00             ; Mapper 0, vertical mirroring

.segment "ZEROPAGE"
gamestate:  .res 1  ; .rs 1 means reserve one byte of space
ballx:      .res 1  ; ball horizontal position
bally:      .res 1  ; ball vertical position
ballup:     .res 1  ; 1 = ball moving up
balldown:   .res 1  ; 1 = ball moving down
ballleft:   .res 1  ; 1 = ball moving left
ballright:  .res 1  ; 1 = ball moving right
ballspeedx: .res 1  ; ball horizontal speed per frame
ballspeedy: .res 1  ; ball vertical speed per frame
paddle1ytop:   .res 1  ; player 1 paddle top vertical position
paddle2ybot:   .res 1  ; player 2 paddle bottom vertical position
buttons1:   .res 1  ; player 1 gamepad buttons, one bit per button
buttons2:   .res 1  ; player 2 gamepad buttons, one bit per button
score1:     .res 1  ; player 1 score, 0-15
score2:     .res 1  ; player 2 score, 0-15
time: .res 1 
lasttime: .res 1



;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen
  
RIGHTWALL      = $EE  ; when ball reaches one of these, do something
TOPWALL        = $36
BOTTOMWALL     = $A1
LEFTWALL       = $10
  
PADDLE1X       = $08  ; horizontal position for paddles, doesnt move
PADDLE2X       = $F0

spriteCollider1Top = $40 ; SPRITE COLLIDER 1 TOP
spriteCollider1Left = $41 ; SPRITE COLLIDER 1 LEFT
spriteCollider1Width = $42 ; SPRITE COLLIDER 1 WIDTH
spriteCollider1Height = $43 ; SPRITE COLLIDER 1 HEIGHT

spriteCollider2Top = $44 ; SPRITE COLLIDER 2 TOP
spriteCollider2Left = $45 ; SPRITE COLLIDER 2 LEFT
spriteCollider2Width = $46 ; SPRITE COLLIDER 2 WIDTH
spriteCollider2Height = $47 ; SPRITE COLLIDER 2 HEIGHT

spriteCollisionResult = $48 ; SPRITE COLLISION RESULT - 0 = NO COLLISION, 1 = COLLISION

SpriteA_Y = $0200  ; Y position of sprite A
SpriteA_X = $0203  ; X position of sprite A

SpriteB_Y = $0204  ; Y position of sprite B
SpriteB_X = $0207  ; X position of sprite B 


.segment "VECTORS"
  .addr NMI                    ; NMI vector
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
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
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
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down


LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$C0
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 

LoadBackgroundLoop:



  LDA background, x    
  STA $2007     

  LDY background    
  STA $2007   
   
  LDA #$01
  STA $0008

   
  
 

  INX                   ; X = X + 1
  CPX #$00
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
              
              
LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$FF              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$14              ; Compare X to hex $10, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down            
              

;;;Set some initial ball stats
  LDA #$01
  STA balldown
  STA ballright
  LDA #$00
  STA ballup
  STA ballleft
  
  LDA #$50
  STA bally
  
  LDA #$80
  STA ballx
  
  LDA #$02
  STA ballspeedx
  STA ballspeedy


;;:Set starting game state
  LDA #STATEPLAYING
  STA gamestate

;;; Set initial paddle state
  LDA $0204
  STA paddle1ytop

              
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop, waiting for NMI
  
 

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  JSR DrawScore

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
    
  ;;;all graphics updates done by here, run game engine


  JSR ReadController1  ;;get the current button data for player 1
  
  
GameEngine:  
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle    ;;game is displaying title screen
    
  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver  ;;game is displaying ending screen
  
  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying   ;;game is playing
GameEngineDone:  
  
  JSR UpdateSprites  ;;set ball/paddle sprites from positions



RTI             ; return from interrupt
 
 
; Delay loop
Delay:
  LDX #$10        ; Outer loop counter
OuterLoop:
  LDY #$10        ; Inner loop counter
InnerLoop:
  DEY             ; Decrement Y
  BNE InnerLoop   ; If Y is not zero, repeat the inner loop
  DEX             ; Decrement X
  BNE OuterLoop   ; If X is not zero, repeat the outer loop
  RTS             ; Return from subroutine

 
;;;;;;;;
 
EngineTitle:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load game screen
  ;;  set starting paddle/ball position
  ;;  go to Playing State
  ;;  turn screen on
  JMP GameEngineDone

;;;;;;;;; 
 
EngineGameOver:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load title screen
  ;;  go to Title State
  ;;  turn screen on 
  JMP GameEngineDone
 
;;;;;;;;;;;
 
EnginePlaying:

MoveBallRight:
  LDA ballright
  BEQ MoveBallRightDone   ;;if ballright=0, skip this section

  LDA ballx
  CLC
  ADC ballspeedx        ;;ballx position = ballx + ballspeedx
  STA ballx

  LDA ballx
  CMP #RIGHTWALL
  BCC MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section
  LDA #$00
  STA ballright
  LDA #$01
  STA ballleft         ;;bounce, ball now moving left

  JSR ball_bonce_sfx
  
  ;;in real game, give point to player 1, reset ball
MoveBallRightDone:




MoveBallLeft:
  LDA ballleft
  BEQ MoveBallLeftDone   ;;if ballleft=0, skip this section

  LDA ballx
  SEC
  SBC ballspeedx        ;;ballx position = ballx - ballspeedx
  STA ballx

  LDA ballx
  CMP #LEFTWALL
  BCS MoveBallLeftDone      ;;if ball x > left wall, still on screen, skip next section
  LDA #$01
  STA ballright
  LDA #$00
  STA ballleft         ;;bounce, ball now moving right

  JSR ball_bonce_sfx
  ;;in real game, give point to player 2, reset ball
MoveBallLeftDone:


MoveBallUp:
  LDA ballup
  BEQ MoveBallUpDone   ;;if ballup=0, skip this section

  LDA bally
  SEC
  SBC ballspeedy        ;;bally position = bally - ballspeedy
  STA bally

  LDA bally
  CMP #TOPWALL
  BCS MoveBallUpDone      ;;if ball y > top wall, still on screen, skip next section
  LDA #$01
  STA balldown
  LDA #$00
  STA ballup         ;;bounce, ball now moving down
MoveBallUpDone:


MoveBallDown:
  LDA balldown
  BEQ MoveBallDownDone   ;;if ballup=0, skip this section

  LDA bally
  CLC
  ADC ballspeedy        ;;bally position = bally + ballspeedy
  STA bally

  LDA bally
  CMP #BOTTOMWALL
  BCC MoveBallDownDone      ;;if ball y < bottom wall, still on screen, skip next section
  LDA #$00
  STA balldown
  LDA #$01
  STA ballup         ;;bounce, ball now moving down
MoveBallDownDone:

MovePaddleUp:
  ;;if up button pressed
  ;;  if paddle top > top wall
  ;;    move paddle top and bottom up
MovePaddleUpDone:

MovePaddleDown:
  ;;if down button pressed
  ;;  if paddle bottom < bottom wall
  ;;    move paddle top and bottom down
MovePaddleDownDone:
  
CheckPaddleCollision:
  ;;if ball x < paddle1x
  ;;  if ball y > paddle y top
  ;;    if ball y < paddle y bottom
  ;;      bounce, ball now moving left
CheckPaddleCollisionDone:










CheckPaddle1Collision:
  ;;if ball x < paddle1x
  ;;  if ball y > paddle y top
  ;;    if ball y < paddle y bottom
  ;;      bounce, ball now moving left
  ;; Check if on paddle x position





;60
CheckPaddle1CollisionDone:





    JSR CheckCollision   ; Check for collision




  JMP GameEngineDone
 
 
 
 
UpdateSprites:

  LDA bally  ;;update all ball sprite info
  STA $0200
  
  LDA #$75
  STA $0201
  
  LDA #$00
  STA $0202
  
  LDA ballx
  STA $0203
  
  
  ;;update paddle sprites
    ;;update paddle 1 sprites
  LDY paddle1ytop ;; load ball position and add paddle offset
  LDX #$00
  RTS
 
 
DrawScore:
  ;;draw score on screen using background tiles
  ;;or using many sprites
  RTS
 
 

ReadController1:

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
  LDA $0208       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0208       ; save sprite X position

  LDA $020C       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $020C       ; save sprite X position

  LDA $0204       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0204       ; save sprite X position

  LDA $0210       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0210       ; save sprite X position





ReadADone:        ; handling this button is done
  

ReadB: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0208       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0208       ; save sprite X position

  LDA $020C       ; load sprite X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $020C       ; save sprite X position

  LDA $0204       ; load sprite X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $0204       ; save sprite X position

  LDA $0210       ; load sprite X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $0210       ; save sprite X position


ReadBDone:        ; handling this button is done


ReadSelect: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadSelectDone   ; branch to ReadBDone if button is NOT pressed (0)



  LDA sprites
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16


  BRK

  
  ReadSelectDone:


ReadStart: 
  LDA $4016       ; player 1 - B
  AND #%00000001  ; only look at bit 0
  BEQ ReadStartDone   ; branch to ReadBDone if button is NOT pressed (0)



  JSR HitByBall

  
  ReadStartDone:




RTS

ball_bonce_sfx:


    lda #%00000000      ; Enable Square 1, Square 2, and Triangle channels
    sta $4015

    ; Triangle (D note)
    lda #%10000001      ; Triangle channel on
    sta $4008
    lda #$5E            ; D in NTSC mode
    sta $400A
    lda #$00
    sta $400B
  
DelayLoopP2F:
  ldx #$BA            ; Outer loop for a longer delay   
DelayLoopOuterP2F:
  ldy #$BA            ; Inner loop
DelayLoopInnerP2F:
  dey
  bne DelayLoopInnerP2F  ; Repeat inner loop until Y = 0
  dex
  bne DelayLoopOuterP2F  ; Repeat outer loop until X = 0

  lda #$00
  sta $4015

  RTS

HitByBall:



  LDX #$00        ; Start at the first sprite slot (X = 0)
MoveAndTrail:
  LDA $0208, X    ; Load the X position of the current sprite
  CLC
  ADC #$01        ; Increment the X position
  STA $0208, X    ; Save it back
  LDA $0204       ; load sprite X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $0205       ; save sprite X position

  LDA $0210       ; load sprite X position
  SEC             ; make sure the carry flag is clear
  SBC #$01        ; A = A + 1
  STA $0210       ; save sprite X position

  INX             ; Move to the next sprite slot
  CPX #$10        ; Stop after 16 sprites (adjust this for your trail length)
  BNE MoveAndTrail
  


CheckCollision:
    LDA SpriteA_X  ; Load Sprite A X
    SEC
    SBC SpriteB_X  ; A = A_X - B_X
    BCC NoCollisionX  ; If A_X < B_X, check other direction

    CMP #8         ; Check if A_X - B_X < 8 pixels
    BCS NoCollisionX  ; If not, no collision in X

    LDA SpriteA_Y  ; Load Sprite A Y
    SEC
    SBC SpriteB_Y  ; A = A_Y - B_Y
    BCC NoCollisionY  ; If A_Y < B_Y, check other direction

    CMP #8         ; Check if A_Y - B_Y < 8 pixels
    BCS NoCollisionY  ; If not, no collision in Y

    JMP CollisionDetected ; If we got here, there is a collision

NoCollisionX:
NoCollisionY:
    RTS  ; Return (no collision)

CollisionDetected:
  
  JMP HitByBall





  RTS  


background:
  .byte $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 2
  .byte $47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47  ;;all sky

  .byte $45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45  ;;all sky
  .byte $47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47  ;;all sky

  .byte $45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45  ;;row 1
  .byte $47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47  ;;all sky

  .byte $45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45  ;;row 1
  .byte $47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47  ;;all sky

  .byte $45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45  ;;row 2
  .byte $47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47  ;;all sky

  .byte $45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45  ;;row 1
  .byte $47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47  ;;all sky

  .byte $45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45  ;;row 1
  .byte $47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$47  ;;all sky

  .byte $45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45  ;;row 2
  .byte $47,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$47  ;;row 2

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 13
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 14
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky



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
  .byte $80, $86, $00, $80   ;sprite 0
  .byte $80, $A0, $00, $88   ;sprite 1
  .byte $88, $B0, $00, $80   ;sprite 2
  .byte $88, $B1, $10, $88   ;sprite 3
  .byte $80, $A0, $00, $80   ;sprite 3
  .byte $50, $F0, $00, $70   ;sprite 3
  .byte $50, $F1, $00, $78   ;sprite 3
  .byte $50, $F2, $00, $7F   ;sprite 3




 .segment "CHARS"
    .incbin "mario.chr"        ; Includes the 8 KB CHR-ROM graphics file
  

