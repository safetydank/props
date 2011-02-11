org
'
' COUNTER
' Creates an 8-bit visual counter, using a separate Cog for each bit.
'
' THEORY OF OPERATION:
' This program spins off seven Cogs, in addition to the default Cog 0.  Each
' receives a parameter in its PAR register -- the numbers 1 through 7 (shifted
' left by two places, because PAR's two LSBs are hardwired to zero).
'
' Cog 0 blinks the LED on pin 23 every ten seconds, using code very similar to
' (and derived from) the Blinker example.
'
' Each subsequent Cog shifts the LED mask one place to the right, and divides
' the delay by two.
'
' The net effect: an eight-bit binary counter that increments at 12.8Hz.
'
' This program demonstrates multiprocessing with the Propeller, as well as
' cooperative control of the I/O ports.

' Init
begin   mov time, PAR            ' Check if we're the initial cog (PAR = 0)
        cmp time, #0 wz
  if_z  jmp #common
 
' Setup for Cog > 0
        shr time, #2             ' Drop the hardwired bits off PAR
        shr mask, time           ' Shift our mask...
        shr cps, time            ' ...and our delay.
        
' Generate the next coginit word
parloop
        add cog, addOne          ' Increment the next cog's PAR by one.
        djnz time, #parloop      ' Continue this until our PAR is exhausted.

' Common code
common  coginit cog              ' start a cog.  Fails silently on last cog.
        mov DIRA, mask           ' LED pin to output
        mov time, CNT            ' Set trigger time to system counter...
        add time, cps            ' ...plus this Cog's delay.
        
blink   andn OUTA, mask          ' AND-NOT our pins to low
        waitcnt time, cps        ' wait, add one second when we resume
        or OUTA, mask            ' OR our pins to high
        waitcnt time, cps        ' wait, add one second when we resume
        jmp #blink               ' loop


' Data
mask            long $0080_0000          ' mask for initial LED pin
cps             long 800_000_000         ' delay for MSB LED: 10 seconds

' Coginit word for starting first cog; subsequent ones are derived from this.
' Fields:            |   PAR    |    ADDRESS            |  NEW (0) |
cog             long (%1 << 18) |  ((@begin >> 2) << 4) | (%1000)
addOne          long (%1 << 18)

time            res 1                    ' temporary word for delays

