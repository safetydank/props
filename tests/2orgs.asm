ORG 0
'
' BLINKER
' Toggles the lights on the Propeller Demo Board once per second.
'
' THEORY OF OPERATION:
' The Demo Board's LEDs are connected to output pins 16-23.  Since the A port is
' presented as a single 32-bit word, this yields a hex mask:
'   $00FF_0000
' This program
' 1. Initializes the pins to be outputs by setting DIRA;
' 2. Sets up a delay of one second;
' 3. Repeatedly loops by turning the lights on, waiting, off, waiting.

begin   coginit cog          ' start another cog
        mov DIRA, mask       ' pins to output
        mov time, CNT        ' get system counter...
        add time, cps        ' ...and add a second.
        
blink   or OUTA, mask        ' OR our pins to high
        waitcnt time, cps    ' wait, adding another second when we're ready
        andn OUTA, mask      ' AND-NOT our pins to low
        waitcnt time, cps    ' wait, adding another second when we're ready
        jmp #blink           ' loop

cog     long %00_0000_0000_0001__00_0000_0001_0101__1__000
mask    long $00e0_0000      ' mask for LED pins
cps     long 12_000_000      ' number of clocks per second
time    res 1                ' a temporary word for computing delays

FIT 496

.horg $1000
ORG 0

cbegin  mov DIRA, cmask        ' pins to output
        mov ctime, CNT         ' get system counter...
        add ctime, ccps        ' ...and add a second.
        
cblink  or OUTA, cmask         ' OR our pins to high
        waitcnt ctime, ccps    ' wait, adding another second when we're ready
        andn OUTA, cmask       ' AND-NOT our pins to low
        waitcnt ctime, ccps    ' wait, adding another second when we're ready
        jmp #cblink

cmask    long $000F_0000      ' mask for LED pins
ccps     long 12_000_000      ' number of clocks per second
ctime    res 1                ' a temporary word for computing delays

FIT 496

