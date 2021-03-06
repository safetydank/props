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

begin   mov DIRA, mask       ' pins to output
        mov time, CNT        ' get system counter...
        add time, cps        ' ...and add a second.
        
blink   or OUTA, mask        ' OR our pins to high
        waitcnt time, cps    ' wait, adding another second when we're ready
        andn OUTA, mask      ' AND-NOT our pins to low
        waitcnt time, cps    ' wait, adding another second when we're ready
        jmp #blink           ' loop

mask    long $00FF_0000      ' mask for LED pins
cps     long 12_000_000      ' number of clocks per second
time    res 1                ' a temporary word for computing delays

