.xinfreq 1_000_000
.clkmode XTAL1 + PLL1X

ORG 0
' A trivial example to test the call address rewrite functionality

begin   mov DIRA, mask       ' pins to output
        mov time, CNT        ' get system counter...
        add time, cps        ' ...and add a second.
loop
        call #blink
        jmp #loop
        long $00000000
        long $00000000
        long $00000000
        long $00000000
        
blink   or OUTA, mask        ' OR our pins to high
        waitcnt time, cps    ' wait, adding another second when we're ready
        andn OUTA, mask      ' AND-NOT our pins to low
        waitcnt time, cps    ' wait, adding another second when we're ready
blink_ret ret

' Data
mask    long $00FF_0000      ' mask for LED pins
cps     long 5_000_000       ' number of clocks per second
time    res 1                ' a temporary word for computing delays

FIT 496

