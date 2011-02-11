' an attempt at an lmm virtual machine
' noticed a bug when putting the pc res 1 line after the jmp #entry line!!!

org

entry
        mov     pc, #$38
nxt     
        rdlong  instr, pc
        add     pc, #4
instr   
        long    $0000
        jmp     #nxt

        jmp     #entry       ' restart at the end...shouldn't reach here

begin   mov DIRA, mask       ' pins to output
        mov time, CNT        ' get system counter...
        add time, cps        ' ...and add a second.
        
blink   or OUTA, mask        ' OR our pins to high
        waitcnt time, cps    ' wait, adding another second when we're ready
        andn OUTA, mask      ' AND-NOT our pins to low
        waitcnt time, cps    ' wait, adding another second when we're ready
        ' jmp #blink           ' loop
        mov pc, #$44
        jmp #nxt

mask    long $00FF_0000      ' mask for LED pins
cps     long 12_000_000      ' number of clocks per second

time    res 1                ' a temporary word for computing delays
pc      res 1                ' program counter

