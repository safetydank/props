' an attempt at an lmm virtual machine

.xinfreq 5000000
.clkmode xtal1+pll1x

org

begin
                mov             pc, entry
nxt             
                rdlong          instr, pc
                add             pc, #4
instr           res             1               ' instruction from hub goes here
                jmp             #nxt
                jmp             #begin          ' restart at the end...shouldn't reach here

LMM_jmp         rdlong          pc, pc

pc              long            0
entry           long            @LmmStart

' program variables
mask            long            $00FF_0000      ' mask for LED pins
cps             long            5_000_000       ' number of clocks per second
time            res             1               ' a temporary word for computing delays

.horg $1000

LmmStart        
                mov             DIRA, mask      ' pins to output
                mov             time, CNT       ' get system counter...
                add             time, cps       ' ...and add a second.
                
blink           or              OUTA, mask      ' OR our pins to high
                waitcnt         time, cps       ' wait, adding another second when we're ready
                andn            OUTA, mask      ' AND-NOT our pins to low
                waitcnt         time, cps       ' wait, adding another second when we're ready

                jmp             #LMM_jmp        ' ljmp =@blink
                long            @blink

