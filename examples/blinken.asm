org
'
' Blinken - a semi-random variation of the Counter example
'

' Init
        mov time, PAR            ' Check if we're the initial cog (PAR = 0)
        cmp time, #0 wz
  if_z  jmp #common
 
' Setup for Cog > 0
        shr time, #2             ' Drop the hardwired bits off PAR
        shr mask, time           ' Shift our mask...
        ror rndmask, time        ' pseudo-randomize time 

' Generate the next coginit word
parloop
        add cog, addOne          ' Increment the next guy's PAR by one.
        djnz time, #parloop      ' Continue this until our PAR is exhausted.

common  
        xor cps, rndmask
        and cps, cpsmask

' Common code
        coginit cog              ' start a cog.  Fails silently on last cog.
        mov DIRA, mask           ' LED pin to output
        mov time, CNT            ' Set trigger time to system counter...
        add time, cps            ' ...plus this Cog's delay.
        
blink   andn OUTA, mask          ' AND-NOT our pins to low
        waitcnt time, cps        ' wait, add one second when we resume
        or OUTA, mask            ' OR our pins to high
        waitcnt time, cps        ' wait, add one second when we resume
        jmp #blink               ' loop


' Data
mask    long $0080_0000          ' mask for initial LED pin
cps     long 800_000_000         ' delay for MSB LED: 10 seconds

' Coginit word for starting first cog; subsequent ones are derived from this.
' Fields:    |       PAR       |      ADDRESS     |  NEW  |
cog     long %00_0000_0000_0001__00_0000_0000_1000__1__000
addOne  long %00_0000_0000_0001__00_0000_0000_0000__0__000 ' adds one to par

long    $ffff1c1c, $00ff1c1c
word    $ffff, $aaaa

time    res 1                    ' temporary word for delays
rndmask long %10000110_01011011_11001011_01101010
cpsmask long $00_ff_ff_ff
minimum long %00000000_00100000_00000000_00000000

