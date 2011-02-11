' Run clock at 20 MHZ (5M x 4)
.xinfreq    5000000
.clkmode    xtal1+pll4x

'***************************************
'*  Full-Duplex Serial Driver v1.1     *
'*  Author: Chip Gracey                *
'*  Copyright (c) 2006 Parallax, Inc.  *
'*  See end of file for terms of use.  *
'***************************************

'
' Entry
'

org 0
                      coginit   serial_cog              ' start serial driver on cog 1
begin
                      rdlong    :t1, #@tx_head
                      add       :t1, #1
                      and       :t1, #$f
                      rdlong    :t2, #@tx_tail
                      cmp       :t1, :t2    wz
            if_z      jmp       #begin                  ' wait for tail to catch up
                      wrlong    :t1, :addr              ' increment tx address
                      jmp       #begin

:t1                   res       1
:t2                   res       1
:addr                 long      @tx_head

'                               |          PAR          |        ADDRESS        |  NEW (1)
serial_cog            long      ((@rx_head >> 2) << 18) | ((@driver >> 2) << 4) |   %1001
time                  res       1
cps                   long      5000000 / 10

fit 496

org 0

' Hub variables (params)

rx_head                 long    0
rx_tail                 long    0
tx_head                 long    0
tx_tail                 long    0
rx_pin                  long    31
tx_pin                  long    30
rxtx_mode               long    0
bit_ticks               long    20000000 / 115200
buffer_ptr              long    @rx_buffer
                                           
rx_buffer               res     4           'transmit and receive buffers

' Hello world!!!!!
tx_buffer               byte    "Hello world!!!!!"

'***********************************
'* Assembly language serial driver *
'***********************************

'
org 0

driver                  mov     t1, par               'get structure address
                        add     t1,#4 << 2            'skip past heads and tails

                        rdlong  t2,t1                 'get rx_pin
                        mov     rxmask,#1
                        shl     rxmask,t2

                        add     t1,#4                 'get tx_pin
                        rdlong  t2,t1
                        mov     txmask,#1
                        shl     txmask,t2

                        add     t1,#4                 'get rxtx_mode
                        rdlong  rxtxmode,t1

                        add     t1,#4                 'get bit_ticks
                        rdlong  bitticks,t1

                        add     t1,#4                 'get buffer_ptr
                        rdlong  rxbuff,t1
                        mov     txbuff,rxbuff
                        add     txbuff,#16

                        test    rxtxmode,#%100  wz    'init tx pin according to mode
                        test    rxtxmode,#%010  wc
        if_z_ne_c       or      outa,txmask
        if_z            or      dira,txmask

                        mov     txcode,#transmit      'initialize ping-pong multitasking
'
'
' Receive
'
receive                 jmpret  rxcode,txcode         'run a chunk of transmit code, then return

                        test    rxtxmode,#%001  wz    'wait for start bit on rx pin
                        test    rxmask,ina      wc
        if_z_eq_c       jmp     #receive

                        mov     rxbits,#9             'ready to receive byte
                        mov     rxcnt,bitticks
                        shr     rxcnt,#1
                        add     rxcnt,cnt                          

:bit                    add     rxcnt,bitticks        'ready next bit period

:wait                   jmpret  rxcode,txcode         'run a chuck of transmit code, then return

                        mov     t1,rxcnt              'check if bit receive period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait

                        test    rxmask,ina      wc    'receive bit on rx pin
                        rcr     rxdata,#1
                        djnz    rxbits,#:bit

                        shr     rxdata,#32-9          'justify and trim received byte
                        and     rxdata,#$FF
                        test    rxtxmode,#%001  wz    'if rx inverted, invert byte
        if_nz           xor     rxdata,#$FF

                        rdlong  t2,par                'save received byte and inc head
                        add     t2,rxbuff
                        wrbyte  rxdata,t2
                        sub     t2,rxbuff
                        add     t2,#1
                        and     t2,#$0F
                        wrlong  t2,par

                        jmp     #receive              'byte done, receive next byte
'
'
' Transmit
'
transmit                jmpret  txcode,rxcode         'run a chunk of receive code, then return

                        mov     t1,par                'check for head <> tail
                        add     t1,#2 << 2
                        rdlong  t2,t1
                        add     t1,#1 << 2
                        rdlong  t3,t1
                        cmp     t2,t3           wz
        if_z            jmp     #transmit

                        add     t3,txbuff             'get byte and inc tail
                        rdbyte  txdata,t3
                        sub     t3,txbuff
                        add     t3,#1
                        and     t3,#$0F
                        wrlong  t3,t1

                        or      txdata,#$100          'ready byte to transmit
                        shl     txdata,#2
                        or      txdata,#1
                        mov     txbits,#11
                        mov     txcnt,cnt

:bit                    test    rxtxmode,#%100  wz    'output bit on tx pin according to mode
                        test    rxtxmode,#%010  wc
        if_z_and_c      xor     txdata,#1
                        shr     txdata,#1       wc
        if_z            muxc    outa,txmask        
        if_nz           muxnc   dira,txmask
                        add     txcnt,bitticks        'ready next cnt

:wait                   jmpret  txcode,rxcode         'run a chunk of receive code, then return

                        mov     t1,txcnt              'check if bit transmit period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait

                        djnz    txbits,#:bit          'another bit to transmit?

                        jmp     #transmit             'byte done, transmit next byte
'
'
' Uninitialized data
'
t1                      res     1
t2                      res     1
t3                      res     1

rxtxmode                res     1
bitticks                res     1

rxmask                  res     1
rxbuff                  res     1
rxdata                  res     1
rxbits                  res     1
rxcnt                   res     1
rxcode                  res     1

txmask                  res     1
txbuff                  res     1
txdata                  res     1
txbits                  res     1
txcnt                   res     1
txcode                  res     1

fit 496

' +------------------------------------------------------------------------------------------------------------------------------+
' ¦                                                   TERMS OF USE: MIT License                                                  ¦                                                            
' +------------------------------------------------------------------------------------------------------------------------------¦
' ¦Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    ¦ 
' ¦files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    ¦
' ¦modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software¦
' ¦is furnished to do so, subject to the following conditions:                                                                   ¦
' ¦                                                                                                                              ¦
' ¦The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.¦
' ¦                                                                                                                              ¦
' ¦THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          ¦
' ¦WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         ¦
' ¦COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   ¦
' ¦ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         ¦
' +------------------------------------------------------------------------------------------------------------------------------+

