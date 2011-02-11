''*****************************************************
''*  MCP3208 12-bit/8-channel ADC Driver v1.0         *
''*  also provides up to two 12-bit sigma-delta DACs  *
''*  Author: Chip Gracey                              *
''*  Copyright (c) 2005 Parallax, Inc.                *
''*  See end of file for terms of use.                *
''*****************************************************

'************************************
'* Assembly language MCP3208 driver *
'************************************

org

'
'
' Entry
'
entry                   mov     t1,par                  'read parameters

                        call    #param                  'setup DIN/DOUT pin
                        mov     dmask,t2

                        call    #param                  'setup CLK pin
                        mov     cmask,t2

                        call    #param                  'setup CS pin
                        mov     smask,t2

                        call    #param                  'set mode
                        mov     enables,t3

                        call    #param                  'setup DAC configuration
        if_c            or      dira,t2
        if_c            movs    ctra,t3
        if_c            movi    ctra,#%00110_000

                        shr     t3,#8
                        call    #param2
        if_c            or      dira,t2
        if_c            movs    ctrb,t3
        if_c            movi    ctrb,#%00110_000

'
'
' Perform conversions continuously
'
                        or      dira,cmask              'output CLK
                        or      dira,smask              'output CS

main_loop               mov     command,#$10            'init command
                        mov     t1,par                  'reset sample pointer
                        mov     t2,enables              'get enables
                        mov     t3,#8                   'ready 8 channels

cloop                   shr     t2,#1           wc      'if channel disabled, skip
        if_nc           jmp     #skip

                        test    t2,#$80         wc      'channel enabled, get single/diff mode
                        muxnc   command,#$08
                        mov     stream,command

                        or      outa,smask              'CS high
                        or      dira,dmask              'make DIN/DOUT output
                        mov     bits,#20                'ready 20 bits (cs+1+diff+ch[3]+0+0+data[12])


bloop                   test    stream,#$20     wc      'update DIN/DOUT
                        muxc    outa,dmask

                        cmp     bits,#14        wz      'if command done, input DIN/DOUT
        if_z            andn    dira,dmask

                        andn    outa,cmask              'CLK low
                        mov     t4,par                  'update DACs between clock transitions
                        add     t4,#20
                        rdlong  frqa,t4
                        add     t4,#4
                        rdlong  frqb,t4
                        or      outa,cmask              'CLK high

                        test    dmask,ina       wc      'sample DIN/DOUT
                        rcl     stream,#1

                        andn    outa,smask              'CS low

                        djnz    bits,#bloop             'next data bit


                        and     stream,mask12           'trim and write sample
                        wrword  stream,t1

skip                    add     t1,#2                   'advance sample pointer
                        add     command,#$01            'advance command
                        djnz    t3,#cloop               'more channels?

                        wrlong  counter,t1              'channels done, update counter
                        add     counter,#1

                        jmp     #main_loop              'perform conversions again
'
'
' Get parameter
'
param                   rdlong  t3,t1                   'get parameter into t3
                        add     t1,#4                   'point to next parameter

param2                  mov     t2,#1                   'make pin mask in t2
                        shl     t2,t3

                        test    t3,#$80         wc      'get DAC flag into c
param2_ret
param_ret               ret
'
'
' Initialized data
'
dlsb                    long    1 << 9
mask12                  long    $FFF
'
'
' Uninitialized data
'
t1                      res     1
t2                      res     1
t3                      res     1
t4                      res     1
dmask                   res     1
cmask                   res     1
smask                   res     1
enables                 res     1
command                 res     1
stream                  res     1
bits                    res     1
counter                 res     1

' 
' 
' +------------------------------------------------------------------------------------------------------------------------------+
' �                                                   TERMS OF USE: MIT License                                                  �                                                            
' +------------------------------------------------------------------------------------------------------------------------------�
' �Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    � 
' �files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    �
' �modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software�
' �is furnished to do so, subject to the following conditions:                                                                   �
' �                                                                                                                              �
' �The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.�
' �                                                                                                                              �
' �THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          �
' �WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         �
' �COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   �
' �ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         �
' +------------------------------------------------------------------------------------------------------------------------------+
'

