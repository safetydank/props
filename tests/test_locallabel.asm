org 0

' test hub ram addressing

begin jmp #dest + (@dest+$1f)
:local mov @dest, #16
       jmp :local
next   mov @dest, #32
:local mov @dest, #48
       jmp :local

dest   long $ffff0000
target long $0000ffff

