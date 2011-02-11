org 0

' test hub ram addressing
begin jmp #dest + (@dest+$1f)
      ' add target, #10
      jmp @dest
      ' add @dest, @begin

dest   long $ffff0000 + (64*9), 1+(2*4)
target long $00000000 + (32*12)

