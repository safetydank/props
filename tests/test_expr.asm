org 0

begin mov dest, #$ff
      mov par + (3+4),  #$ff >> (1 + 2) + (3+4)
      jmp #begin+(3+9)

' test hub ram addressing
      jmp dest
      jmp @dest
      add @dest, @begin

dest  long $ffff0000 + (2*5), $1c1c0000+(4*11), $1d1d+(17)
hello byte "Hello world", "How", "are", "you", 9*2
end   long $00000000

