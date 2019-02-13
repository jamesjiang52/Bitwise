"""
0000:          END
0001 a b:      MOV a, b
0010 a #b:     MVI a, #b
0011 a b c:    ADD a, b, c
0100 a b c:    SUB a, b, c
0101 a b c:    AND a, b, c
0110 a b c:    OR a, b, c
0111 a b c:    XOR a, b, c
1000 a b:      NOT a, b
1001 a:        B a
1010 a b c:    GT a, b, c
1011 a b c:    EQ a, b, c
1100 a [b]:    LDR a, [b]
1101 a [b]:    STR a, [b]
1110 a:        PUSH a
1111 a:        POP a
"""
