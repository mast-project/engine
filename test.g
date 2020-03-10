% comment

alphabet digits
         letters 0 1 2 3 4 5 6 7 8 9
;

alphabet test % comment
         letters a b ą sz, % comment
         modifiers <U+0301> ’,
         class V a ą,
         token NUM # digits \;
;

alphabet test2
         letters a b ã ∫,
         modifiers \'
;

transform test: test -> test2
        _́ ->* {}-́
        _’ -> @test({}-’)+'
        #a -> b
        a# -> b
        a -> a
        ą -> ã
        sz -> ∫
;

transform testi: test -> test
        a -> b
;
