\ Bowed string (Jmcc)
: root 5 ;
: scale [ 0 2 4 5 7 9 11 ] ;
: oct [ 24 36 48 60 72 84 ] ;
: rand-f scale choose root + oct choose + midicps ;
: param { f r } [ 12 0 do f i * f + r i ** 1 3 Rand.ir loop ] ;
: bowed-string
    BrownNoise.ar 2 clone 0.007 * 0 0.125 0.5 ExpRand.ir LfNoise1.kr 0.6 * 0.4 + Max *
    1 0 1 rand-f 0.7 0.9 Rand.ir param Klank 0.1 * SoftClip ;
bowed-string 5 2 12 inf overlapTexture
