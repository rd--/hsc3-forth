\ alien froggies (jmcc) #1
: alien-froggie
    11 -0.2 0.2 0 LinRand.ir Exp * 1 30 Fold { r }
    r 200 3000 ExpRand.ir 0 9 Rand.ir r * r + Formant.ar 0.05 *
;
: alien-froggies alien-froggie 0.25 0.5 5 inf overlapTexture ;
alien-froggies play
