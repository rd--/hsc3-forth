\ tgb (rd)
: ln { l0 l1 } l0 1 -99 -99 l1 1 1 0 8 mce ;
: ln' { l0 l1 l2 } l0 2 -99 -99 l1 0.5 1 0 l2 0.5 1 0 12 mce ;
: mkls { env dur } 1 1 0 dur remove-synth env EnvGen.kr ;
: wrp { in l r } r l - 2 / { m } in m l m + MulAdd ;
: pm/n { l r d } WhiteNoise.ar l d mkls r d mkls wrp ;
: pm/t { l r d t } l d mkls r d mkls t TRand.ar ;
: tgb { b d }
    400 900 ln 600 1200 ln d pm/n { gps }
    gps 0 Impulse.ar { t }
    0.005 0.015 0.005 ln' 0.009 0.020 0.009 ln' d t pm/t { dur }
    06 12 06 ln' 12 12 12 ln' d t pm/t { rate }
    0 0.95 ln 0 1 ln d t pm/t { cpos }
    0.25 0.55 0.15 ln' 0.5 0.75 0.25 ln' d t pm/t { amp }
    -1 -0.5 0.5 ln' -0.5 0.5 1 ln' d t pm/t { pan }
    t b rate cpos b BufDur.kr * dur pan amp 1 2 TGrains.ar
;
10 s" /home/rohan/data/audio/metal.wav" 0 0 b_allocRead
10 30 tgb
play
