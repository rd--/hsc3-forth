\ pulsing bottles (jmcc) #2

: r whitenoise.ar 4 14 rand.ir 0 0.25 lfpulse.kr *
    0 0.7 rand.ir * 400 7400 rand.ir 0.01 resonz ;
: s 0.1 0.5 rand.ir 0 6.28319 rand.ir sinosc.kr ;
: pb 0 6 0 do r s 1 pan2 + loop ;
pb 4 4 4 inf overlap-texture
