\ rails (jmcc) #2 texture=overlap,3,2,4,inf
: rails
    100 Dust.ar 0.04 * 3000 300 8 0 XLine.kr 0.2 Resonz
    1 0 1
    20 0 do 200 0 3000 0 LinRand.ir + 1 0.2 0 1 Rand.ir + loop 20 3 * mce
    Klank -1 1 Rand.ir -1 1 Rand.ir 8 0 Line.kr 1 Pan2 ;
rails
