\ Drone plus rhythm (Jmcc) #12
: scale [ 0 2 3 5 7 9 10 ] ;
: iseqr { s tr } tr tr 0 inf s dxrand.dr demand.ar * ;
: drone-1
    [ 24 36 ] choose 0.08 rand2.ir + midicps { f0 }
    [ f0 f0 0.2 + ] 0 lfsaw.ar f0 [ 0.05 0.04 ] * lfnoise2.kr * 0.06 *
    1000 3000 rand.ir lpf
;
: drone-2
    [ 60 72 ] choose scale choose + 0.05 rand2.ir 2 clone + midicps
    0 sinosc.ar 0.04 0.07 rand.ir * 0 1 rand.ir 0.8 > *
;
: rhy
    [ 48 60 72 84 ] choose scale choose + 0.03 rand2.ir + { m }
    [ 0 1 0 1 1 0 ] [ 1.5 3 6 ] choose 0 impulse.ar iseqr { sq }
    m midicps 0 0.4 lfpulse.ar 0.03 0.08 rand.ir * { sg }
    sq 0.004 0.2 0.7 rand.ir decay2 sg * 800 2000 exprand.ir 0.1 rlpf
;
: rev-cmb { z } z 0.5 0.5 6 combn z + ;
: drone-1-txt drone-1 4 4 8 inf overlapTexture ;
: drone-2-txt drone-2 4 6 3 inf overlapTexture ;
: rhy-txt rhy 6 6 6 inf overlapTexture ;
' rev-cmb texturePostProc
fork drone-1-txt .
fork drone-2-txt .
fork rhy-txt .
( killall )
