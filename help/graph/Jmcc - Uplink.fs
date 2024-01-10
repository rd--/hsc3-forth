\ uplink (Jmcc) #2 texture=overlap,4,1,5,inf
: p0 0 20 Rand.ir 0 0 1 Rand.ir LfPulse.kr ;
: p1 0 4 Rand.ir 0 0 1 Rand.ir LfPulse.kr 0 8000 Rand.ir * 0 2000 Rand.ir + ;
: uplink p0 p1 * 2 clone mix 0 0.5 LfPulse.ar 0.04 * -0.8 0.8 Rand.ir 1 Pan2 ;
uplink
