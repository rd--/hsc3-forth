\ metal plate (jmcc) #4
: tap { b dt nc } b 1 0 dt sr neg * 1 0 nc PlayBuf.ar ;
: sr 48000 ;
: n 4 ;
: maxdt sr 0.03 * ;
: buf maxdt 0 do 0 loop maxdt mce asLocalBuf ;
: tap-tm 0.015 0.03 Rand.ir ;
: exc-freq 10 8000 0 0.2 MouseY.kr ;
: exc-trig 0.5 0 Impulse.ar 0.2 * ;
: exc exc-trig 0.01 0.2 Decay2 exc-freq LFNoise2.ar * ;
: flt-freq 10 5000 0 0.2 MouseX.kr ;
: nd
    buf { b } b tap-tm 1 tap flt-freq lpf 0.98 * { flt }
    flt b 0 1 0 1 1 1 0 flt exc + RecordBuf.ar 2 mrg
;
: mp 0 n 0 do nd + loop ;
mp
