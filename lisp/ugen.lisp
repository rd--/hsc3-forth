(define local-buf
  (lambda (rt numChannels numFrames)
    (mk-ugen (list "LocalBuf" rt (list numChannels numFrames) #f 1 #f (incr-uid 1)))))
(define mul-add
  (lambda (in mul add)
    (mk-ugen (list "MulAdd" #f (list in mul add) #f 1 #f #f))))
(define set-buf
  (lambda (rt buf numValues offset values)
    (mk-ugen (list "SetBuf" rt (list buf numValues offset) values 1 #f #f))))
(define a2k
  (lambda (rt in)
    (mk-ugen (list "A2K" rt (list in) #f 1 #f #f))))
(define apf
  (lambda (rt in freq radius)
    (mk-ugen (list "APF" rt (list in freq radius) #f 1 #f #f))))
(define allpass-c
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassC" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define allpass-l
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassL" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define allpass-n
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "AllpassN" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define amp-comp
  (lambda (rt freq root exp)
    (mk-ugen (list "AmpComp" rt (list freq root exp) #f 1 #f #f))))
(define amp-comp-a
  (lambda (rt freq root minAmp rootAmp)
    (mk-ugen (list "AmpCompA" rt (list freq root minAmp rootAmp) #f 1 #f #f))))
(define amplitude
  (lambda (rt in attackTime releaseTime)
    (mk-ugen (list "Amplitude" rt (list in attackTime releaseTime) #f 1 #f #f))))
(define audio-control
  (lambda (rt values)
    (mk-ugen (list "AudioControl" rt (list values) #f 1 #f #f))))
(define b-all-pass
  (lambda (in freq rq)
    (mk-ugen (list "BAllPass" #f (list in freq rq) #f 1 #f #f))))
(define b-band-pass
  (lambda (in freq bw)
    (mk-ugen (list "BBandPass" #f (list in freq bw) #f 1 #f #f))))
(define b-band-stop
  (lambda (in freq bw)
    (mk-ugen (list "BBandStop" #f (list in freq bw) #f 1 #f #f))))
(define b-hi-pass
  (lambda (in freq rq)
    (mk-ugen (list "BHiPass" #f (list in freq rq) #f 1 #f #f))))
(define b-hi-shelf
  (lambda (in freq rs db)
    (mk-ugen (list "BHiShelf" #f (list in freq rs db) #f 1 #f #f))))
(define b-low-pass
  (lambda (in freq rq)
    (mk-ugen (list "BLowPass" #f (list in freq rq) #f 1 #f #f))))
(define b-low-shelf
  (lambda (in freq rs db)
    (mk-ugen (list "BLowShelf" #f (list in freq rs db) #f 1 #f #f))))
(define bpf
  (lambda (in freq rq)
    (mk-ugen (list "BPF" #f (list in freq rq) #f 1 #f #f))))
(define bpz2
  (lambda (in)
    (mk-ugen (list "BPZ2" #f (list in) #f 1 #f #f))))
(define b-peak-eq
  (lambda (in freq rq db)
    (mk-ugen (list "BPeakEQ" #f (list in freq rq db) #f 1 #f #f))))
(define brf
  (lambda (in freq rq)
    (mk-ugen (list "BRF" #f (list in freq rq) #f 1 #f #f))))
(define brz2
  (lambda (in)
    (mk-ugen (list "BRZ2" #f (list in) #f 1 #f #f))))
(define balance2
  (lambda (rt left right pos level)
    (mk-ugen (list "Balance2" rt (list left right pos level) #f 2 #f #f))))
(define ball
  (lambda (rt in g damp friction)
    (mk-ugen (list "Ball" rt (list in g damp friction) #f 1 #f #f))))
(define beat-track
  (lambda (rt chain lock)
    (mk-ugen (list "BeatTrack" rt (list chain lock) #f 1 #f #f))))
(define beat-track2
  (lambda (rt busindex numfeatures windowsize phaseaccuracy lock weightingscheme)
    (mk-ugen (list "BeatTrack2" rt (list busindex numfeatures windowsize phaseaccuracy lock weightingscheme) #f 6 #f #f))))
(define bi-pan-b2
  (lambda (rt inA inB azimuth gain)
    (mk-ugen (list "BiPanB2" rt (list inA inB azimuth gain) #f 3 #f #f))))
(define binary-op-ugen
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 #f #f))))
(define blip
  (lambda (rt freq numharm)
    (mk-ugen (list "Blip" rt (list freq numharm) #f 1 #f #f))))
(define block-size
  (lambda (rt)
    (mk-ugen (list "BlockSize" rt nil #f 1 #f #f))))
(define brown-noise
  (lambda (rt)
    (mk-ugen (list "BrownNoise" rt nil #f 1 #f (incr-uid 1)))))
(define buf-allpass-c
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufAllpassC" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define buf-allpass-l
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufAllpassL" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define buf-allpass-n
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufAllpassN" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define buf-channels
  (lambda (rt bufnum)
    (mk-ugen (list "BufChannels" rt (list bufnum) #f 1 #f #f))))
(define buf-comb-c
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufCombC" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define buf-comb-l
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufCombL" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define buf-comb-n
  (lambda (rt buf in delaytime decaytime)
    (mk-ugen (list "BufCombN" rt (list buf in delaytime decaytime) #f 1 #f #f))))
(define buf-delay-c
  (lambda (rt buf in delaytime)
    (mk-ugen (list "BufDelayC" rt (list buf in delaytime) #f 1 #f #f))))
(define buf-delay-l
  (lambda (rt buf in delaytime)
    (mk-ugen (list "BufDelayL" rt (list buf in delaytime) #f 1 #f #f))))
(define buf-delay-n
  (lambda (rt buf in delaytime)
    (mk-ugen (list "BufDelayN" rt (list buf in delaytime) #f 1 #f #f))))
(define buf-dur
  (lambda (rt bufnum)
    (mk-ugen (list "BufDur" rt (list bufnum) #f 1 #f #f))))
(define buf-frames
  (lambda (rt bufnum)
    (mk-ugen (list "BufFrames" rt (list bufnum) #f 1 #f #f))))
(define buf-info-ugen-base
  (lambda (rt bufnum)
    (mk-ugen (list "BufInfoUGenBase" rt (list bufnum) #f 1 #f #f))))
(define buf-rate-scale
  (lambda (rt bufnum)
    (mk-ugen (list "BufRateScale" rt (list bufnum) #f 1 #f #f))))
(define buf-rd
  (lambda (nc rt bufnum phase loop interpolation)
    (mk-ugen (list "BufRd" rt (list bufnum phase loop interpolation) #f nc #f #f))))
(define buf-sample-rate
  (lambda (rt bufnum)
    (mk-ugen (list "BufSampleRate" rt (list bufnum) #f 1 #f #f))))
(define buf-samples
  (lambda (rt bufnum)
    (mk-ugen (list "BufSamples" rt (list bufnum) #f 1 #f #f))))
(define buf-wr
  (lambda (rt inputArray bufnum phase loop)
    (mk-ugen (list "BufWr" rt (list inputArray bufnum phase) loop 1 #f #f))))
(define c-osc
  (lambda (rt bufnum freq beats)
    (mk-ugen (list "COsc" rt (list bufnum freq beats) #f 1 #f #f))))
(define changed
  (lambda (rt input threshold)
    (mk-ugen (list "Changed" rt (list input threshold) #f 1 #f #f))))
(define check-bad-values
  (lambda (rt in id post)
    (mk-ugen (list "CheckBadValues" rt (list in id post) #f 1 #f #f))))
(define clip
  (lambda (in lo hi)
    (mk-ugen (list "Clip" #f (list in lo hi) #f 1 #f #f))))
(define clip-noise
  (lambda (rt)
    (mk-ugen (list "ClipNoise" rt nil #f 1 #f (incr-uid 1)))))
(define coin-gate
  (lambda (rt prob in)
    (mk-ugen (list "CoinGate" rt (list prob in) #f 1 #f (incr-uid 1)))))
(define comb-c
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombC" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define comb-l
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombL" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define comb-n
  (lambda (in maxdelaytime delaytime decaytime)
    (mk-ugen (list "CombN" #f (list in maxdelaytime delaytime decaytime) #f 1 #f #f))))
(define compander
  (lambda (in control thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "Compander" #f (list in control thresh slopeBelow slopeAbove clampTime relaxTime) #f 1 #f #f))))
(define compander-d
  (lambda (rt in thresh slopeBelow slopeAbove clampTime relaxTime)
    (mk-ugen (list "CompanderD" rt (list in thresh slopeBelow slopeAbove clampTime relaxTime) #f 1 #f #f))))
(define control
  (lambda (rt values)
    (mk-ugen (list "Control" rt (list values) #f 1 #f #f))))
(define control-dur
  (lambda (rt)
    (mk-ugen (list "ControlDur" rt nil #f 1 #f #f))))
(define control-rate
  (lambda (rt)
    (mk-ugen (list "ControlRate" rt nil #f 1 #f #f))))
(define convolution
  (lambda (rt in kernel framesize)
    (mk-ugen (list "Convolution" rt (list in kernel framesize) #f 1 #f #f))))
(define convolution2
  (lambda (rt in kernel trigger framesize)
    (mk-ugen (list "Convolution2" rt (list in kernel trigger framesize) #f 1 #f #f))))
(define convolution2l
  (lambda (rt in kernel trigger framesize crossfade)
    (mk-ugen (list "Convolution2L" rt (list in kernel trigger framesize crossfade) #f 1 #f #f))))
(define convolution3
  (lambda (rt in kernel trigger framesize)
    (mk-ugen (list "Convolution3" rt (list in kernel trigger framesize) #f 1 #f #f))))
(define crackle
  (lambda (rt chaosParam)
    (mk-ugen (list "Crackle" rt (list chaosParam) #f 1 #f #f))))
(define cusp-l
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspL" rt (list freq a b xi) #f 1 #f #f))))
(define cusp-n
  (lambda (rt freq a b xi)
    (mk-ugen (list "CuspN" rt (list freq a b xi) #f 1 #f #f))))
(define dc
  (lambda (rt in)
    (mk-ugen (list "DC" rt (list in) #f 1 #f #f))))
(define dbrown
  (lambda (rt lo hi step length)
    (mk-ugen (list "Dbrown" rt (list lo hi step length) #f 1 #f #f))))
(define dbufrd
  (lambda (rt bufnum phase loop)
    (mk-ugen (list "Dbufrd" rt (list bufnum phase loop) #f 1 #f #f))))
(define dbufwr
  (lambda (rt input bufnum phase loop)
    (mk-ugen (list "Dbufwr" rt (list input bufnum phase loop) #f 1 #f #f))))
(define decay
  (lambda (in decayTime)
    (mk-ugen (list "Decay" #f (list in decayTime) #f 1 #f #f))))
(define decay2
  (lambda (in attackTime decayTime)
    (mk-ugen (list "Decay2" #f (list in attackTime decayTime) #f 1 #f #f))))
(define decode-b2
  (lambda (nc rt w x y orientation)
    (mk-ugen (list "DecodeB2" rt (list w x y orientation) #f nc #f #f))))
(define degree-to-key
  (lambda (bufnum in octave)
    (mk-ugen (list "DegreeToKey" #f (list bufnum in octave) #f 1 #f #f))))
(define del-tap-rd
  (lambda (rt buffer phase delTime interp)
    (mk-ugen (list "DelTapRd" rt (list buffer phase delTime interp) #f 1 #f #f))))
(define del-tap-wr
  (lambda (rt buffer in)
    (mk-ugen (list "DelTapWr" rt (list buffer in) #f 1 #f #f))))
(define delay1
  (lambda (in)
    (mk-ugen (list "Delay1" #f (list in) #f 1 #f #f))))
(define delay2
  (lambda (in)
    (mk-ugen (list "Delay2" #f (list in) #f 1 #f #f))))
(define delay-c
  (lambda (in maxdelaytime delaytime)
    (mk-ugen (list "DelayC" #f (list in maxdelaytime delaytime) #f 1 #f #f))))
(define delay-l
  (lambda (in maxdelaytime delaytime)
    (mk-ugen (list "DelayL" #f (list in maxdelaytime delaytime) #f 1 #f #f))))
(define delay-n
  (lambda (in maxdelaytime delaytime)
    (mk-ugen (list "DelayN" #f (list in maxdelaytime delaytime) #f 1 #f #f))))
(define demand
  (lambda (trig reset demandUGens)
    (mk-ugen (list "Demand" #f (list trig reset demandUGens) #f 1 #f #f))))
(define demand-env-gen
  (lambda (rt level dur shape curve gate reset levelScale levelBias timeScale doneAction)
    (mk-ugen (list "DemandEnvGen" rt (list level dur shape curve gate reset levelScale levelBias timeScale doneAction) #f 1 #f #f))))
(define detect-index
  (lambda (rt bufnum in)
    (mk-ugen (list "DetectIndex" rt (list bufnum in) #f 1 #f #f))))
(define detect-silence
  (lambda (rt in amp time doneAction)
    (mk-ugen (list "DetectSilence" rt (list in amp time doneAction) #f 1 #f #f))))
(define dgeom
  (lambda (rt start grow length)
    (mk-ugen (list "Dgeom" rt (list start grow length) #f 1 #f #f))))
(define dibrown
  (lambda (rt lo hi step length)
    (mk-ugen (list "Dibrown" rt (list lo hi step length) #f 1 #f #f))))
(define disk-in
  (lambda (nc rt bufnum loop)
    (mk-ugen (list "DiskIn" rt (list bufnum loop) #f nc #f #f))))
(define disk-out
  (lambda (rt bufnum channelsArray)
    (mk-ugen (list "DiskOut" rt (list bufnum) channelsArray 1 #f #f))))
(define diwhite
  (lambda (rt lo hi length)
    (mk-ugen (list "Diwhite" rt (list lo hi length) #f 1 #f #f))))
(define donce
  (lambda (rt in)
    (mk-ugen (list "Donce" rt (list in) #f 1 #f #f))))
(define done
  (lambda (rt src)
    (mk-ugen (list "Done" rt (list src) #f 1 #f #f))))
(define dpoll
  (lambda (rt in label run trigid)
    (mk-ugen (list "Dpoll" rt (list in label run trigid) #f 1 #f #f))))
(define drand
  (lambda (list_ repeats)
    (mk-ugen (list "Drand" dr (list list_) repeats 1 #f (incr-uid 1)))))
(define dreset
  (lambda (rt in reset)
    (mk-ugen (list "Dreset" rt (list in reset) #f 1 #f #f))))
(define dseq
  (lambda (list_ repeats)
    (mk-ugen (list "Dseq" dr (list list_) repeats 1 #f (incr-uid 1)))))
(define dser
  (lambda (rt list_ repeats)
    (mk-ugen (list "Dser" rt (list list_) repeats 1 #f #f))))
(define dseries
  (lambda (rt start step length)
    (mk-ugen (list "Dseries" rt (list start step length) #f 1 #f #f))))
(define dshuf
  (lambda (rt list_ repeats)
    (mk-ugen (list "Dshuf" rt (list list_ repeats) #f 1 #f (incr-uid 1)))))
(define dstutter
  (lambda (rt n in)
    (mk-ugen (list "Dstutter" rt (list n in) #f 1 #f #f))))
(define dswitch
  (lambda (rt list_ index)
    (mk-ugen (list "Dswitch" rt (list list_) index 1 #f #f))))
(define dswitch1
  (lambda (rt list_ index)
    (mk-ugen (list "Dswitch1" rt (list list_) index 1 #f #f))))
(define dunique
  (lambda (rt source maxBufferSize protected)
    (mk-ugen (list "Dunique" rt (list source maxBufferSize protected) #f 1 #f #f))))
(define dust
  (lambda (rt density)
    (mk-ugen (list "Dust" rt (list density) #f 1 #f (incr-uid 1)))))
(define dust2
  (lambda (rt density)
    (mk-ugen (list "Dust2" rt (list density) #f 1 #f (incr-uid 1)))))
(define dust-r
  (lambda (rt iot_min iot_max)
    (mk-ugen (list "DustR" rt (list iot_min iot_max) #f 1 #f #f))))
(define duty
  (lambda (rt dur reset level doneAction)
    (mk-ugen (list "Duty" rt (list dur reset level doneAction) #f 1 #f #f))))
(define dwhite
  (lambda (rt lo hi length)
    (mk-ugen (list "Dwhite" rt (list lo hi length) #f 1 #f #f))))
(define dwrand
  (lambda (rt list_ weights repeats)
    (mk-ugen (list "Dwrand" rt (list list_ weights repeats) #f 1 #f (incr-uid 1)))))
(define dxrand
  (lambda (rt list_ repeats)
    (mk-ugen (list "Dxrand" rt (list list_) repeats 1 #f #f))))
(define dyn-klang
  (lambda (rt specificationsArrayRef freqscale freqoffset)
    (mk-ugen (list "DynKlang" rt (list specificationsArrayRef freqscale freqoffset) #f 1 #f #f))))
(define dyn-klank
  (lambda (rt specificationsArrayRef input freqscale freqoffset decayscale)
    (mk-ugen (list "DynKlank" rt (list specificationsArrayRef input freqscale freqoffset decayscale) #f 1 #f #f))))
(define env-gen
  (lambda (rt envelope gate levelScale levelBias timeScale doneAction)
    (mk-ugen (list "EnvGen" rt (list envelope gate levelScale levelBias timeScale) doneAction 1 #f #f))))
(define exp-rand
  (lambda (lo hi)
    (mk-ugen (list "ExpRand" ir (list lo hi) #f 1 #f (incr-uid 1)))))
(define fb-sine-c
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineC" rt (list freq im fb a c xi yi) #f 1 #f #f))))
(define fb-sine-l
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineL" rt (list freq im fb a c xi yi) #f 1 #f #f))))
(define fb-sine-n
  (lambda (rt freq im fb a c xi yi)
    (mk-ugen (list "FBSineN" rt (list freq im fb a c xi yi) #f 1 #f #f))))
(define fft
  (lambda (rt buffer in hop wintype active winsize)
    (mk-ugen (list "FFT" rt (list buffer in hop wintype active winsize) #f 1 #f #f))))
(define fos
  (lambda (in a0 a1 b1)
    (mk-ugen (list "FOS" #f (list in a0 a1 b1) #f 1 #f #f))))
(define f-sin-osc
  (lambda (rt freq iphase)
    (mk-ugen (list "FSinOsc" rt (list freq iphase) #f 1 #f #f))))
(define fold
  (lambda (in lo hi)
    (mk-ugen (list "Fold" #f (list in lo hi) #f 1 #f #f))))
(define formant
  (lambda (rt fundfreq formfreq bwfreq)
    (mk-ugen (list "Formant" rt (list fundfreq formfreq bwfreq) #f 1 #f #f))))
(define formlet
  (lambda (in freq attacktime decaytime)
    (mk-ugen (list "Formlet" #f (list in freq attacktime decaytime) #f 1 #f #f))))
(define free
  (lambda (rt trig id)
    (mk-ugen (list "Free" rt (list trig id) #f 1 #f #f))))
(define free-self
  (lambda (rt in)
    (mk-ugen (list "FreeSelf" rt (list in) #f 1 #f #f))))
(define free-self-when-done
  (lambda (rt src)
    (mk-ugen (list "FreeSelfWhenDone" rt (list src) #f 1 #f #f))))
(define free-verb
  (lambda (in mix room damp)
    (mk-ugen (list "FreeVerb" #f (list in mix room damp) #f 1 #f #f))))
(define free-verb2
  (lambda (in in2 mix room damp)
    (mk-ugen (list "FreeVerb2" #f (list in in2 mix room damp) #f 2 #f #f))))
(define freq-shift
  (lambda (rt in freq phase)
    (mk-ugen (list "FreqShift" rt (list in freq phase) #f 1 #f #f))))
(define g-verb
  (lambda (in roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize)
    (mk-ugen (list "GVerb" #f (list in roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize) #f 2 #f #f))))
(define gate
  (lambda (in trig)
    (mk-ugen (list "Gate" #f (list in trig) #f 1 #f #f))))
(define gbman-l
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanL" rt (list freq xi yi) #f 1 #f #f))))
(define gbman-n
  (lambda (rt freq xi yi)
    (mk-ugen (list "GbmanN" rt (list freq xi yi) #f 1 #f #f))))
(define gendy1
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy1" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum) #f 1 #f (incr-uid 1)))))
(define gendy2
  (lambda (rt ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c)
    (mk-ugen (list "Gendy2" rt (list ampdist durdist adparam ddparam minfreq maxfreq ampscale durscale initCPs knum a c) #f 1 #f (incr-uid 1)))))
(define gendy3
  (lambda (rt ampdist durdist adparam ddparam freq ampscale durscale initCPs knum)
    (mk-ugen (list "Gendy3" rt (list ampdist durdist adparam ddparam freq ampscale durscale initCPs knum) #f 1 #f (incr-uid 1)))))
(define grain-buf
  (lambda (nc rt trigger dur sndbuf rate pos interp pan envbufnum maxGrains)
    (mk-ugen (list "GrainBuf" rt (list trigger dur sndbuf rate pos interp pan envbufnum maxGrains) #f nc #f #f))))
(define grain-fm
  (lambda (nc rt trigger dur carfreq modfreq index pan envbufnum maxGrains)
    (mk-ugen (list "GrainFM" rt (list trigger dur carfreq modfreq index pan envbufnum maxGrains) #f nc #f #f))))
(define grain-in
  (lambda (nc rt trigger dur in pan envbufnum maxGrains)
    (mk-ugen (list "GrainIn" rt (list trigger dur in pan envbufnum maxGrains) #f nc #f #f))))
(define grain-sin
  (lambda (nc rt trigger dur freq pan envbufnum maxGrains)
    (mk-ugen (list "GrainSin" rt (list trigger dur freq pan envbufnum maxGrains) #f nc #f #f))))
(define gray-noise
  (lambda (rt)
    (mk-ugen (list "GrayNoise" rt nil #f 1 #f (incr-uid 1)))))
(define hpf
  (lambda (in freq)
    (mk-ugen (list "HPF" #f (list in freq) #f 1 #f #f))))
(define hpz1
  (lambda (in)
    (mk-ugen (list "HPZ1" #f (list in) #f 1 #f #f))))
(define hpz2
  (lambda (in)
    (mk-ugen (list "HPZ2" #f (list in) #f 1 #f #f))))
(define hasher
  (lambda (in)
    (mk-ugen (list "Hasher" #f (list in) #f 1 #f #f))))
(define henon-c
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonC" rt (list freq a b x0 x1) #f 1 #f #f))))
(define henon-l
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonL" rt (list freq a b x0 x1) #f 1 #f #f))))
(define henon-n
  (lambda (rt freq a b x0 x1)
    (mk-ugen (list "HenonN" rt (list freq a b x0 x1) #f 1 #f #f))))
(define hilbert
  (lambda (in)
    (mk-ugen (list "Hilbert" #f (list in) #f 2 #f #f))))
(define hilbert-fir
  (lambda (rt in buffer)
    (mk-ugen (list "HilbertFIR" rt (list in buffer) #f 2 #f #f))))
(define i-env-gen
  (lambda (rt envelope index)
    (mk-ugen (list "IEnvGen" rt (list envelope index) #f 1 #f #f))))
(define ifft
  (lambda (rt buffer wintype winsize)
    (mk-ugen (list "IFFT" rt (list buffer wintype winsize) #f 1 #f #f))))
(define i-rand
  (lambda (lo hi)
    (mk-ugen (list "IRand" ir (list lo hi) #f 1 #f (incr-uid 1)))))
(define impulse
  (lambda (rt freq phase)
    (mk-ugen (list "Impulse" rt (list freq phase) #f 1 #f #f))))
(define in
  (lambda (nc rt bus)
    (mk-ugen (list "In" rt (list bus) #f nc #f #f))))
(define in-bus
  (lambda (rt bus offset clip)
    (mk-ugen (list "InBus" rt (list bus offset clip) #f 1 #f #f))))
(define in-feedback
  (lambda (nc rt bus)
    (mk-ugen (list "InFeedback" rt (list bus) #f nc #f #f))))
(define in-range
  (lambda (in lo hi)
    (mk-ugen (list "InRange" #f (list in lo hi) #f 1 #f #f))))
(define in-rect
  (lambda (rt x y rect)
    (mk-ugen (list "InRect" rt (list x y rect) #f 1 #f #f))))
(define in-trig
  (lambda (nc rt bus)
    (mk-ugen (list "InTrig" rt (list bus) #f nc #f #f))))
(define index
  (lambda (rt bufnum in)
    (mk-ugen (list "Index" rt (list bufnum in) #f 1 #f #f))))
(define index-in-between
  (lambda (rt bufnum in)
    (mk-ugen (list "IndexInBetween" rt (list bufnum in) #f 1 #f #f))))
(define index-l
  (lambda (rt bufnum in)
    (mk-ugen (list "IndexL" rt (list bufnum in) #f 1 #f #f))))
(define info-ugen-base
  (lambda (rt)
    (mk-ugen (list "InfoUGenBase" rt nil #f 1 #f #f))))
(define integrator
  (lambda (in coef)
    (mk-ugen (list "Integrator" #f (list in coef) #f 1 #f #f))))
(define k2a
  (lambda (rt in)
    (mk-ugen (list "K2A" rt (list in) #f 1 #f #f))))
(define key-state
  (lambda (rt keycode minval maxval lag)
    (mk-ugen (list "KeyState" rt (list keycode minval maxval lag) #f 1 #f #f))))
(define key-track
  (lambda (rt chain keydecay chromaleak)
    (mk-ugen (list "KeyTrack" rt (list chain keydecay chromaleak) #f 1 #f #f))))
(define klang
  (lambda (rt specificationsArrayRef freqscale freqoffset)
    (mk-ugen (list "Klang" rt (list specificationsArrayRef freqscale) freqoffset 1 #f #f))))
(define klank
  (lambda (specificationsArrayRef input freqscale freqoffset decayscale)
    (mk-ugen (list "Klank" #f (list specificationsArrayRef input freqscale freqoffset) decayscale 1 #f #f))))
(define lf-clip-noise
  (lambda (rt freq)
    (mk-ugen (list "LFClipNoise" rt (list freq) #f 1 #f (incr-uid 1)))))
(define lf-cub
  (lambda (rt freq iphase)
    (mk-ugen (list "LFCub" rt (list freq iphase) #f 1 #f #f))))
(define lfd-clip-noise
  (lambda (rt freq)
    (mk-ugen (list "LFDClipNoise" rt (list freq) #f 1 #f (incr-uid 1)))))
(define lfd-noise0
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise0" rt (list freq) #f 1 #f (incr-uid 1)))))
(define lfd-noise1
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise1" rt (list freq) #f 1 #f (incr-uid 1)))))
(define lfd-noise3
  (lambda (rt freq)
    (mk-ugen (list "LFDNoise3" rt (list freq) #f 1 #f (incr-uid 1)))))
(define lfgauss
  (lambda (rt duration width iphase loop doneAction)
    (mk-ugen (list "LFGauss" rt (list duration width iphase loop doneAction) #f 1 #f #f))))
(define lf-noise0
  (lambda (rt freq)
    (mk-ugen (list "LFNoise0" rt (list freq) #f 1 #f (incr-uid 1)))))
(define lf-noise1
  (lambda (rt freq)
    (mk-ugen (list "LFNoise1" rt (list freq) #f 1 #f (incr-uid 1)))))
(define lf-noise2
  (lambda (rt freq)
    (mk-ugen (list "LFNoise2" rt (list freq) #f 1 #f (incr-uid 1)))))
(define lf-par
  (lambda (rt freq iphase)
    (mk-ugen (list "LFPar" rt (list freq iphase) #f 1 #f #f))))
(define lf-pulse
  (lambda (rt freq iphase width)
    (mk-ugen (list "LFPulse" rt (list freq iphase width) #f 1 #f #f))))
(define lf-saw
  (lambda (rt freq iphase)
    (mk-ugen (list "LFSaw" rt (list freq iphase) #f 1 #f #f))))
(define lf-tri
  (lambda (rt freq iphase)
    (mk-ugen (list "LFTri" rt (list freq iphase) #f 1 #f #f))))
(define lpf
  (lambda (in freq)
    (mk-ugen (list "LPF" #f (list in freq) #f 1 #f #f))))
(define lpz1
  (lambda (in)
    (mk-ugen (list "LPZ1" #f (list in) #f 1 #f #f))))
(define lpz2
  (lambda (in)
    (mk-ugen (list "LPZ2" #f (list in) #f 1 #f #f))))
(define lag
  (lambda (in lagTime)
    (mk-ugen (list "Lag" #f (list in lagTime) #f 1 #f #f))))
(define lag2
  (lambda (in lagTime)
    (mk-ugen (list "Lag2" #f (list in lagTime) #f 1 #f #f))))
(define lag2ud
  (lambda (in lagTimeU lagTimeD)
    (mk-ugen (list "Lag2UD" #f (list in lagTimeU lagTimeD) #f 1 #f #f))))
(define lag3
  (lambda (in lagTime)
    (mk-ugen (list "Lag3" #f (list in lagTime) #f 1 #f #f))))
(define lag3ud
  (lambda (in lagTimeU lagTimeD)
    (mk-ugen (list "Lag3UD" #f (list in lagTimeU lagTimeD) #f 1 #f #f))))
(define lag-control
  (lambda (rt values lags)
    (mk-ugen (list "LagControl" rt (list values lags) #f 1 #f #f))))
(define lag-in
  (lambda (nc rt bus lag)
    (mk-ugen (list "LagIn" rt (list bus lag) #f nc #f #f))))
(define lag-ud
  (lambda (in lagTimeU lagTimeD)
    (mk-ugen (list "LagUD" #f (list in lagTimeU lagTimeD) #f 1 #f #f))))
(define last-value
  (lambda (in diff)
    (mk-ugen (list "LastValue" #f (list in diff) #f 1 #f #f))))
(define latch
  (lambda (in trig)
    (mk-ugen (list "Latch" #f (list in trig) #f 1 #f #f))))
(define latoocarfian-c
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianC" rt (list freq a b c d xi yi) #f 1 #f #f))))
(define latoocarfian-l
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianL" rt (list freq a b c d xi yi) #f 1 #f #f))))
(define latoocarfian-n
  (lambda (rt freq a b c d xi yi)
    (mk-ugen (list "LatoocarfianN" rt (list freq a b c d xi yi) #f 1 #f #f))))
(define leak-dc
  (lambda (in coef)
    (mk-ugen (list "LeakDC" #f (list in coef) #f 1 #f #f))))
(define least-change
  (lambda (rt a b)
    (mk-ugen (list "LeastChange" rt (list a b) #f 1 #f #f))))
(define limiter
  (lambda (in level dur)
    (mk-ugen (list "Limiter" #f (list in level dur) #f 1 #f #f))))
(define lin-cong-c
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongC" rt (list freq a c m xi) #f 1 #f #f))))
(define lin-cong-l
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongL" rt (list freq a c m xi) #f 1 #f #f))))
(define lin-cong-n
  (lambda (rt freq a c m xi)
    (mk-ugen (list "LinCongN" rt (list freq a c m xi) #f 1 #f #f))))
(define lin-exp
  (lambda (in srclo srchi dstlo dsthi)
    (mk-ugen (list "LinExp" #f (list in srclo srchi dstlo dsthi) #f 1 #f #f))))
(define lin-pan2
  (lambda (in pos level)
    (mk-ugen (list "LinPan2" #f (list in pos level) #f 2 #f #f))))
(define lin-rand
  (lambda (lo hi minmax)
    (mk-ugen (list "LinRand" ir (list lo hi minmax) #f 1 #f (incr-uid 1)))))
(define lin-x-fade2
  (lambda (rt inA inB pan level)
    (mk-ugen (list "LinXFade2" rt (list inA inB pan level) #f 1 #f #f))))
(define line
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "Line" rt (list start end dur doneAction) #f 1 #f #f))))
(define linen
  (lambda (rt gate attackTime susLevel releaseTime doneAction)
    (mk-ugen (list "Linen" rt (list gate attackTime susLevel releaseTime doneAction) #f 1 #f #f))))
(define list-dugen
  (lambda (rt list_ repeats)
    (mk-ugen (list "ListDUGen" rt (list list_ repeats) #f 1 #f #f))))
(define local-buf
  (lambda (rt numChannels numFrames)
    (mk-ugen (list "LocalBuf" rt (list numChannels numFrames) #f 1 #f (incr-uid 1)))))
(define local-in
  (lambda (nc rt default)
    (mk-ugen (list "LocalIn" rt (list default) #f nc #f #f))))
(define local-out
  (lambda (channelsArray)
    (mk-ugen (list "LocalOut" #f nil channelsArray 1 #f #f))))
(define logistic
  (lambda (rt chaosParam freq init)
    (mk-ugen (list "Logistic" rt (list chaosParam freq init) #f 1 #f #f))))
(define lorenz-l
  (lambda (rt freq s r b h xi yi zi)
    (mk-ugen (list "LorenzL" rt (list freq s r b h xi yi zi) #f 1 #f #f))))
(define loudness
  (lambda (rt chain smask tmask)
    (mk-ugen (list "Loudness" rt (list chain smask tmask) #f 1 #f #f))))
(define mfcc
  (lambda (rt chain numcoeff)
    (mk-ugen (list "MFCC" rt (list chain numcoeff) #f 13 #f #f))))
(define mantissa-mask
  (lambda (in bits)
    (mk-ugen (list "MantissaMask" #f (list in bits) #f 1 #f #f))))
(define max-local-bufs
  (lambda (rt)
    (mk-ugen (list "MaxLocalBufs" rt nil #f 1 #f #f))))
(define median
  (lambda (length in)
    (mk-ugen (list "Median" #f (list length in) #f 1 #f #f))))
(define mid-eq
  (lambda (in freq rq db)
    (mk-ugen (list "MidEQ" #f (list in freq rq db) #f 1 #f #f))))
(define mod-dif
  (lambda (rt x y mod)
    (mk-ugen (list "ModDif" rt (list x y mod) #f 1 #f #f))))
(define moog-ff
  (lambda (in freq gain reset)
    (mk-ugen (list "MoogFF" #f (list in freq gain reset) #f 1 #f #f))))
(define most-change
  (lambda (a b)
    (mk-ugen (list "MostChange" #f (list a b) #f 1 #f #f))))
(define mouse-button
  (lambda (rt minval maxval lag)
    (mk-ugen (list "MouseButton" rt (list minval maxval lag) #f 1 #f #f))))
(define mouse-x
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseX" rt (list minval maxval warp lag) #f 1 #f #f))))
(define mouse-y
  (lambda (rt minval maxval warp lag)
    (mk-ugen (list "MouseY" rt (list minval maxval warp lag) #f 1 #f #f))))
(define n-rand
  (lambda (lo hi n)
    (mk-ugen (list "NRand" ir (list lo hi n) #f 1 #f (incr-uid 1)))))
(define normalizer
  (lambda (in level dur)
    (mk-ugen (list "Normalizer" #f (list in level dur) #f 1 #f #f))))
(define num-audio-buses
  (lambda (rt)
    (mk-ugen (list "NumAudioBuses" rt nil #f 1 #f #f))))
(define num-buffers
  (lambda (rt)
    (mk-ugen (list "NumBuffers" rt nil #f 1 #f #f))))
(define num-control-buses
  (lambda (rt)
    (mk-ugen (list "NumControlBuses" rt nil #f 1 #f #f))))
(define num-input-buses
  (lambda (rt)
    (mk-ugen (list "NumInputBuses" rt nil #f 1 #f #f))))
(define num-output-buses
  (lambda (rt)
    (mk-ugen (list "NumOutputBuses" rt nil #f 1 #f #f))))
(define num-running-synths
  (lambda (rt)
    (mk-ugen (list "NumRunningSynths" rt nil #f 1 #f #f))))
(define offset-out
  (lambda (rt bus channelsArray)
    (mk-ugen (list "OffsetOut" rt (list bus) channelsArray 1 #f #f))))
(define one-pole
  (lambda (in coef)
    (mk-ugen (list "OnePole" #f (list in coef) #f 1 #f #f))))
(define one-zero
  (lambda (in coef)
    (mk-ugen (list "OneZero" #f (list in coef) #f 1 #f #f))))
(define onsets
  (lambda (rt chain threshold odftype relaxtime floor mingap medianspan whtype rawodf)
    (mk-ugen (list "Onsets" rt (list chain threshold odftype relaxtime floor mingap medianspan whtype rawodf) #f 1 #f #f))))
(define osc
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "Osc" rt (list bufnum freq phase) #f 1 #f #f))))
(define osc-n
  (lambda (rt bufnum freq phase)
    (mk-ugen (list "OscN" rt (list bufnum freq phase) #f 1 #f #f))))
(define out
  (lambda (bus channelsArray)
    (mk-ugen (list "Out" #f (list bus) channelsArray 1 #f #f))))
(define p-sin-grain
  (lambda (rt freq dur amp)
    (mk-ugen (list "PSinGrain" rt (list freq dur amp) #f 1 #f #f))))
(define pv-add
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Add" rt (list bufferA bufferB) #f 1 #f #f))))
(define pv-bin-scramble
  (lambda (rt buffer wipe width trig)
    (mk-ugen (list "PV_BinScramble" rt (list buffer wipe width trig) #f 1 #f #f))))
(define pv-bin-shift
  (lambda (rt buffer stretch shift interp)
    (mk-ugen (list "PV_BinShift" rt (list buffer stretch shift interp) #f 1 #f #f))))
(define pv-bin-wipe
  (lambda (rt bufferA bufferB wipe)
    (mk-ugen (list "PV_BinWipe" rt (list bufferA bufferB wipe) #f 1 #f #f))))
(define pv-brick-wall
  (lambda (rt buffer wipe)
    (mk-ugen (list "PV_BrickWall" rt (list buffer wipe) #f 1 #f #f))))
(define pv-chain-ugen
  (lambda (rt maxSize)
    (mk-ugen (list "PV_ChainUGen" rt (list maxSize) #f 1 #f #f))))
(define pv-conformal-map
  (lambda (rt buffer areal aimag)
    (mk-ugen (list "PV_ConformalMap" rt (list buffer areal aimag) #f 1 #f #f))))
(define pv-conj
  (lambda (rt buffer)
    (mk-ugen (list "PV_Conj" rt (list buffer) #f 1 #f #f))))
(define pv-copy
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Copy" rt (list bufferA bufferB) #f 1 #f #f))))
(define pv-copy-phase
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_CopyPhase" rt (list bufferA bufferB) #f 1 #f #f))))
(define pv-diffuser
  (lambda (rt buffer trig)
    (mk-ugen (list "PV_Diffuser" rt (list buffer trig) #f 1 #f #f))))
(define pv-div
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Div" rt (list bufferA bufferB) #f 1 #f #f))))
(define pv-hainsworth-foote
  (lambda (rt maxSize)
    (mk-ugen (list "PV_HainsworthFoote" rt (list maxSize) #f 1 #f #f))))
(define pv-jensen-andersen
  (lambda (rt maxSize)
    (mk-ugen (list "PV_JensenAndersen" rt (list maxSize) #f 1 #f #f))))
(define pv-local-max
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_LocalMax" rt (list buffer threshold) #f 1 #f #f))))
(define pv-mag-above
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_MagAbove" rt (list buffer threshold) #f 1 #f #f))))
(define pv-mag-below
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_MagBelow" rt (list buffer threshold) #f 1 #f #f))))
(define pv-mag-clip
  (lambda (rt buffer threshold)
    (mk-ugen (list "PV_MagClip" rt (list buffer threshold) #f 1 #f #f))))
(define pv-mag-div
  (lambda (rt bufferA bufferB zeroed)
    (mk-ugen (list "PV_MagDiv" rt (list bufferA bufferB zeroed) #f 1 #f #f))))
(define pv-mag-freeze
  (lambda (rt buffer freeze)
    (mk-ugen (list "PV_MagFreeze" rt (list buffer freeze) #f 1 #f #f))))
(define pv-mag-mul
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_MagMul" rt (list bufferA bufferB) #f 1 #f #f))))
(define pv-mag-noise
  (lambda (rt buffer)
    (mk-ugen (list "PV_MagNoise" rt (list buffer) #f 1 #f #f))))
(define pv-mag-shift
  (lambda (rt buffer stretch shift)
    (mk-ugen (list "PV_MagShift" rt (list buffer stretch shift) #f 1 #f #f))))
(define pv-mag-smear
  (lambda (rt buffer bins)
    (mk-ugen (list "PV_MagSmear" rt (list buffer bins) #f 1 #f #f))))
(define pv-mag-squared
  (lambda (rt buffer)
    (mk-ugen (list "PV_MagSquared" rt (list buffer) #f 1 #f #f))))
(define pv-max
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Max" rt (list bufferA bufferB) #f 1 #f #f))))
(define pv-min
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Min" rt (list bufferA bufferB) #f 1 #f #f))))
(define pv-mul
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Mul" rt (list bufferA bufferB) #f 1 #f #f))))
(define pv-phase-shift
  (lambda (rt buffer shift integrate)
    (mk-ugen (list "PV_PhaseShift" rt (list buffer shift integrate) #f 1 #f #f))))
(define pv-phase-shift270
  (lambda (rt buffer)
    (mk-ugen (list "PV_PhaseShift270" rt (list buffer) #f 1 #f #f))))
(define pv-phase-shift90
  (lambda (rt buffer)
    (mk-ugen (list "PV_PhaseShift90" rt (list buffer) #f 1 #f #f))))
(define pv-rand-comb
  (lambda (rt buffer wipe trig)
    (mk-ugen (list "PV_RandComb" rt (list buffer wipe trig) #f 1 #f #f))))
(define pv-rand-wipe
  (lambda (rt bufferA bufferB wipe trig)
    (mk-ugen (list "PV_RandWipe" rt (list bufferA bufferB wipe trig) #f 1 #f #f))))
(define pv-rect-comb
  (lambda (rt buffer numTeeth phase width)
    (mk-ugen (list "PV_RectComb" rt (list buffer numTeeth phase width) #f 1 #f #f))))
(define pv-rect-comb2
  (lambda (rt bufferA bufferB numTeeth phase width)
    (mk-ugen (list "PV_RectComb2" rt (list bufferA bufferB numTeeth phase width) #f 1 #f #f))))
(define pv-split
  (lambda (rt bufferA bufferB)
    (mk-ugen (list "PV_Split" rt (list bufferA bufferB) #f 2 #f #f))))
(define pan2
  (lambda (in pos level)
    (mk-ugen (list "Pan2" #f (list in pos level) #f 2 #f #f))))
(define pan4
  (lambda (rt in xpos ypos level)
    (mk-ugen (list "Pan4" rt (list in xpos ypos level) #f 4 #f #f))))
(define pan-az
  (lambda (nc rt in pos level width orientation)
    (mk-ugen (list "PanAz" rt (list in pos level width orientation) #f nc #f #f))))
(define pan-b
  (lambda (rt in azimuth elevation gain)
    (mk-ugen (list "PanB" rt (list in azimuth elevation gain) #f 4 #f #f))))
(define pan-b2
  (lambda (rt in azimuth gain)
    (mk-ugen (list "PanB2" rt (list in azimuth gain) #f 3 #f #f))))
(define part-conv
  (lambda (rt in fftsize irbufnum)
    (mk-ugen (list "PartConv" rt (list in fftsize irbufnum) #f 1 #f #f))))
(define pause
  (lambda (rt gate id)
    (mk-ugen (list "Pause" rt (list gate id) #f 1 #f #f))))
(define pause-self
  (lambda (rt in)
    (mk-ugen (list "PauseSelf" rt (list in) #f 1 #f #f))))
(define pause-self-when-done
  (lambda (rt src)
    (mk-ugen (list "PauseSelfWhenDone" rt (list src) #f 1 #f #f))))
(define peak
  (lambda (in trig)
    (mk-ugen (list "Peak" #f (list in trig) #f 1 #f #f))))
(define peak-follower
  (lambda (rt in decay)
    (mk-ugen (list "PeakFollower" rt (list in decay) #f 1 #f #f))))
(define phasor
  (lambda (rt trig rate start end resetPos)
    (mk-ugen (list "Phasor" rt (list trig rate start end resetPos) #f 1 #f #f))))
(define pink-noise
  (lambda (rt)
    (mk-ugen (list "PinkNoise" rt nil #f 1 #f (incr-uid 1)))))
(define pitch
  (lambda (rt in initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar)
    (mk-ugen (list "Pitch" rt (list in initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample clar) #f 2 #f #f))))
(define pitch-shift
  (lambda (in windowSize pitchRatio pitchDispersion timeDispersion)
    (mk-ugen (list "PitchShift" #f (list in windowSize pitchRatio pitchDispersion timeDispersion) #f 1 #f #f))))
(define play-buf
  (lambda (rt bufnum rate trigger startPos loop doneAction)
    (mk-ugen (list "PlayBuf" rt (list bufnum rate trigger startPos loop doneAction) #f 1 #f #f))))
(define pluck
  (lambda (in trig maxdelaytime delaytime decaytime coef)
    (mk-ugen (list "Pluck" #f (list in trig maxdelaytime delaytime decaytime coef) #f 1 #f #f))))
(define poll
  (lambda (rt trig in label trigid)
    (mk-ugen (list "Poll" rt (list trig in label trigid) #f 1 #f #f))))
(define pulse
  (lambda (rt freq width)
    (mk-ugen (list "Pulse" rt (list freq width) #f 1 #f #f))))
(define pulse-count
  (lambda (trig reset)
    (mk-ugen (list "PulseCount" #f (list trig reset) #f 1 #f #f))))
(define pulse-divider
  (lambda (trig div start)
    (mk-ugen (list "PulseDivider" #f (list trig div start) #f 1 #f #f))))
(define pure-multi-out-ugen
  (lambda (rt maxSize)
    (mk-ugen (list "PureMultiOutUGen" rt (list maxSize) #f 1 #f #f))))
(define pure-ugen
  (lambda (rt maxSize)
    (mk-ugen (list "PureUGen" rt (list maxSize) #f 1 #f #f))))
(define quad-c
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadC" rt (list freq a b c xi) #f 1 #f #f))))
(define quad-l
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadL" rt (list freq a b c xi) #f 1 #f #f))))
(define quad-n
  (lambda (rt freq a b c xi)
    (mk-ugen (list "QuadN" rt (list freq a b c xi) #f 1 #f #f))))
(define r-delay-map
  (lambda (rt bufnum in dynamic spec)
    (mk-ugen (list "RDelayMap" rt (list bufnum in dynamic spec) #f 1 #f #f))))
(define r-delay-set
  (lambda (rt in spec)
    (mk-ugen (list "RDelaySet" rt (list in spec) #f 1 #f #f))))
(define r-delay-set-b
  (lambda (rt bufnum in spec)
    (mk-ugen (list "RDelaySetB" rt (list bufnum in spec) #f 1 #f #f))))
(define r-freezer
  (lambda (rt bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops)
    (mk-ugen (list "RFreezer" rt (list bufnum left right gain increment incrementOffset incrementRandom rightRandom syncPhaseTrigger randomizePhaseTrigger numberOfLoops) #f 1 #f #f))))
(define rhpf
  (lambda (in freq rq)
    (mk-ugen (list "RHPF" #f (list in freq rq) #f 1 #f #f))))
(define rlpf
  (lambda (in freq rq)
    (mk-ugen (list "RLPF" #f (list in freq rq) #f 1 #f #f))))
(define r-loop-set
  (lambda (rt bufnum left right gain increment spec)
    (mk-ugen (list "RLoopSet" rt (list bufnum left right gain increment spec) #f 1 #f #f))))
(define r-play-trace
  (lambda (rt bufnum degree rate axis)
    (mk-ugen (list "RPlayTrace" rt (list bufnum degree rate axis) #f 1 #f #f))))
(define r-shuffler-b
  (lambda (rt bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta)
    (mk-ugen (list "RShufflerB" rt (list bufnum readLocationMinima readLocationMaxima readIncrementMinima readIncrementMaxima durationMinima durationMaxima envelopeAmplitudeMinima envelopeAmplitudeMaxima envelopeShapeMinima envelopeShapeMaxima envelopeSkewMinima envelopeSkewMaxima stereoLocationMinima stereoLocationMaxima interOffsetTimeMinima interOffsetTimeMaxima ftableReadLocationIncrement readIncrementQuanta interOffsetTimeQuanta) #f 2 #f #f))))
(define r-shuffler-l
  (lambda (rt in fragmentSize maxDelay)
    (mk-ugen (list "RShufflerL" rt (list in fragmentSize maxDelay) #f 1 #f #f))))
(define r-trace-rd
  (lambda (rt bufnum degree index axis)
    (mk-ugen (list "RTraceRd" rt (list bufnum degree index axis) #f 1 #f #f))))
(define r-trace-rd-x
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdX" rt (list bufnum degree index) #f 1 #f #f))))
(define r-trace-rd-y
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdY" rt (list bufnum degree index) #f 1 #f #f))))
(define r-trace-rd-z
  (lambda (rt bufnum degree index)
    (mk-ugen (list "RTraceRdZ" rt (list bufnum degree index) #f 1 #f #f))))
(define radians-per-sample
  (lambda (rt)
    (mk-ugen (list "RadiansPerSample" rt nil #f 1 #f #f))))
(define ramp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 52 #f))))
(define rand
  (lambda (lo hi)
    (mk-ugen (list "Rand" ir (list lo hi) #f 1 #f (incr-uid 1)))))
(define rand-id
  (lambda (rt id)
    (mk-ugen (list "RandID" rt (list id) #f 1 #f #f))))
(define rand-seed
  (lambda (rt trig seed)
    (mk-ugen (list "RandSeed" rt (list trig seed) #f 1 #f #f))))
(define record-buf
  (lambda (rt inputArray bufnum offset recLevel preLevel run loop trigger doneAction)
    (mk-ugen (list "RecordBuf" rt (list inputArray bufnum offset recLevel preLevel run loop trigger) doneAction 1 #f #f))))
(define replace-out
  (lambda (bus channelsArray)
    (mk-ugen (list "ReplaceOut" #f (list bus) channelsArray 1 #f #f))))
(define resonz
  (lambda (in freq bwr)
    (mk-ugen (list "Resonz" #f (list in freq bwr) #f 1 #f #f))))
(define ringz
  (lambda (in freq decaytime)
    (mk-ugen (list "Ringz" #f (list in freq decaytime) #f 1 #f #f))))
(define rotate2
  (lambda (rt x y pos)
    (mk-ugen (list "Rotate2" rt (list x y pos) #f 2 #f #f))))
(define running-max
  (lambda (in trig)
    (mk-ugen (list "RunningMax" #f (list in trig) #f 1 #f #f))))
(define running-min
  (lambda (in trig)
    (mk-ugen (list "RunningMin" #f (list in trig) #f 1 #f #f))))
(define running-sum
  (lambda (in numsamp)
    (mk-ugen (list "RunningSum" #f (list in numsamp) #f 1 #f #f))))
(define sos
  (lambda (in a0 a1 a2 b1 b2)
    (mk-ugen (list "SOS" #f (list in a0 a1 a2 b1 b2) #f 1 #f #f))))
(define sample-dur
  (lambda (rt)
    (mk-ugen (list "SampleDur" rt nil #f 1 #f #f))))
(define sample-rate
  (lambda (rt)
    (mk-ugen (list "SampleRate" rt nil #f 1 #f #f))))
(define saw
  (lambda (rt freq)
    (mk-ugen (list "Saw" rt (list freq) #f 1 #f #f))))
(define schmidt
  (lambda (rt in lo hi)
    (mk-ugen (list "Schmidt" rt (list in lo hi) #f 1 #f #f))))
(define scope-out
  (lambda (rt inputArray bufnum)
    (mk-ugen (list "ScopeOut" rt (list inputArray bufnum) #f 1 #f #f))))
(define scope-out2
  (lambda (rt inputArray scopeNum maxFrames scopeFrames)
    (mk-ugen (list "ScopeOut2" rt (list inputArray scopeNum maxFrames scopeFrames) #f 1 #f #f))))
(define select
  (lambda (which array)
    (mk-ugen (list "Select" #f (list which) array 1 #f #f))))
(define send-trig
  (lambda (in id value)
    (mk-ugen (list "SendTrig" #f (list in id value) #f 1 #f #f))))
(define set-buf
  (lambda (rt buf numValues offset values)
    (mk-ugen (list "SetBuf" rt (list buf numValues offset) values 1 #f #f))))
(define set-reset-ff
  (lambda (trig reset)
    (mk-ugen (list "SetResetFF" #f (list trig reset) #f 1 #f #f))))
(define shaper
  (lambda (bufnum in)
    (mk-ugen (list "Shaper" #f (list bufnum in) #f 1 #f #f))))
(define sin-osc
  (lambda (rt freq phase)
    (mk-ugen (list "SinOsc" rt (list freq phase) #f 1 #f #f))))
(define sin-osc-fb
  (lambda (rt freq feedback)
    (mk-ugen (list "SinOscFB" rt (list freq feedback) #f 1 #f #f))))
(define slew
  (lambda (in up dn)
    (mk-ugen (list "Slew" #f (list in up dn) #f 1 #f #f))))
(define slope
  (lambda (rt in)
    (mk-ugen (list "Slope" rt (list in) #f 1 #f #f))))
(define spec-centroid
  (lambda (rt buffer)
    (mk-ugen (list "SpecCentroid" rt (list buffer) #f 1 #f #f))))
(define spec-flatness
  (lambda (rt buffer)
    (mk-ugen (list "SpecFlatness" rt (list buffer) #f 1 #f #f))))
(define spec-pcile
  (lambda (rt buffer fraction interpolate)
    (mk-ugen (list "SpecPcile" rt (list buffer fraction interpolate) #f 1 #f #f))))
(define splay
  (lambda (rt inArray spread level center levelComp)
    (mk-ugen (list "Splay" rt (list inArray spread level center levelComp) #f 2 #f #f))))
(define splay-az
  (lambda (rt inArray spread level width center orientation levelComp)
    (mk-ugen (list "SplayAz" rt (list inArray spread level width center orientation levelComp) #f 1 #f #f))))
(define spring
  (lambda (rt in spring damp)
    (mk-ugen (list "Spring" rt (list in spring damp) #f 1 #f #f))))
(define standard-l
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardL" rt (list freq k xi yi) #f 1 #f #f))))
(define standard-n
  (lambda (rt freq k xi yi)
    (mk-ugen (list "StandardN" rt (list freq k xi yi) #f 1 #f #f))))
(define stepper
  (lambda (trig reset min max step resetval)
    (mk-ugen (list "Stepper" #f (list trig reset min max step resetval) #f 1 #f #f))))
(define stereo-convolution2l
  (lambda (rt in kernelL kernelR trigger framesize crossfade)
    (mk-ugen (list "StereoConvolution2L" rt (list in kernelL kernelR trigger framesize crossfade) #f 2 #f #f))))
(define subsample-offset
  (lambda (rt)
    (mk-ugen (list "SubsampleOffset" rt nil #f 1 #f #f))))
(define sum3
  (lambda (in0 in1 in2)
    (mk-ugen (list "Sum3" #f (list in0 in1 in2) #f 1 #f #f))))
(define sum4
  (lambda (in0 in1 in2 in3)
    (mk-ugen (list "Sum4" #f (list in0 in1 in2 in3) #f 1 #f #f))))
(define sweep
  (lambda (trig rate)
    (mk-ugen (list "Sweep" #f (list trig rate) #f 1 #f #f))))
(define sync-saw
  (lambda (rt syncFreq sawFreq)
    (mk-ugen (list "SyncSaw" rt (list syncFreq sawFreq) #f 1 #f #f))))
(define t2a
  (lambda (rt in offset)
    (mk-ugen (list "T2A" rt (list in offset) #f 1 #f #f))))
(define t2k
  (lambda (rt in)
    (mk-ugen (list "T2K" rt (list in) #f 1 #f #f))))
(define t-ball
  (lambda (rt in g damp friction)
    (mk-ugen (list "TBall" rt (list in g damp friction) #f 1 #f #f))))
(define t-delay
  (lambda (in dur)
    (mk-ugen (list "TDelay" #f (list in dur) #f 1 #f #f))))
(define t-duty
  (lambda (rt dur reset level doneAction gapFirst)
    (mk-ugen (list "TDuty" rt (list dur reset level doneAction gapFirst) #f 1 #f #f))))
(define t-exp-rand
  (lambda (lo hi trig)
    (mk-ugen (list "TExpRand" #f (list lo hi trig) #f 1 #f (incr-uid 1)))))
(define tgrains
  (lambda (nc rt trigger bufnum rate centerPos dur pan amp interp)
    (mk-ugen (list "TGrains" rt (list trigger bufnum rate centerPos dur pan amp interp) #f nc #f #f))))
(define ti-rand
  (lambda (lo hi trig)
    (mk-ugen (list "TIRand" #f (list lo hi trig) #f 1 #f (incr-uid 1)))))
(define t-rand
  (lambda (lo hi trig)
    (mk-ugen (list "TRand" #f (list lo hi trig) #f 1 #f (incr-uid 1)))))
(define t-windex
  (lambda (rt in array normalize)
    (mk-ugen (list "TWindex" rt (list in array) normalize 1 #f (incr-uid 1)))))
(define tap
  (lambda (nc rt bufnum delaytime)
    (mk-ugen (list "Tap" rt (list bufnum delaytime) #f nc #f #f))))
(define timer
  (lambda (trig)
    (mk-ugen (list "Timer" #f (list trig) #f 1 #f #f))))
(define toggle-ff
  (lambda (trig)
    (mk-ugen (list "ToggleFF" #f (list trig) #f 1 #f #f))))
(define trig
  (lambda (in dur)
    (mk-ugen (list "Trig" #f (list in dur) #f 1 #f #f))))
(define trig1
  (lambda (in dur)
    (mk-ugen (list "Trig1" #f (list in dur) #f 1 #f #f))))
(define trig-control
  (lambda (rt values)
    (mk-ugen (list "TrigControl" rt (list values) #f 1 #f #f))))
(define two-pole
  (lambda (in freq radius)
    (mk-ugen (list "TwoPole" #f (list in freq radius) #f 1 #f #f))))
(define two-zero
  (lambda (in freq radius)
    (mk-ugen (list "TwoZero" #f (list in freq radius) #f 1 #f #f))))
(define unary-op-ugen
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 #f #f))))
(define v-disk-in
  (lambda (nc rt bufnum rate loop sendID)
    (mk-ugen (list "VDiskIn" rt (list bufnum rate loop sendID) #f nc #f #f))))
(define v-osc
  (lambda (rt bufpos freq phase)
    (mk-ugen (list "VOsc" rt (list bufpos freq phase) #f 1 #f #f))))
(define v-osc3
  (lambda (rt bufpos freq1 freq2 freq3)
    (mk-ugen (list "VOsc3" rt (list bufpos freq1 freq2 freq3) #f 1 #f #f))))
(define var-lag
  (lambda (rt in time curvature warp start)
    (mk-ugen (list "VarLag" rt (list in time curvature warp start) #f 1 #f #f))))
(define var-saw
  (lambda (rt freq iphase width)
    (mk-ugen (list "VarSaw" rt (list freq iphase width) #f 1 #f #f))))
(define vibrato
  (lambda (rt freq rate depth delay onset rateVariation depthVariation iphase)
    (mk-ugen (list "Vibrato" rt (list freq rate depth delay onset rateVariation depthVariation iphase) #f 1 #f (incr-uid 1)))))
(define warp1
  (lambda (nc rt bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp)
    (mk-ugen (list "Warp1" rt (list bufnum pointer freqScale windowSize envbufnum overlaps windowRandRatio interp) #f nc #f #f))))
(define white-noise
  (lambda (rt)
    (mk-ugen (list "WhiteNoise" rt nil #f 1 #f (incr-uid 1)))))
(define width-first-ugen
  (lambda (rt maxSize)
    (mk-ugen (list "WidthFirstUGen" rt (list maxSize) #f 1 #f #f))))
(define wrap
  (lambda (in lo hi)
    (mk-ugen (list "Wrap" #f (list in lo hi) #f 1 #f #f))))
(define wrap-index
  (lambda (bufnum in)
    (mk-ugen (list "WrapIndex" #f (list bufnum in) #f 1 #f #f))))
(define x-fade2
  (lambda (rt inA inB pan level)
    (mk-ugen (list "XFade2" rt (list inA inB pan level) #f 1 #f #f))))
(define x-line
  (lambda (rt start end dur doneAction)
    (mk-ugen (list "XLine" rt (list start end dur doneAction) #f 1 #f #f))))
(define x-out
  (lambda (bus xfade channelsArray)
    (mk-ugen (list "XOut" #f (list bus xfade) channelsArray 1 #f #f))))
(define zero-crossing
  (lambda (rt in)
    (mk-ugen (list "ZeroCrossing" rt (list in) #f 1 #f #f))))
(define neg
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 0 #f))))
(define not
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 1 #f))))
(define is-nil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 2 #f))))
(define not-nil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 3 #f))))
(define bit-not
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 4 #f))))
(define abs
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 5 #f))))
(define as-float
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 6 #f))))
(define as-int
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 7 #f))))
(define ceil
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 8 #f))))
(define floor
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 9 #f))))
(define frac
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 10 #f))))
(define sign
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 11 #f))))
(define squared
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 12 #f))))
(define cubed
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 13 #f))))
(define sqrt
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 14 #f))))
(define exp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 15 #f))))
(define recip
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 16 #f))))
(define midicps
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 17 #f))))
(define cpsmidi
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 18 #f))))
(define midi-ratio
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 19 #f))))
(define ratio-midi
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 20 #f))))
(define db-amp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 21 #f))))
(define amp-db
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 22 #f))))
(define oct-cps
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 23 #f))))
(define cps-oct
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 24 #f))))
(define log
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 25 #f))))
(define log2
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 26 #f))))
(define log10
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 27 #f))))
(define sin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 28 #f))))
(define cos
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 29 #f))))
(define tan
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 30 #f))))
(define arc-sin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 31 #f))))
(define arc-cos
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 32 #f))))
(define arc-tan
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 33 #f))))
(define sin-h
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 34 #f))))
(define cos-h
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 35 #f))))
(define tan-h
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 36 #f))))
(define rand-
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 37 #f))))
(define rand2
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 38 #f))))
(define lin-rand-
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 39 #f))))
(define bi-lin-rand
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 40 #f))))
(define sum3rand
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 41 #f))))
(define distort
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 42 #f))))
(define soft-clip
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 43 #f))))
(define coin
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 44 #f))))
(define digit-value
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 45 #f))))
(define silence
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 46 #f))))
(define thru
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 47 #f))))
(define rect-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 48 #f))))
(define han-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 49 #f))))
(define welch-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 50 #f))))
(define tri-window
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 51 #f))))
(define ramp
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 52 #f))))
(define s-curve
  (lambda (a)
    (mk-ugen (list "UnaryOpUGen" #f (list a) #f 1 53 #f))))
(define add
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 0 #f))))
(define sub
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 1 #f))))
(define mul
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 2 #f))))
(define i-div
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 3 #f))))
(define f-div
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 4 #f))))
(define mod
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 5 #f))))
(define eq-
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 6 #f))))
(define ne
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 7 #f))))
(define lt-
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 8 #f))))
(define gt-
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 9 #f))))
(define le
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 10 #f))))
(define ge
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 11 #f))))
(define min
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 12 #f))))
(define max
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 13 #f))))
(define bit-and
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 14 #f))))
(define bit-or
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 15 #f))))
(define bit-xor
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 16 #f))))
(define lcm
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 17 #f))))
(define gcd
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 18 #f))))
(define round
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 19 #f))))
(define round-up
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 20 #f))))
(define trunc
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 21 #f))))
(define atan2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 22 #f))))
(define hypot
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 23 #f))))
(define hypotx
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 24 #f))))
(define pow
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 25 #f))))
(define shift-left
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 26 #f))))
(define shift-right
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 27 #f))))
(define unsigned-shift
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 28 #f))))
(define fill
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 29 #f))))
(define ring1
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 30 #f))))
(define ring2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 31 #f))))
(define ring3
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 32 #f))))
(define ring4
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 33 #f))))
(define dif-sqr
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 34 #f))))
(define sum-sqr
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 35 #f))))
(define sqr-sum
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 36 #f))))
(define sqr-dif
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 37 #f))))
(define abs-dif
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 38 #f))))
(define thresh
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 39 #f))))
(define am-clip
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 40 #f))))
(define scale-neg
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 41 #f))))
(define clip2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 42 #f))))
(define excess
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 43 #f))))
(define fold2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 44 #f))))
(define wrap2
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 45 #f))))
(define first-arg
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 46 #f))))
(define rand-range
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 47 #f))))
(define exp-rand-range
  (lambda (a b)
    (mk-ugen (list "BinaryOpUGen" #f (list a b) #f 1 48 #f))))
